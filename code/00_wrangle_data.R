library(Hmisc)   # function %nin%
library(tidyverse)
library(readxl)
library(glue)


# Read data from AllomApp -------------------------------------------------

allom <- read_excel("./data/allometries_table_2020-07-16.xlsx")  
montero <- read_excel("./data/BAT.xlsx", sheet = "new_values")
ruiz <- read_excel("./data/BAT.xlsx", sheet = "ruiz-peinado")
coefs <- read_excel("./data/coefs.xlsx", sheet = "conversion")


all_allom <- bind_rows(allom, montero, ruiz)  %>%
    bind_rows(ruiz) %>%
    distinct(functional_group_level_name, spatial_level_name, 
             source, dependent_var, equation, cubication_shape, special_param, .keep_all = TRUE) %>%
    mutate(Var_Type = case_when(grepl("iomasa", dependent_var_translation_spa)  ~ "Biomasa",
                                grepl("olum", dependent_var_translation_spa) ~ "Volumen",
                                TRUE ~ "Alometria")) %>%
    rename(species = functional_group_level_name) %>%
    mutate(Var_Type = factor(Var_Type, levels = c("Biomasa", "Volumen", "Alometria"))) 


table(all_allom$source, all_allom$dependent_var)

# Change names to match all sources ---------------------------------------

all_allom <- all_allom %>%
    mutate(species = case_when(
        species == "Altres caducifolis" ~ "Otras frondosas",
        species == "Altres planifolis" ~ "Otras frondosas",
        species == "Altres coníferes" ~ "Otras coniferas",
        species == "Altres pins" ~ "Pinus spp.",
        species == "Altres roures" ~ "Quercus spp.",
        species == "Betula" ~ "Betula spp.",
        species == "Betula pendula" & dependent_var == "Ht" ~ "Betula spp.",
        species == "Fraxinus excelsior" & source == "IEFC" ~ "Fraxinus spp.",         # los fraxinus en iefc no estan a nivel de especie
        species == "Fraxinus excelsior" & dependent_var == "Ht" ~ "Fraxinus spp.",
        species == "Eucalyptus globulus" ~ "Eucalyptus spp.",
        species == "Platanus hispanica" ~ "Platanus x hispanica",
        species == "Quercus canariensis x humilis"  & dependent_var %in% c("BAT", "VOB") ~ "Quercus canariensis",
        species == "Populus hybrides" ~ "Populus x canadensis",
        species == "Populus nigra" & dependent_var == "VOB" & source == "IEFC" ~ "Populus x canadensis",
        species == "Eucalyptus gomphocephallus" ~ "Eucalyptus spp.",
        TRUE ~ species
    ))

iefc_names <- filter(all_allom, source == "IEFC", dependent_var == "BAT") %>% 
    select(species) %>% unique() %>%
    arrange(species)

inia_names <- filter(all_allom,  source == "INIA") %>% 
    select(species) %>% unique() %>%
    arrange(species)


ruiz_names <- filter(all_allom, source == "RUIZ") %>% 
    select(species) %>% unique() %>%
    arrange(species)

sps_list <- inner_join(ruiz_names, inia_names) %>% 
    inner_join(iefc_names) %>% pull()

save(all_allom, file = "./data/all_allom.Rdata")
save(sps_list, file = "./data/sps_list.Rdata")

# Generate file -----------------------------------------------------------------------------------------

data_bat <- all_allom %>% 
    filter(#species %in% sps_list,
           source %in% c("IEFC", "INIA", "RUIZ"),
           independent_var_1 == "DBH",
           Var_Type %in% c("Biomasa") | dependent_var %in% c("Ht","VOB"),
           spatial_level != "county",
           spatial_level != "province",
           !is.na(param_a),
           equation != "BH = a·(DBH·10)^b") %>%
    mutate(dep_var_group = case_when(dependent_var %in% c("BF", "BF+BR7", "BF_R7", "BM") ~ "BF",
                                     dependent_var %in% c("BR7", "BR7+2_7") ~ "BR7",
                                     dependent_var %in% c("BR2_7", "BR2_7+2+H") ~ "BR2_7",
                                     dependent_var %in% c("BR2", "BR2H", "BR2+H") ~ "BR2",
                                     TRUE ~ dependent_var)) %>%
    distinct(across(c(species, dependent_var, source, equation)), .keep_all = T) %>%
    group_by(species, dep_var_group, source) %>%
    dplyr::mutate(id = 1:n(),
           id = case_when(equation == "BAT = a·DBH^b·Ht^c" ~ 2,
                          equation == "BH = a·(DBH·10)^b" ~ 2,
                          TRUE ~ as.numeric(id)),
           eq_id = paste0(dep_var_group,"_",source,"_", id))

## Algunas comprobaciones

table(data_bat$species, data_bat$source)
table(data_bat$equation, data_bat$eq_id)
# table(data_bat$eq_id, data_bat$source)
# table(data_bat$dependent_var, data_bat$source)
table(data_bat$eq_id, data_bat$dependent_var_units)



# Some descriptive stats --------------------------------------------------

data_bat %>% 
    filter(dep_var_group == "BAT") %>% 
    group_by(eq_id) %>%
    summarise(across(c(n_obs, r_sqr), 
                     .fns= list(min = min, max = max, mean = mean), na.rm = T))


save(data_bat, file = "./data/data_allom.Rdata")







# Determine biomass for each species ------------------------------------------------------------------------------

data_wide <- data_bat %>%
    pivot_wider(c(func_group,species, eq_id,param_a: param_d),
                names_from = eq_id,
                values_from =  param_a:param_d,
                names_glue = "{eq_id}_{.value}") %>%
    left_join(coefs)

save(data_wide, file = "./data/allometry_functions.Rdata")




dmin_dmax <- coefs %>%
    filter(species %in% sps_list)
#     group_by(func_group,species, source) %>%
#     slice(1) %>%
#     pivot_wider(c(func_group,species, source, dmin:dmax),
#                 names_from = source,
#                 values_from =  dmin:dmax,
#                 names_glue = "{source}_{.value}")

bat_output <- data_wide %>%
    left_join(dmin_dmax) %>%
    rowwise() %>%
    mutate(dbh = list(seq(5,85,0.5))) %>%
    unnest(dbh) %>%
    mutate(Ht_IEFC_1 = Ht_IEFC_1_param_a * dbh ^ Ht_IEFC_1_param_b,
           Ht = Ht_IEFC_1,
           VOB = VOB_IEFC_1_param_a * dbh ^ VOB_IEFC_1_param_b,
           BAT_VOL = VOB*BEFD,
           BAT_IEFC_1 =   BAT_IEFC_1_param_a * dbh ^ BAT_IEFC_1_param_b,
           BAT_IEFC_2 =   BAT_IEFC_2_param_a * dbh ^ BAT_IEFC_2_param_b * Ht_IEFC_1 ^ BAT_IEFC_2_param_c,
           BAT_INIA_1 =   BAT_INIA_1_param_a * dbh ^ BAT_INIA_1_param_b, 
           BF_IEFC_1 = BF_IEFC_1_param_a * dbh ^ BF_IEFC_1_param_b,
           BF_INIA_1 =    BF_INIA_1_param_a * dbh ^ BF_INIA_1_param_b,
           BF_RUIZ_1 =    case_when(species == "Fagus sylvatica" ~
                                        BF_RUIZ_1_param_a * dbh^2 + BF_RUIZ_1_param_b * dbh^2 * Ht,
                                    TRUE ~ (BF_RUIZ_1_param_a * dbh ^ BF_RUIZ_1_param_b) * (Ht ^ BF_RUIZ_1_param_c) + BF_RUIZ_1_param_d * Ht),
           BR_IEFC_1 =    (BR_IEFC_1_param_a * (dbh*10) ^ BR_IEFC_1_param_b) /1000,
           BRH_IEFC_1 =   (BRH_IEFC_1_param_a * dbh ^ BRH_IEFC_1_param_b),
           BR7_INIA_1 =   BR7_INIA_1_param_a * dbh ^ BR7_INIA_1_param_b,
           BR7_RUIZ_1 =   case_when(species %in% c("Alnus glutinosa", "Quercus canariensis", "Quercus faginea", "Quercus suber", "Abies alba", "Pinus pinaster","Pinus uncinata") ~
                                        BR7_RUIZ_1_param_a*dbh ^ BR7_RUIZ_1_param_b * Ht ^ BR7_RUIZ_1_param_c,
                                    species %nin% c("Alnus glutinosa", "Quercus canariensis", "Quercus faginea", "Quercus suber", "Abies alba", "Pinus pinaster","Pinus uncinata") & dbh <= BR7_RUIZ_1_param_b ~
                                        0,
                                    species %nin% c("Alnus glutinosa", "Quercus canariensis", "Quercus faginea", "Quercus suber", "Abies alba", "Pinus pinaster","Pinus uncinata") & dbh > BR7_RUIZ_1_param_b ~
                                        BR7_RUIZ_1_param_a * (dbh - BR7_RUIZ_1_param_b)^BR7_RUIZ_1_param_c + BR7_RUIZ_1_param_d*(dbh - BR7_RUIZ_1_param_b)^2*Ht),
           
           
           
           BR2_INIA_1 =   BR2_INIA_1_param_a * dbh ^ BR2_INIA_1_param_b,
           BR2_7_INIA_1 = BR2_7_INIA_1_param_a* dbh ^ BR2_7_INIA_1_param_b,
           BH_INIA_1 =    BH_INIA_1_param_a * dbh ^BH_INIA_1_param_b,
           BST_INIA_1 =   BST_INIA_1_param_a * dbh ^ BST_INIA_1_param_b,
           BR2_7_RUIZ_1 = case_when(species == "Pinus halepensis" ~
                                        BR2_7_RUIZ_1_param_a + BR2_7_RUIZ_1_param_b * dbh^2*Ht + BR2_7_RUIZ_1_param_c*dbh*Ht,
                                    species %in% c("Quercus faginea", "Quercus pyrenaica") ~
                                        BR2_7_RUIZ_1_param_a * dbh^BR2_7_RUIZ_1_param_b + BR2_7_RUIZ_1_param_c * Ht + BR2_7_RUIZ_1_param_d * dbh^2 * Ht,
                                    TRUE ~ BR2_7_RUIZ_1_param_a * dbh ^ BR2_7_RUIZ_1_param_b * Ht ^ BR2_7_RUIZ_1_param_c),
           BR2_RUIZ_1 = case_when(species %in% c("Pinus pinea", "Pinus uncinata") ~
                                      BR2_RUIZ_1_param_a + BR2_RUIZ_1_param_b * dbh ^ BR2_RUIZ_1_param_c - BR2_RUIZ_1_param_d * Ht,
                                  species == "Pinus halepensis" ~
                                      BR2_RUIZ_1_param_a + BR2_RUIZ_1_param_b * dbh^2 * Ht + BR2_RUIZ_1_param_c * dbh * Ht,
                                  species == "Eucalyptus globulus" ~
                                      BR2_RUIZ_1_param_a *dbh^BR2_RUIZ_1_param_b + BR2_RUIZ_1_param_c * dbh^BR2_RUIZ_1_param_d *Ht,
                                  species %in% c("Pinus nigra", "Pinus pinaster", "Alnus glutinosa", "Castanea sativa", "Juniperus thurifera","Pinus sylvestris",
                                                 "Fraxinus angustifolia") ~
                                      BR2_RUIZ_1_param_a * dbh ^ BR2_RUIZ_1_param_b * BR2_RUIZ_1_param_c * Ht ^BR2_RUIZ_1_param_d,
                                  TRUE ~
                                      BR2_RUIZ_1_param_a * dbh ^ BR2_RUIZ_1_param_b + BR2_RUIZ_1_param_c * dbh ^BR2_RUIZ_1_param_d * Ht),
           BC_IEFC_1 = BC_IEFC_1_param_a * dbh ^ BC_IEFC_1_param_b,
           BH_IEFC_1 = (BH_IEFC_1_param_a * dbh ^ BH_IEFC_1_param_b)/1000) %>%
           # BH_IEFC_2 = BH_IEFC_2_param_a * (dbh*10) ^ BH_IEFC_2_param_b,
    rowwise() %>%
    mutate(BAT_IEFC_partes = sum(BF_IEFC_1, BR_IEFC_1, BC_IEFC_1, BH_IEFC_1,  BRH_IEFC_1, na.rm = T),
           BAT_INIA_partes = sum(BF_INIA_1, BR7_INIA_1, BR2_7_INIA_1, BR2_INIA_1, BH_INIA_1,na.rm = T),
           BAT_RUIZ_1 = sum(BF_RUIZ_1, BR7_RUIZ_1, BR2_7_RUIZ_1, BR2_RUIZ_1, na.rm = T)) 

# Calcular ratios -------------------------------------------------------------------------------------------------

bat_output <- bat_output %>%
    mutate(func_group = fct_recode(func_group, "Softwood" = "C", "Hardwood" = "F")) %>%
    mutate(r_INIA_IEFC1 = (BAT_INIA_1 - BAT_IEFC_1) / (BAT_INIA_1 + BAT_IEFC_1),
           r_INIA_IEFC2 = (BAT_INIA_1 - BAT_IEFC_2) / (BAT_INIA_1 + BAT_IEFC_2),
           r_INIA_RUIZ = (BAT_INIA_1 - BAT_RUIZ_1) / (BAT_INIA_1 + BAT_RUIZ_1),
           r_RUIZ_IEFC1 = (BAT_RUIZ_1 - BAT_IEFC_1) / (BAT_RUIZ_1 + BAT_IEFC_1),
           r_RUIZ_IEFC2 =(BAT_RUIZ_1 - BAT_IEFC_2) / (BAT_RUIZ_1 + BAT_IEFC_2),
           r_IEFC2_IEFC1 = (BAT_IEFC_2 - BAT_IEFC_1) / (BAT_IEFC_2 + BAT_IEFC_1),
           r_BIO_VOL = (BAT_VOL - BAT_IEFC_1)/(BAT_VOL + BAT_IEFC_1)) %>%
    mutate(CD = cut(dbh,breaks = seq(5,85, 10), 
                    include.lowest = T,
                    labels = c("CD10", "CD20", "CD30", "CD40", "CD50","CD60", "CD70", "CD80")))


output_long <- bat_output %>%
    select(func_group, species, CD, dbh, Ht_IEFC_1, Ht,  
           BAT_VOL, BAT_IEFC_1, BAT_IEFC_2, BAT_INIA_1, BAT_RUIZ_1, 
           starts_with("r_"), contains("dm")) %>%
    pivot_longer(cols = !c(func_group, species, dbh, CD),
                 names_to = "Equation",
                 names_prefix = "BAT_",
                 values_to = "value") 

save(data_bat, bat_output, output_long, file = "./data/BAT_output.Rdata")


