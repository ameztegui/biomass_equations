library(tidyverse)
library(readxl)
library(glue)

# Read data from AllomApp -------------------------------------------------

allom <- read_excel("./data/allometries_table_2020-07-16.xlsx")  
new_montero <- read_excel("./data/BAT.xlsx", sheet = "new_values")

all_allom <- bind_rows(allom, new_montero)  %>%
    distinct(functional_group_level_name, spatial_level_name, 
             source, dependent_var, equation, cubication_shape, special_param, .keep_all = TRUE) %>%
    mutate(Var_Type = case_when(grepl("iomasa", dependent_var_translation_spa)  ~ "Biomasa",
                                grepl("olum", dependent_var_translation_spa) ~ "Volumen",
                                TRUE ~ "Alometria"),
           species = functional_group_level_name) %>%
    mutate(Var_Type = factor(Var_Type, levels = c("Biomasa", "Volumen", "Alometria"))) 


# table(all_allom$source, all_allom$dependent_var)

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
        species == "Fraxinus excelsior" & dependent_var == "Ht" ~ "Fraxinus spp.",
        allometry_id %in% c("BAT_111") ~ "Fraxinus spp.",        # los fraxinus en iefc no estan a nivel de especie
        species == "Platanus hispanica" ~ "Platanus x hispanica",
        species == "Quercus canariensis x humilis"  & dependent_var == "BAT" ~ "Quercus canariensis",
        species == "Populus hybrides" ~ "Populus x canadensis",
        species == "Eucalyptus gomphocephallus" ~ "Eucalyptus spp.",
        species == "Eucaplytpus spp." ~ "Eucalyptus spp.",
        TRUE ~ species
    ))

iefc_names <- filter(all_allom, dependent_var == "BAT", source == "IEFC") %>% 
    select(species) %>% unique() %>%
    arrange(species)

inia_names <- filter(all_allom, dependent_var == "BAT", source == "INIA") %>% 
    select(species) %>% unique() %>%
    arrange(species)

sps_list <- inner_join(iefc_names, inia_names) %>% pull()



# Generate file -----------------------------------------------------------------------------------------

data_bat <- all_allom %>% 
    filter(species %in% sps_list,
           source %in% c("IEFC", "INIA"),
           independent_var_1 == "DBH",
           Var_Type %in% c("Biomasa") | dependent_var == "Ht",
           spatial_level != "county",
           spatial_level != "province") %>%
    mutate(id = case_when(equation == "BAT = a·DBH^b·Ht^c" ~ 2,
                          equation == "BH = a·(DBH·10)^b" ~ 2,
                          TRUE ~ 1),
           eq_id = paste0(dependent_var,"_",source,"_", id)) %>%
    group_by(eq_id, species) %>%
    arrange(allometry_id) %>%
    slice(1)



## Algunas comprobaciones

# table(data_bat$eq_id, data_bat$dependent_var)
# table(data_bat$equation, data_bat$dependent_var)
# table(data_bat$equation, data_bat$eq_id)
# 
# table(data_bat$equation, data_bat$eq_id)
# table(data_bat$eq_id, data_bat$source)
# table(data_bat$dependent_var, data_bat$source)

