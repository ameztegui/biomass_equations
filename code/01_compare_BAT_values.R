# source("./code/00_wrangle_data.R")

library(viridis)

# Determine biomass for each species ------------------------------------------------------------------------------

data_wide <- data_bat %>%
pivot_wider(c(func_group,species, eq_id,param_a: param_d),
            names_from = eq_id,
            values_from = param_a:param_d,
            names_glue = "{eq_id}_{.value}") 


 bat_output <- data_wide %>%
    rowwise() %>%
    mutate(dbh = list(seq(10,75,1))) %>%
    unnest(dbh) %>%
    mutate(Ht_IEFC_1 = Ht_IEFC_1_param_a * dbh ^ Ht_IEFC_1_param_b,
           BAT_IEFC_1 = BAT_IEFC_1_param_a * dbh ^ BAT_IEFC_1_param_b,
           BAT_IEFC_2 = BAT_IEFC_2_param_a * dbh ^ BAT_IEFC_2_param_b * Ht_IEFC_1 ^ BAT_IEFC_2_param_c,
           BAT_INIA_1 = BAT_INIA_1_param_a * dbh ^ BAT_INIA_1_param_b,
           BF_INIA_1 = BF_INIA_1_param_a * dbh ^ BF_INIA_1_param_b,
           BR2_INIA_1 = BR2_INIA_1_param_a * dbh ^ BR2_INIA_1_param_b,
           BR2_7_INIA_1 = `BR2-7_INIA_1_param_a`* dbh ^ `BR2-7_INIA_1_param_b`,
           BR7_INIA_1 = BR7_INIA_1_param_a * dbh ^ BR7_INIA_1_param_b,
           BH_INIA_1 = BH_INIA_1_param_a * dbh ^BH_INIA_1_param_b,
           BST_INIA_1 = BST_INIA_1_param_a * dbh ^ BST_INIA_1_param_b,
           BM_IEFC_1 = BM_IEFC_1_param_a * dbh ^ BM_IEFC_1_param_b,
           BC_IEFC_1 = BC_IEFC_1_param_a * dbh ^ BC_IEFC_1_param_b,
           BR_IEFC_1 = BR_IEFC_1_param_a * (dbh*10) ^ BR_IEFC_1_param_b,
           BH_IEFC_1 = BH_IEFC_1_param_a * dbh ^ BH_IEFC_1_param_b,
           BH_IEFC_2 = BH_IEFC_2_param_a * (dbh*10) ^ BH_IEFC_2_param_b,
           BRH_IEFC_1 = BRH_IEFC_1_param_a * dbh ^ BRH_IEFC_1_param_b) %>%
    rowwise() %>%
    mutate(BAT_INIA_partes = sum(BF_INIA_1, BR2_INIA_1, BR2_7_INIA_1, BR7_INIA_1, BH_INIA_1,na.rm = T),
           BAT_IEFC_partes = sum(BM_IEFC_1, BC_IEFC_1, BH_IEFC_1, BH_IEFC_2, BRH_IEFC_1, na.rm = T)) %>%
     mutate(r_INIA_IEFC1 = BAT_INIA_1/BAT_IEFC_1,
            r_INIA_IEFC2 = BAT_INIA_1/BAT_IEFC_2,
            r_IEFC_1_2 = BAT_IEFC_1/BAT_IEFC_2) %>%
     mutate(CD = cut_width(dbh, width = 10, center = 10)) 
     


 # Create plots ----------------------------------------------------------------------------------------------------

 
 output_long <- bat_output %>%
     select(func_group, species, dbh, Ht_IEFC_1:BAT_IEFC_partes) %>%
     pivot_longer(cols = !c(func_group, species, dbh),
                  names_to = "Equation",
                  names_prefix = "BAT_",
                  values_to = "BAT")
 
 
new_theme <- theme_bw() +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "italic"),
          legend.position = "bottom")

output_long %>%
    filter(Equation == "Ht_IEFC_1") %>%
    ggplot() +
    geom_line(aes(dbh, BAT, color = Equation)) +
    facet_wrap(~ species, scale = "free") +
    new_theme


# Partes de INIA
output_long %>%
    filter(Equation %in% c("INIA_partes", "INIA_1")) %>%
    ggplot() +
    geom_line(aes(dbh, BAT, color = Equation)) +
    scale_color_viridis_d() +
    facet_wrap(~ species, scale = "free") +
    new_theme

# Partes de IEFC
output_long %>%
    filter(Equation %in% c("IEFC_partes", "IEFC_1", "IEFC_2")) %>%
    ggplot() +
    geom_line(aes(dbh, BAT, color = Equation)) +
    scale_color_viridis_d() +
    facet_wrap(~ species, scale = "free") +
    new_theme

# INIA vs. IEFC

output_long %>%
    filter(Equation %in% c("INIA_1", "IEFC_1", "IEFC_2")) %>%
    ggplot() +
    geom_line(aes(dbh, BAT, color = Equation)) +
    facet_wrap(~ species, scale = "free") +
    new_theme


# Partes de INIA
output_long %>%
    filter(Equation %in% c("BAT_INIA_1", "BAT_IEFC_1", "BAT_IEFC_2")) %>%
    ggplot() +
    geom_line(aes(dbh, BAT, color = Equation)) +
    facet_wrap(~ species, scale = "free")

