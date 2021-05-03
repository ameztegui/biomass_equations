
new_theme <- theme_bw() +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "italic"),
          legend.position = "bottom")

eq_colors <- c("#5F978E","#5B4887", "#9B2F5D", "#E7BC66", "#D55E00")

load("./data/BAT_output.Rdata")

# Comparar resultados ------------------------------------------------------------------------------------

# Biomasa aerea total
total <- data_bat %>% filter(dep_var_group == "BAT")
table(total$species, total$source)

bat_output %>%
    select(species, dbh, Ht,VOB, starts_with("BAT")) %>%
    ggplot() +
    geom_line(aes(dbh, BAT_IEFC_1), color = eq_colors[1], size = 1.5) +
    geom_line(aes(dbh, BAT_IEFC_2), color = eq_colors[4], size = 1.5) +
    geom_line(aes(dbh, BAT_INIA_1), color = eq_colors[2], size = 1.5) +
    geom_line(aes(dbh, BAT_RUIZ_1), color = eq_colors[3], size = 1.5) +
    geom_line(aes(dbh, VOB), color = "dark green", size = 1.5) +
    facet_wrap(~ species, scale = "free") +
    new_theme

# Comparar partes -------------------------------------------------------------------------------------------------

colors = c("IEFC_1" = eq_colors[1], "INIA" = eq_colors[2], "RUIZ" = eq_colors[3])

# Biomasa del fuste
fuste <- data_bat %>% filter(dep_var_group == "BF")
table(fuste$species, fuste$source)

bat_output %>%
    select(species, dbh, Ht, starts_with("BF")) %>%
    ggplot() +
    geom_line(aes(dbh, BF_IEFC_1, color = "IEFC_1"), size = 1.15) +
    geom_line(aes(dbh, BF_INIA_1, color = "INIA"), size = 1.15) +
    geom_line(aes(dbh, BF_RUIZ_1, color = "RUIZ"), size = 1.15) +
    facet_wrap(~ species, scale = "free") +
    labs(y = "Biomasa fuste", color = "Equation") +
    new_theme +
    scale_color_manual(values = colors) 
ggsave("figures/check_BFuste.png", width = 10, height = 6)

# Biomassa de ramas gruesas
rama_gr <- data_bat %>% filter(dep_var_group %in% c("BR7", "BR", "BRH"))
table(rama_gr$species, rama_gr$source)
table(rama_gr$equation, rama_gr$eq_id)
table(rama_gr$equation, rama_gr$dependent_var_units)


bat_output %>%
    ggplot() +
    geom_line(aes(dbh, BR_IEFC_1, color = "IEFC_1"), size = 1.15) +
    geom_line(aes(dbh, BRH_IEFC_1, color = "IEFC_1"), size = 1.15) +
    geom_line(aes(dbh, BR7_INIA_1, color = "INIA"), size = 1.15) +
    geom_line(aes(dbh, BR7_RUIZ_1, color = "RUIZ"), size = 1.15) +
    facet_wrap(~ species, scales = "free") +       
    labs(y = "Biomasa ramas", color = "Equation") +
    new_theme +
    scale_color_manual(values = colors) 
ggsave("figures/check_BRamas_gruesas.png", width = 10, height = 6)

# Biomasa de ramas 2_7

rama_md <- data_bat %>% filter(dep_var_group == "BR2_7")
table(rama_md$species, rama_md$source)

bat_output %>%
    ggplot() +
    geom_line(aes(dbh, BR_IEFC_1, color = "IEFC_1"), size = 1.5) +
    geom_line(aes(dbh, BR2_7_INIA_1, color = "INIA"), size = 1.5) +
    geom_line(aes(dbh, BR2_7_RUIZ_1, color = "RUIZ"), size = 1.5) +
    facet_wrap(~ species, scales = "free") +       
    labs(y = "Biomasa ramas", color = "Equation") +
    new_theme +
    scale_color_manual(values = colors) 
ggsave("figures/check_BRamas_medias.png", width = 10, height = 6)

# biomasa de ramas finas y hojas

rama_pt <- data_bat %>% filter(dep_var_group == "BR2")
table(rama_pt$species, rama_pt$source)

bat_output %>%
    ggplot() +
    geom_line(aes(dbh, BRH_IEFC_1, color = "IEFC_1"), size = 1.5) +
    geom_line(aes(dbh, BR2_INIA_1, color = "INIA"), size = 1.5) +
    geom_line(aes(dbh, BR2_RUIZ_1, color = "RUIZ"), size = 1.5) +
    facet_wrap(~ species, scales = "free") +       
    labs(y = "Biomasa ramas y hojas", color = "Equation") +
    new_theme +
    scale_color_manual(values = colors) 
ggsave("figures/check_BRamas_hojas.png", width = 10, height = 6)




# Comparar total y partes -------------------------------------------------



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

# Partes de Ruiz-Peinado 2011
output_long %>%
    filter(Equation %in% c("RUIZ_1")) %>%
    ggplot() +
    geom_line(aes(dbh, value, color = Equation)) +
    scale_color_viridis_d() +
    facet_wrap(~ species, scale = "free") +
    new_theme



# Partes de INIA
output_long %>%
    filter(Equation %in% c("INIA_1", "IEFC_1", "IEFC_2", "RUIZ_1")) %>%
    ggplot() +
    geom_line(aes(dbh, value, color = Equation), size = 2) +
    facet_wrap(~ species, scale = "free")




