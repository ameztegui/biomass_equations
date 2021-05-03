rm(list=ls())

library(tidyverse)
library(Hmisc)
library(sf)
library(patchwork)

load("./data/BAT_output.Rdata")
load( "./data/sps_list.Rdata")
data_allom <- data_bat %>% select(species, dependent_var, param_a:param_d, eq_id)

# 
# df = trees
# variable = "BAT"
# equation = "RUIZ"
# dbh = "Dn"
# height = "Ht"
# sps <- "Name"
# 
# df <- data.frame(species = c("Abies alba", "Abies alba", "Pinus sylvestris", "Pinus uncinata"),
#                  diam = c(10,20,39,35),
#                  altura = c(4,10, 18, 15))


source("./code/calculate_allom.R")

# Test with IFN3 ----------------------------------------------------------

load("C:/Dades/Software/IFN/data/rawdata/IFN3_Spain.Rdata")

names <- read_tsv("C:/Dades/Software/IFN/data/rawdata/TreeCodes.txt")
trees <- PCMayores_IFN3 %>% 
    # filter(Provincia %in% c("08", "17", "25", "43")) %>%
    filter(Dn != 0, Ht != 0) %>%
    left_join(names, by = c("Especie" = "CodeIFN3")) %>%
    mutate(Name = case_when(Name == "Eucalyptus camaldulensis" ~ "Eucalyptus spp.",
                             Name == "Eucalyptus globulus" ~ "Eucalyptus spp.",
                             Name == "Fraxinus angustifolia" ~ "Fraxinus spp.",
                             Name == "Fraxinus excelsior" ~ "Fraxinus spp.",
                             Name == "Quercus pubescens" ~ "Quercus faginea",
                             TRUE ~ Name)) 


trees$IEFC_1_unit <- calculate_allom (trees, "BAT", equation = "IEFC_1", dbh = "Dn", height = "Ht", sps = "Name")
trees$IEFC_2_unit <- calculate_allom (trees, "BAT", equation = "IEFC_2", dbh = "Dn", height = "Ht", sps = "Name")
trees$INIA_unit <- calculate_allom (trees, "BAT", equation = "INIA", dbh = "Dn", height = "Ht", sps = "Name", frac = F)
trees$RUIZ_unit <- calculate_allom (trees, "BAT", equation = "RUIZ", dbh = "Dn", height = "Ht", sps = "Name", frac = F)

trees <- trees %>%
    mutate(IEFC_1_unit = if_else(Name == "Populus x canadensis", IEFC_2_unit, IEFC_1_unit),
           BA = N*pi*(Dn/100)^2/4)
           

# Check the results! 

trees %>%
    filter(Name %in% sps_list) %>%
    ggplot() +
    geom_line(aes(Dn, IEFC_1_unit), color = "red") +
    geom_line(aes(Dn, IEFC_2_unit), color = "orange") +
    geom_line(aes(Dn, INIA_unit), color = "green") +
    geom_line(aes(Dn, RUIZ_unit), color = "blue") +
    facet_wrap(~Name, scales = "free")
    

#  Plot the results -------------------------------------------------------

# Calculate sumamry by plot

sps_dom <- trees %>%
    group_by(Codigo, Name) %>%
    summarise(BA_tot = sum(BA)) %>%
    mutate(prop_BA = 100*BA_tot/sum(BA_tot)) 

incl_dom <- trees %>%
    mutate(Include = if_else(Name %in% sps_list, 1, 0)) %>%
    group_by(Codigo, Include) %>%
    summarise(BA_tot = sum(BA)) %>%
    mutate(prop_BA = 100*BA_tot/sum(BA_tot)) %>%
    filter(Include == 1, prop_BA > 80) %>%
    pull(Codigo)

plots <- trees %>%
    filter(Codigo %in% incl_dom) %>%
    mutate(IEFC_1 = IEFC_1_unit * N/1000,
           IEFC_2 = IEFC_2_unit * N/1000,
           INIA = INIA_unit * N/1000,
           RUIZ = RUIZ_unit * N/1000) %>%
    group_by(Codigo)%>%
    summarise(across(IEFC_1:RUIZ,.fns = sum, na.rm = T)) %>%
    mutate(across(IEFC_1:RUIZ, ~na_if(., 0))) %>%
    mutate(IE1_2 = 100*(IEFC_2 - IEFC_1)/IEFC_1,
           IE1_INIA = 100*(INIA - IEFC_1)/IEFC_1,
           IE1_RUIZ = 100*(RUIZ - IEFC_1)/IEFC_1,
           IE2_INIA = 100*(INIA - IEFC_2)/IEFC_2,
           IE2_RUIZ = 100*(RUIZ - IEFC_2)/IEFC_2,
           INIA_RUIZ = 100*(RUIZ - INIA)/INIA)

# Calculate number of plot per dif. class
plots_class <- plots %>%
    mutate(class_IE1_2 = cut(IE1_2, breaks = c(-Inf, -25, -10, 10, 25, 50, Inf)),
           class_IE1_INIA = cut(IE1_INIA, breaks = c(-Inf, -25, -10, 10, 25, 50, Inf)),
           class_IE1_RUIZ = cut(IE1_RUIZ, breaks = c(-Inf, -25, -10, 10, 25, 50, Inf)),
           class_IE2_INIA = cut(IE2_INIA, breaks = c(-Inf, -25, -10, 10, 25, 50, Inf)),
           class_IE2_RUIZ = cut(IE2_RUIZ, breaks = c(-Inf, -25, -10, 10, 25, 50, Inf)),
           class_INIA_RUIZ = cut(INIA_RUIZ, breaks = c(-Inf, -25, -10, 10, 25, 50, Inf)))

prop.table(table(plots_class$class_IE1_2))
prop.table(table(plots_class$class_IE1_INIA))
prop.table(table(plots_class$class_IE1_RUIZ))
prop.table(table(plots_class$class_IE2_INIA))
prop.table(table(plots_class$class_IE2_RUIZ))
prop.table(table(plots_class$class_INIA_RUIZ))


# Add coordinates
load("Y:/IFN/data/coords_ifn_ETRS89_30.Rdata")

spain_plots <- real_coor_spain %>% right_join(plots)
st_write(spain_plots, "data/plots_biomass.shp", append = F)

new_theme <- theme_bw() +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "italic"),
          legend.position = "bottom",
          axis.title.x = element_text(hjust = 1),
          axis.title.y = element_text(angle = 90, 
                                      hjust = 1))

# Linear models

lin_mod_IE1_2 <- summary(lm(IEFC_2 ~ IEFC_1, data = plots))
lin_mod_IE1_INIA <- summary(lm(INIA ~ IEFC_1, data = plots))
lin_mod_IE1_RUIZ <- summary(lm(RUIZ ~ IEFC_1, data = plots))
lin_mod_IE2_INIA <- summary(lm(INIA ~ IEFC_2, data = plots))
lin_mod_IE2_RUIZ <- summary(lm(RUIZ ~ IEFC_2, data = plots))
lin_mod_INIA_RUIZ <- summary(lm(RUIZ ~ INIA, data = plots))

b1 <- coefficients(lin_mod_INIA_RUIZ)[2]
sigma_beta1 <- coefficients(lin_mod_INIA_RUIZ)[2,2]
n= nrow(plots)
pvalue <- pt(q = (b1 - 1)/sigma_beta1, df = n - 2, lower.tail = FALSE) * 2

IEFC_1_2 <- ggplot(plots) +
    geom_point(aes(IEFC_1, IEFC_2)) +
    geom_abline(intercept = 0,slope =1) +
    geom_smooth(aes(IEFC_1, IEFC_2),method='lm', color = "orange") +
    xlim(c(0,600)) + ylim(c(0,600)) +
    new_theme 
IEFC_1_INIA <- ggplot(plots) +
    geom_point(aes(IEFC_1, INIA)) +
    geom_abline(intercept = 0,slope =1)+
    geom_smooth(aes(IEFC_1, INIA),method='lm', color = "orange") +
    xlim(c(0,600)) + ylim(c(0,600)) +
    new_theme
IEFC_1_RUIZ <- ggplot(plots) +
    geom_point(aes(IEFC_1, RUIZ)) +
    geom_abline(intercept = 0,slope =1)+
    geom_smooth(aes(IEFC_1, RUIZ),method='lm', color = "orange") +
    xlim(c(0,600)) + ylim(c(0,600)) +
    new_theme
IEFC_2_INIA <- ggplot(plots) +
    geom_point(aes(IEFC_2, INIA)) +
    geom_abline(intercept = 0,slope =1)+
    geom_smooth(aes(IEFC_2, INIA),method='lm', color = "orange") +
    xlim(c(0,600)) + ylim(c(0,600)) +
    new_theme
IEFC_2_RUIZ <- ggplot(plots) +
    geom_point(aes(IEFC_2, RUIZ)) +
    geom_abline(intercept = 0,slope =1)+
    geom_smooth(aes(IEFC_2, RUIZ),method='lm', color = "orange") +
    xlim(c(0,600)) + ylim(c(0,600)) +
    new_theme
INIA_RUIZ <- ggplot(plots) +
    geom_point(aes(INIA, RUIZ)) +
    geom_abline(intercept = 0,slope =1)+
    geom_smooth(aes(INIA, RUIZ),method='lm', color = "orange") +
    xlim(c(0,600)) + ylim(c(0,600)) +
    new_theme

IEFC_1_2 + IEFC_1_INIA + IEFC_1_RUIZ + IEFC_2_INIA + IEFC_2_RUIZ + INIA_RUIZ
ggsave("figures/plot_level_corr_spain.png", width = 8, height = 6)

hist_theme <- new_theme +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())

hist_IEFC1_2 <- ggplot(plots) +
    geom_histogram(aes(IE1_2)) +
    xlim(c(-100,100)) +
    hist_theme +
    ggtitle("IEFC1 vs. IEFC2")+
    geom_vline(aes(xintercept =0))

hist_IEFC1_INIA <- ggplot(plots) +
    geom_histogram(aes(IE1_INIA)) +
    xlim(c(-100,100)) +
    hist_theme +
    ggtitle("IEFC1 vs. INIA")+
    geom_vline(aes(xintercept =0))

hist_IEFC1_RUIZ <- ggplot(plots) +
    geom_histogram(aes(IE1_RUIZ)) +
    xlim(c(-100,100)) +
    hist_theme +
    ggtitle("IEFC1 vs. RUIZ")+
    geom_vline(aes(xintercept =0))

hist_IEFC2_INIA <- ggplot(plots) +
    geom_histogram(aes(IE2_INIA)) +
    xlim(c(-100,100)) +
    hist_theme +
    ggtitle("IEFC2 vs. INIA")+
    geom_vline(aes(xintercept =0))

hist_IEFC2_RUIZ <- ggplot(plots) +
    geom_histogram(aes(IE2_RUIZ)) +
    xlim(c(-100,100)) +
    hist_theme +
    ggtitle("IEFC2 vs. RUIZ")+
    geom_vline(aes(xintercept =0))

hist_INIA_RUIZ <- ggplot(plots) +
    geom_histogram(aes(INIA_RUIZ)) +
    xlim(c(-100,100)) +
    hist_theme +
    ggtitle("INIA vs. RUIZ")+
    geom_vline(aes(xintercept =0))

hist_IEFC1_2 + hist_IEFC1_INIA + hist_IEFC1_RUIZ +
    hist_IEFC2_INIA + hist_IEFC2_RUIZ + hist_INIA_RUIZ
ggsave("figures/plot_level_hist_spain.png", width = 8, height = 6)
