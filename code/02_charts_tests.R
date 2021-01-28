# source("./code/00_wrangle_data.R")
source("./code/01_compare_BAT_values.R")

library(trend)  # for Pettit test


# Divide the sample in dbh classes --------------------------------------------------------------------------------

foo <- bat_output %>%
     group_by(func_group, species, CD) %>%
     summarise(mean_ratio = mean(r_INIA_IEFC1, na.rm = T),
               sd_ratio = sd(r_INIA_IEFC1, na.rm = T)) 
 
 foo %>%
     ggplot(aes(x = CD, y = mean_ratio)) +
     geom_point(aes(y = mean_ratio, color = species),
                position = position_jitter(width = .25), size = .9, alpha=0.5) +
     geom_boxplot(aes(color = func_group), width = .6, outlier.shape = NA, alpha = 0.5) +
     geom_hline(yintercept=1, linetype="dashed", color = "black") +
     coord_flip()


# Some stats ------------------------------------------------------------------------------------------------------

kruskal.test(r_INIA_IEFC1 ~ CD, data = filter(bat_output, func_group == "F")) 
kruskal.test(r_INIA_IEFC1 ~ func_group, data = bat_output)
kruskal.test(r_INIA_IEFC1 ~ species, data = bat_output)

# Should we create mean values per CD class and then run stats on this?
bat_output %>%
    ggplot(aes(x = CD, y = r_INIA_IEFC1)) +
    geom_point(aes(y = r_INIA_IEFC1, color = species),
               position = position_jitter(width = .25), size = .9, alpha=0.5) +
    geom_boxplot(aes(color = func_group), width = .6, outlier.shape = NA, alpha = 0.5) +
    geom_hline(yintercept=1, linetype="dashed", color = "black") +
    coord_flip()


### Pettit test

calculate_pettit <- function( data, func_group) {
    
pettit_data <- data %>%
    filter(func_group == {{func_group}}) %>%
    select(dbh, r_INIA_IEFC1) %>%
    # group_by(CD) %>%
    # summarise(mean_ratio = mean(r_INIA_IEFC1, na.rm = T),
    #           sd_ratio = sd(r_INIA_IEFC1, na.rm = T)) %>%
    na.omit() %>%
    arrange(dbh)

pettittTest <- trend::pettitt.test(x = pettit_data$r_INIA_IEFC1)
print(pettittTest)
print(pettit_data$dbh[pettittTest$estimate])

bruishandTest <- trend::br.test(x = pettit_data$r_INIA_IEFC1)
print(bruishandTest)
print(pettit_data$dbh[bruishandTest$estimate])

}

calculate_pettit(bat_output, "C")
calculate_pettit(bat_output, "F")
# deberiamos agrupar los valores por dbh??


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
    scale_color_viridis_a() +
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
