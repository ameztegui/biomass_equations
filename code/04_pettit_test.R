
library(trend)  # for Pettit test
library(tidyverse)

load("./data/BAT_output.Rdata")

sps <- unique(bat_output$species)

# Some stats ------------------------------------------------------------------------------------------------------

kruskal.test(r_INIA_IEFC1 ~ CD, data = filter(bat_output)) 
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

output_pettitt <- list()
for (i in seq_along(sps)) {
    
    print(sps[i])
    data <- bat_output %>%
        filter(species == sps[i]) %>%
        select(dbh, CD, starts_with("r_")) %>%
        na.omit()
    
    pettittTest_IEFC2_IEFC1 <- trend::pettitt.test(data$r_IEFC2_IEFC1)
    pettittTest_INIA_IEFC1  <- trend::pettitt.test(data$r_INIA_IEFC1)
    pettittTest_INIA_IEFC2  <- trend::pettitt.test(data$r_INIA_IEFC2)
    pettittTest_INIA_RUIZ   <- trend::pettitt.test(data$r_INIA_RUIZ)
    pettittTest_RUIZ_IEFC1  <- trend::pettitt.test(data$r_RUIZ_IEFC1)
    pettittTest_RUIZ_IEFC2  <- trend::pettitt.test(data$r_RUIZ_IEFC2)
    
    results <- data.frame(
        species = sps[i],
        t_IEFC2_IEFC1 = (data$dbh[pettittTest_IEFC2_IEFC1$estimate])[1],
        p_IEFC2_IEFC1 = pettittTest_IEFC2_IEFC1$p.value,
        
        t_INIA_IEFC1  = (data$dbh[pettittTest_INIA_IEFC1$estimate])[1],
        p_INIA_IEFC1 = pettittTest_INIA_IEFC1$p.value,
        
        t_INIA_IEFC2  = (data$dbh[pettittTest_INIA_IEFC2$estimate])[1],
        p_INIA_IEFC2 = pettittTest_INIA_IEFC2$p.value,
        
        t_INIA_RUIZ   = (data$dbh[pettittTest_INIA_RUIZ$estimate])[1],
        p_INIA_RUIZ = pettittTest_INIA_RUIZ$p.value,
        
        t_RUIZ_IEFC1  = (data$dbh[pettittTest_RUIZ_IEFC1$estimate])[1],
        p_RUIZ_IEFC1 = pettittTest_RUIZ_IEFC1$p.value,
        
        t_RUIZ_IEFC2  = (data$dbh[pettittTest_RUIZ_IEFC2$estimate])[1],
        p_RUIZ_IEFC2 = pettittTest_RUIZ_IEFC2$p.value)
    
    output_pettitt[[i]] <- results

}


pettit_long <- left_join(bat_output, bind_rows(output_pettitt)) %>%
    select(species, dbh, CD, starts_with("r_"), starts_with("t_"), starts_with("p_")) %>% 
    pivot_longer(-c(species, dbh, CD), names_to = c("var", "comp"), names_sep = 2, values_to = "diff") %>%
    pivot_wider(names_from = var, values_from = diff)

ggplot(pettit_long) +
    geom_line(aes(dbh, r_, color = comp), size = 1.5) +
    geom_vline(aes(xintercept = t_, color = comp), size = 1, linetype = "dashed") +
    facet_wrap(. ~ species) +
    ylab("normalized difference") +
    scale_color_brewer(type = "qual", palette = 2) +
theme_bw()




# Bruishand test ----------------------------------------------------------

output_br <- list()
for (i in seq_along(sps)) {
    
    print(sps[i])
    data <- bat_output %>%
        filter(species == sps[i]) %>%
        select(dbh, CD, starts_with("r_")) %>%
        na.omit()
    
    br_IEFC2_IEFC1 <- trend::br.test(data$r_IEFC2_IEFC1)
    br_INIA_IEFC1  <- trend::br.test(data$r_INIA_IEFC1)
    br_INIA_IEFC2  <- trend::br.test(data$r_INIA_IEFC2)
    br_INIA_RUIZ   <- trend::br.test(data$r_INIA_RUIZ)
    br_RUIZ_IEFC1  <- trend::br.test(data$r_RUIZ_IEFC1)
    br_RUIZ_IEFC2  <- trend::br.test(data$r_RUIZ_IEFC2)
    
    results <- data.frame(
        species = sps[i],
        t_IEFC2_IEFC1 = (data$dbh[br_IEFC2_IEFC1$estimate])[1],
        p_IEFC2_IEFC1 = br_IEFC2_IEFC1$p.value,
        
        t_INIA_IEFC1  = (data$dbh[br_INIA_IEFC1$estimate])[1],
        p_INIA_IEFC1 = br_INIA_IEFC1$p.value,
        
        t_INIA_IEFC2  = (data$dbh[br_INIA_IEFC2$estimate])[1],
        p_INIA_IEFC2 = br_INIA_IEFC2$p.value,
        
        t_INIA_RUIZ   = (data$dbh[br_INIA_RUIZ$estimate])[1],
        p_INIA_RUIZ =  br_INIA_RUIZ$p.value,
        
        t_RUIZ_IEFC1  = (data$dbh[br_RUIZ_IEFC1$estimate])[1],
        p_RUIZ_IEFC1 = br_RUIZ_IEFC1$p.value,
        
        t_RUIZ_IEFC2  = (data$dbh[br_RUIZ_IEFC2$estimate])[1],
        p_RUIZ_IEFC2 = br_RUIZ_IEFC2$p.value)
    
    output_br[[i]] <- results
    
}


br_long <- left_join(bat_output, bind_rows(output_br)) %>%
    select(species, dbh, CD, starts_with("r_"), starts_with("t_"), starts_with("p_")) %>% 
    pivot_longer(-c(species, dbh, CD), names_to = c("var", "comp"), names_sep = 2, values_to = "diff") %>%
    pivot_wider(names_from = var, values_from = diff)

ggplot(br_long) +
    geom_line(aes(dbh, r_, color = comp), size = 1.5) +
    geom_vline(aes(xintercept = t_, color = comp), size = 1, linetype = "dashed") +
    facet_wrap(. ~ species) +
    ylab("normalized difference") +
    scale_color_brewer(type = "qual", palette = 2) +
    theme_bw()




#####################


pettit_data <- bat_output %>%
    # filter(species == "Abies alba") %>%
    group_by(dbh) %>%
    summarise(mean_ratio = mean(r_INIA_IEFC1, na.rm = T),
           sd_ratio = sd(r_INIA_IEFC1, na.rm = T)) %>%
    na.omit() 

pettittTest <- trend::pettitt.test(x = pettit_data$mean_ratio)
print(pettittTest)
print(pettit_data$dbh[pettittTest$estimate])

bruishandTest <- trend::br.test(x = pettit_data$mean_ratio)
print(bruishandTest)
print(pettit_data$dbh[bruishandTest$estimate])

}

calculate_pettit(foo, "Abies alba")
calculate_pettit(bat_output, "F")
# deberiamos agrupar los valores por dbh??

# hacerlo por especie

# paquete change point  -> bucar mas de un punto de ruptura

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

