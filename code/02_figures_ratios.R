library(tidyverse)
library(ggridges)
library(extrafont)
library(viridis)
# font_import()
# loadfonts(device = "win")

source("./code/00_get_IFN_values.R")

eq_colors <- c("#5F978E","#5B4887", "#9B2F5D", "#E7BC66", "#D55E00")
# rcp_colors <- c("#0072B2", "#D55E00")
# species_colors <- c("#0072B2","#E69F00","#A14063" )
# okabe_ito <- c( "#D55E00", "#009E73","#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#CC79A7","#999999")

new_theme <- theme_bw() +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "italic"),
          legend.position = "bottom")

new_theme_ridges <- theme_ridges() +
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = "bottom")

load("./data/BAT_output.Rdata")



norm_difs <- c("r_INIA_IEFC1", "r_INIA_IEFC2", "r_INIA_RUIZ", "r_RUIZ_IEFC1" , "r_RUIZ_IEFC2" ,"r_IEFC2_IEFC1",
               "r_BIO_VOL")
bio_eqs <- c("INIA_1", "IEFC_1", "IEFC_2", "RUIZ_1", "VOL")



# Basic plots -----------------------------------------------------------------------------------------------------

#♣ Height
output_long %>%
  filter(Equation == "Ht_IEFC_1") %>%
  ggplot() +
  geom_line(aes(dbh, value), color = "maroon", size = 1.5) +
  xlab("DBH (cm)") + ylab("Height (m)")+
  facet_wrap(~ species, scales = "free") +
  new_theme
ggsave("figures/height_dbh.png")

 output_long %>%
  filter(Equation %in% bio_eqs) %>%
   ggplot() +
   geom_col(aes(dbh, n_trees2), data = trees_data,  alpha = 0.4, fill = "grey37") +
   geom_line(aes(dbh, value, color = Equation), size = 1.05) +
   geom_vline(data = dmin_dmax, aes(xintercept=dmin), color="grey17", linetype="dashed") +
   geom_vline(data = dmin_dmax, aes(xintercept=dmax), color="grey17", linetype="dashed") +
   scale_x_continuous(breaks = seq(10,80,10)) +
   xlab("DBH (cm)") +   ylab("Biomass (kg)") +
   scale_color_manual(values = eq_colors, labels = c("IEFC 1", "IEFC 2", "INIA", "Ruiz", "Volume (IEFC)")) +
   facet_wrap(~ species, scales = "free_y") +
   new_theme +
  theme(legend.title = element_text(""))
ggsave("figures/all_biomass_equations.png", width = 10, height = 6)

# Ridgeline Plots -------------------------------------------------------------------------------------------------

# Ridges
    output_long %>%
      filter(Equation %in% norm_difs,
             Equation != "r_BIO_VOL") %>%
      ggplot() +
      geom_density_ridges(aes(x = value, y = CD), alpha = 0.25, fill = "dark red") +
      geom_vline(xintercept=0, color="black", linetype="dashed") +
      xlim(c(-0.75,0.75)) +
      xlab("Normalized difference") +
      new_theme_ridges  +
      facet_wrap(.~func_group) 
  
    output_long %>%
      filter(Equation %in% norm_difs,
             Equation != "r_BIO_VOL") %>%   
      ggplot() +
      geom_density_ridges(aes(x = value, y = CD, fill = Equation), alpha = 0.5) +
      geom_vline(xintercept=0, color="black", linetype="dashed") +
      xlim(c(-0.75,0.75)) +
      xlab("Normalized difference") +
      new_theme_ridges +
      facet_wrap(.~func_group) 
    
# Ridges por grupo funcional  
    output_long %>%
      filter(Equation %in% norm_difs,
             Equation != "r_BIO_VOL") %>%    
      ggplot() +
      geom_density_ridges(aes(x = value, y = CD, fill = func_group), alpha = 0.5) +
      geom_vline(xintercept=0, color="black", linetype="dashed") +
      xlim(c(-0.75,0.75)) +
      xlab("Normalized difference") +
      facet_wrap(.~func_group) +
      new_theme_ridges  +
      theme(legend.position = "n")
    ggsave("figures/ridges_fg.png", width = 9, height = 6)
    
# Ridges por especie    
    output_long %>%
      filter(Equation %in% norm_difs,
             Equation != "r_BIO_VOL") %>%  
      ggplot() +
      geom_density_ridges(aes(x = value, y = CD, fill = species), alpha = 0.5) +
      geom_vline(xintercept=0, color="black", linetype="dashed") +
      xlab("Normalized difference") +
      xlim(c(-0.5,0.5)) +
      facet_wrap(.~species) +
      new_theme  +
      theme(legend.position = "n")
    ggsave("figures/ridges_sps.png", width = 9, height = 6)
    
# Ridges por ecuacion y grupo    
    output_long %>%
      filter(Equation %in% norm_difs,
             Equation != "r_BIO_VOL") %>%
      ggplot() +
      geom_density_ridges(aes(x = value, y = CD, fill = func_group), alpha = 0.5) +
      geom_vline(xintercept=0, color="black", linetype="dashed") +
      xlim(c(-0.5,0.5)) +
      new_theme   +
      facet_grid(func_group~Equation) +
      theme(legend.position = "n")
    ggsave("figures/ridges_equation.png", width = 9, height = 6)
  
    
## Only volume against biomass -------------------------
    output_long %>%
      filter(Equation %in% c("IEFC_2", "VOL")) %>%
      ggplot() +
      geom_col(aes(dbh, n_trees2), data = trees_data,  alpha = 0.4, fill = "grey37") +
      geom_line(aes(dbh, value, color = Equation), size = 1.05) +
      # geom_vline(data = dmin_dmax, aes(xintercept=RUIZ_dmin), color="grey17", linetype="dashed") +
      # geom_vline(data = dmin_dmax, aes(xintercept=RUIZ_dmax), color="grey17", linetype="dashed") +
      scale_x_continuous(breaks = seq(10,80,10)) +
      xlab("DBH (cm)") +   ylab("Biomass (kg)") +
      # scale_color_manual(values = eq_colors, labels = c("IEFC 1", "IEFC 2", "INIA", "Ruiz", "Volume (IEFC)")) +
      facet_wrap(~ species, scales = "free_y") +
      new_theme +
      theme(legend.title = element_text(""))
    ggsave("figures/volume-biomass_equations.png", width = 10, height = 6)
    
    
    
    output_long %>%
      filter(Equation == "r_BIO_VOL") %>%    
      ggplot() +
      geom_density_ridges(aes(x = value, y = CD, fill = func_group), alpha = 0.5) +
      geom_vline(xintercept=0, color="black", linetype="dashed") +
      xlim(c(-0.75,0.75)) +
      xlab("Normalized difference") +
      facet_wrap(.~func_group) +
      new_theme_ridges  +
      theme(legend.position = "n")
    ggsave("figures/ridges_vol_bat.png", width = 7, height = 6)
    
    
# Forest Plot -----------------------------------------------------------------------------------------------------

my_plot_data <- output_long %>% 
      filter(Equation %in% norm_difs,
             Equation != "r_BIO_VOL") %>%      
      group_by(CD, func_group) %>%
      # group_by(CD, Equation, func_group) %>%
      dplyr::summarise(x = mean(value, na.rm = T),
                     sd = sd(value, na.rm = T),
                     n_cases = n(),
                     xmin = x - 1.96*sd/sqrt(6*17),
                     xmax = x + 1.96*sd/sqrt(6*17),
                     min = min(value, na.rm = T),
                     max = max(value, na.rm = T))

  all <-  output_long %>% 
    filter(Equation %in% norm_difs,
           Equation != "r_BIO_VOL") %>%    
    group_by(CD) %>%
      # group_by(CD, Equation, func_group) %>%
      dplyr::summarise(x = mean(value, na.rm = T),
                       sd = sd(value, na.rm = T),
                       n_cases = n(),
                       xmin = x - 1.96*sd/sqrt(6*17),
                       xmax = x + 1.96*sd/sqrt(6*17),
                       min = min(value, na.rm = T),
                       max = max(value, na.rm = T))  %>%
    mutate(func_group = "All") 

  my_plot_data <- bind_rows(my_plot_data, all)
  
  ggplot(data=my_plot_data, aes(y= CD, x, xmin = x - sd, xmax = x + sd)) + 
    geom_point( size = 2) + 
    #this changes the features of the overall effects
    geom_point(data=subset(my_plot_data, func_group=="All"), size=3, color = "black")+ 
    geom_errorbarh(height=.3) +
    # scale_x_continuous(limits=c(-0.30,0.30), name="Ratio") +
    geom_vline(xintercept=0, color="dark gray", linetype="dashed", alpha=.5, size = 1.2) +
    # annotate("rect", xmin = -0.15, xmax = 0.15, ymin=0, ymax=Inf, fill = "dark gray", alpha = 0.5) +
    #faceting based on my subgroups
    facet_grid(func_group ~., scales= "free", space="free", )+
    #thematic stuff
    ggtitle("Equation Effects")+
    theme_minimal()+
    theme(text=element_text(family="serif",size=18, color="black"),
          # panel.spacing = unit(1, "lines"),
          legend.position = "bottom")
ggsave("figures/forest_plot.png", width = 7, height = 6.5)


    

