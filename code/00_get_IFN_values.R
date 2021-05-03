load("Y:/IFN/data/rawdata/IFN4_Spain.Rdata")


names <- read_tsv("Y:/IFN/data/rawdata/TreeCodes.txt") %>%
    mutate(Name = case_when(CodeIFN3 == 61 ~ "Eucalyptus spp.",
                            CodeIFN3 == 62 ~ "Eucalyptus spp.",
                            CodeIFN3 == 64 ~ "Eucalyptus spp.",
                            CodeIFN3 == 364 ~ "Eucalyptus spp.",
                            CodeIFN3 == 55 ~ "Fraxinus spp.",
                            CodeIFN3 == 255 ~ "Fraxinus spp.",
                            CodeIFN3 == 455 ~ "Fraxinus spp.",
                            CodeIFN3 == 955 ~ "Fraxinus spp.",
                            TRUE ~ Name))

trees <- PCMayores_IFN4 %>%
    left_join(names, by = c("Especie" = "CodeIFN3")) %>%
    filter(Name %in% sps_list) %>%
    left_join(dmin_dmax, by = c("Name" = "species")) %>%
    mutate(species = Name, 
           dbh = (Dn1+Dn2)/20,
           CD = cut(dbh,breaks = seq(5,80, 10), 
                    include.lowest = T,
                    labels = c("CD10", "CD20", "CD30", "CD40", "CD50","CD60", "CD70"))) %>%
    filter(!is.na(CD)) 


trees %>%
    ggplot() +
    geom_bar(aes(CD)) +
    facet_wrap(. ~ species, scales = "free_y")

max_bio <- output_long  %>% 
    group_by(species) %>%
    summarise(max = max(value, na.rm = T))

trees_data <- trees %>%
    group_by(species, CD) %>%
    summarise(n = n()) %>%
    group_by(species) %>%
    mutate(n_trees = n/max(n)) %>%
     separate(CD, into = c(NA, "dbh"), sep = 2,remove = F, convert = T) %>%
    left_join(max_bio) %>%
    mutate(n_trees2 = n_trees*max)




# Calculate number of trees within dmin and dmax ----------------------------------------

trees %>% 
    group_by(Name) %>%
    summarise(dmin = mean(dmin),
              dmax = mean(dmax),
              dmin_obs = min(Dn),
              dmax_obs = max(Dn),
              menos = 100*sum(N[Dn < dmin])/sum(N),
              mas = 100*sum(N[Dn > dmax])/sum(N),
              entre = 100*sum(N[Dn > dmin & Dn < dmax])/sum(N))

