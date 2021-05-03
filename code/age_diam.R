rm(list = ls())

library(tidyverse) 
library(readxl)
library(broom)

coefs <- read_excel("data/coefs.xlsx", sheet = 1) 

# Explore diameter-age relationships --------------------------------------

age_diam <- coefs %>%
  mutate(a = if_else(is.na(a) & Tipo == "Conifera", 6, 
                     if_else(is.na(a) & Tipo == "Frondosa", 6, a)),
         b = if_else(is.na(b) & Tipo == "Conifera", 2.86, 
                     if_else(is.na(b) & Tipo == "Frondosa", 1.55, b)),
         c = if_else(is.na(c) & Tipo == "Conifera", 0.00591, 
                     if_else(is.na(c) & Tipo == "Frondosa", 0.00596, c))) %>%
  rowwise() %>%
  mutate(DBH = list(seq(0, 100, 0.5))) %>%
  unnest(DBH) %>%
  mutate(age = a + b*DBH + c*DBH^2) %>%
  group_by(Tipo, Especie) %>%
  nest()


fit_diam_age <- function(df) {
    nls(DBH ~ in_a + in_b*age + in_c*age^2, data = df, 
        start = list(in_a=1, in_b = 1, in_c = 1))
}

diam_age_models <- age_diam %>%
    mutate(mod = map(data, fit_diam_age),
           glance = mod %>% map(broom::glance),
           augment = mod %>% map(augment),
           tidy = mod %>% map(tidy))


# Calculate coefficients
diam_age <- diam_age_models %>%
    unnest(tidy) %>%
    dplyr::select(Tipo, Especie, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate)

# Calculate predicted values
diam_age_pred <- diam_age_models %>%
    dplyr::select(Tipo, Especie, augment) %>%
    unnest(augment) 


# # Calculate R2
# get_r2 <- function (df) {
#     linear <- lm(fitted ~ DBH, data = df)
#     r2 <- summary(linear)$r.squared
#     r2
# }
# 
# diam_age_r2 <- diam_age_pred %>%
#     rename(fitted = .fitted) %>%
#     group_by(Tipo, Especie) %>%
#     nest() %>%
#     mutate(R2 = map_dbl(data, get_r2)) %>%
#     dplyr::select(-data)
# 
# 
# diam_age <- diam_age %>%
#     left_join(diam_age_r2)


diam_age %>%
  rowwise() %>%
  mutate(age = list(seq(0, 200, 1))) %>%
  unnest(age) %>%
  mutate(DBH = in_a + in_b*age + in_c*age^2) %>%
  ggplot() +
  geom_line(aes(age, DBH), color = "red") +
  facet_wrap( ~ Especie, scale = "free")

save(diam_age, file = "data/diam_age.Rdata")
  
