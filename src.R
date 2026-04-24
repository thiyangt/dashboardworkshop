library(tidyverse)

data("penguins")
names(penguins)[names(penguins) == "sex"] <- "gender"
penguins_complete <- penguins |>
  drop_na()
quartiles <- quantile(penguins_complete$body_mass_g, probs = c(0.1, 0.5, 0.8))

pf <- penguins_complete |>
  mutate(BMI_category = case_when(
    body_mass <= quartiles[1] ~ "Underweight",
    body_mass > quartiles[1] & body_mass <= quartiles[2] ~ "Normal",
    body_mass > quartiles[2] & body_mass <= quartiles[3] ~ "Overweight",
    body_mass > quartiles[3] ~ "Obese"
  ))



frequency_table <- pf |>
  count(species) |>
  mutate(percentage = round(n / sum(n) * 100, 2))
