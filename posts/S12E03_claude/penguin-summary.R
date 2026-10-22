# Summarize body mass for Adelie and Gentoo penguins
# (uses the penguins data that is built into R 4.5.0 and later)

library(tidyverse)

# Keep only Adelie and Gentoo penguins
two_species <- penguins |>
  filter(species == c("Adelie", "Gentoo"))

# Number of penguins and mean body mass per species
two_species |>
  group_by(species) |>
  summarize(
    n = n(),
    mean_mass_g = mean(body_mass)
  )
