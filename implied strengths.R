library(tidyverse)
library(nflreadr)
library(nflplotR)
library(stringr)
library(mgcv)
library(gt)
library(scales)
library(purrr)



data <- load_pbp(2022:2024) %>%
  filter((rush == 1 | pass == 1) ) %>%
  mutate(name = ifelse(name == "G.Minshew II","G.Minshew",name))


B <- optimal_beta <- 0.9974176