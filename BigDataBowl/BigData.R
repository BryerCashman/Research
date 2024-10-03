library(tidyverse)
library(ggplot2)

setwd("/Users/bryer/documents/NFL Projects/BigDataBowl")
week1 <- read.csv("tracking_week_1.csv")
week2 <- read.csv("tracking_week_2.csv")
week3 <- read.csv("tracking_week_3.csv")
week4 <- read.csv("tracking_week_4.csv")
week5 <- read.csv("tracking_week_5.csv")
week6 <- read.csv("tracking_week_6.csv")
week7 <- read.csv("tracking_week_7.csv")
week8 <- read.csv("tracking_week_8.csv")
week9 <- read.csv("tracking_week_9.csv")
tackles <- read.csv("tackles.csv")

total <- rbind(week1,week2,week3,week4,week5,week6,week7,week8,week9)
rm(week2,week3,week4,week5,week6,week7,week8,week9)

tackles_comb <- total %>%
  left_join(tackles,by = c("gameId","playId","nflId")) %>%
  mutate(tackle_on_play = tackle) %>%
  select(-tackle)


tackles_comb$tackle_on_play[is.na(tackles_comb$tackle_on_play)] <- 0
tackles_comb$assist[is.na(tackles_comb$assist)] <- 0
tackles_comb$forcedFumble[is.na(tackles_comb$forcedFumble)] <- 0
tackles_comb$pff_missedTackle[is.na(tackles_comb$pff_missedTackle)] <- 0


