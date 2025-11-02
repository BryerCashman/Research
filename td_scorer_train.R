library(tidyverse)
library(nflreadr)

data <- load_pbp(2022:2025)

data <- data %>% mutate(game_date = as.Date(game_date),
                        name = ifelse(name == "G.Minshew II","G.Minshew",
                                      ifelse(name == "R.Griffin III","R.Griffin",ifelse(name == "Aa.Rodgers","A.Rodgers",name)))) %>% 
  filter(!is.na(epa)) %>% 
  filter(season_type == "REG")

participation <- load_participation(2022:2025)

ewm_irregular_lagged <- function(x, days_gap, base) {
  stopifnot(length(x) == length(days_gap))
  n <- length(x)
  s <- 0.0  # decayed numerator
  w <- 0.0  # decayed weight
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    # mean *before* seeing row i
    out[i] <- if (w > 0) s / w else NA_real_
    # now update state with row i
    k <- base ^ days_gap[i]          # decay since previous row
    s <- s * k + x[i]                # add current value
    w <- w * k + 1.0                 # add unit weight
  }
  out
}


tds <- data %>%
  group_by(game_id, fantasy_player_id, fantasy_player_name) %>%
  dplyr::summarize(tds = sum(td_player_id == fantasy_player_id, na.rm = T)) %>%
  mutate(scored = ifelse(tds > 0, 1, 0))

base_off <- 0.9962453


offense_2223 <- data %>%
  filter(season %in% c(2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    cum_proe = ewm_irregular_lagged(pass_oe, days_gap, base_off)
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_off),

  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            proe = first(proe),
            .groups = "drop") %>%
  filter(season != 2022)

offense_2324 <- data %>%
  filter(season %in% c(2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    cum_proe = ewm_irregular_lagged(pass_oe, days_gap, base_off)
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_off),
    
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            cum_proe = ewm_irregular_lagged(pass_oe, days_gap, base_off),
            .groups = "drop") %>%
  filter(season != 2023)

offense_2425 <- data %>%
  filter(season %in% c(2024, 2025),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_off),
    cum_proe = ewm_irregular_lagged(pass_oe, days_gap, base_off)
    
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            proe = first(proe),
            .groups = "drop") %>%
  filter(season != 2024)

offense <- rbind(offense_2223, offense_2324, offense_2425)

skill2223 <- data %>%
  filter(season %in% c(2022,23)) %>%
  group_by(fantasy_player_id, fantasy_player_name) %>%
  mutate(rushes = lag(cumsum(rusher_player_id == fantasy_player_id)),
         targets = lag(cumsum(receiver_player_id == fantasy_player_id)),
         high_td_prob_rush = lag(coalesce(cumsum(rusher_player_id[td_prob > .75] == fantasy_player_id[td_prob > .75])), 0)
         )
