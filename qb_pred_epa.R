qb_data1921 <- data %>%
  filter(season %in% c(2019, 2020, 2021),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA)
  ) %>%
  filter(season == 2021)

qb_data2022 <- data %>%
  filter(season %in% c(2020, 2021, 2022),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA)
  ) %>%
  ungroup() %>%
  filter(season == 2022)

qb_data2123 <- data %>%
  filter(season %in% c(2021, 2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA)
  ) %>%
  ungroup() %>%
  filter(season == 2023)

qb_data2224 <- data %>%
  filter(season %in% c(2022, 2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA)
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs),
            qb_sack_rate = dplyr::first(ewm_qb_sack_rate, na_rm = T),
            qb_air_epa = dplyr::first(ewm_air_epa, na_rm = T),
            .groups = "drop") %>%
  filter(season == 2024)

qb_data2325 <- data %>%
  filter(season %in% c(2023, 2024, 2025),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA)
  ) %>%
  ungroup() %>%
  filter(season == 2025)

qb_data_pred <- rbind(qb_data1921, qb_data2022, qb_data2123, qb_data2224, qb_data2325)
rm(qb_data1921, qb_data2022, qb_data2123, qb_data2224, qb_data2325)

m <- lm(qb_epa ~ ewm_qb_epa_play, data = qb_data_pred)
m <- mgcv::gam(qb_epa ~ s(ewm_qb_epa_play, log(dbs + 1)), data = qb_data_pred)
summary(m)


grid <- expand.grid(ewm_qb_epa_play = c(-200:300)/1000, dbs = c(0:800))
grid$lm  <- predict(m, grid)
grid$gam <- predict(m, grid)

ggplot(grid, aes(dbs, ewm_qb_epa_play, color = gam)) +
  geom_point() +
  scale_color_gradient2(high = "red", low = "blue", mid = "white", midpoint = mean(grid$gam)) +
  theme_bw()

model_pred_qb_epa <- m

save(model_pred_qb_epa, file = "/Users/bryer/Documents/GitHub/model_pred_qba.R")
