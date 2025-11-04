library(tidyverse)
library(nflreadr)
library(mgcv)
library(odbc)

data <- load_pbp(2022:2025)

data <- data %>% mutate(game_date = as.Date(game_date),
                        name = ifelse(name == "G.Minshew II","G.Minshew",
                                      ifelse(name == "R.Griffin III","R.Griffin",ifelse(name == "Aa.Rodgers","A.Rodgers",name)))) %>% 
  filter(!is.na(epa)) %>% 
  filter(season_type == "REG")

participation <- load_participation(2022:2025)
snap_counts <- load_snap_counts(2022:2025)
ids <- load_ff_playerids()
opp <- load_ff_opportunity(seasons = 2022:2025) %>% drop_na(player_id) %>% mutate(season = as.integer(season)) %>% arrange(game_id)
abbr <- as.data.frame(team_abbr_mapping)
abbr <- rownames_to_column(abbr, var = "team") %>%
  filter(grepl(" ", team), !grepl("LOUIS", team), !grepl("RED", team), !grepl("DIEGO", team), !grepl("OAKLAND", team))
spreads <- load_schedules(2022:2025) %>% dplyr::select(game_id, spread_line, total_line)

snap_counts <- snap_counts %>% left_join(ids %>% dplyr::select(pfr_id, gsis_id), by = c("pfr_player_id" = "pfr_id")) %>% drop_na(player)

opp_new <- opp %>% left_join(snap_counts, by = c("game_id","player_id" = "gsis_id", "season", "position"))

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
    cum_proe = ifelse(!is.na(pass_oe),ewm_irregular_lagged(pass_oe[!is.na(pass_oe)], days_gap[!is.na(pass_oe)], base_off),NA),
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_off)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date, home_team) %>%
  summarize(epa_pp = first(ewm_epa_play),
            proe = first(cum_proe),
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
    cum_proe = ifelse(!is.na(pass_oe),ewm_irregular_lagged(pass_oe[!is.na(pass_oe)], days_gap[!is.na(pass_oe)], base_off),NA),
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_off),
    
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date, home_team) %>%
  summarize(epa_pp = first(ewm_epa_play),
            proe = first(cum_proe),
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
    cum_proe = ifelse(!is.na(pass_oe),ewm_irregular_lagged(pass_oe[!is.na(pass_oe)], days_gap[!is.na(pass_oe)], base_off),NA),
    
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date, home_team) %>%
  summarize(epa_pp = first(ewm_epa_play),
            proe = first(cum_proe),
            .groups = "drop") %>%
  filter(season != 2024)

offense <- rbind(offense_2223, offense_2324, offense_2425)

players2223 <- opp_new %>%
  filter(season %in% c(2022, 2023)) %>%
  group_by(player, player_id) %>%
  mutate(scored = ifelse(rec_touchdown > 0 | rush_touchdown > 0, 1, 0),
         cum_rush = lag(cummean(rush_attempt)),
         cum_rec_air_yards = lag(cummean(rec_air_yards)),
         cum_receptions = lag(cummean(receptions)),
         cum_targets = lag(cummean(rec_attempt)),
         cum_exp_receptions = lag(cummean(receptions_exp)),
         cum_rec_yds = lag(cummean(rec_yards_gained)),
         cum_rush_yds = lag(cummean(rush_yards_gained)),
         cum_exp_rec_yds = lag(cummean(rec_yards_gained_exp)),
         cum_exp_rush_yards = lag(cummean(rush_yards_gained_exp)),
         cum_exp_rush_td = lag(cummean(rush_touchdown_exp)),
         cum_exp_rec_td = lag(cummean(rec_touchdown_exp)),
         cum_off_share = lag(cummean(offense_pct)),
         games = lag(cumsum(!is.na(player)), default = 0)) %>%
  ungroup() %>%
  filter(season == 2023) %>%
  dplyr::select(game_id, player, player_id, posteam,scored,  rush = cum_rush, adot = cum_rec_air_yards, receptions = cum_receptions, targets = cum_targets, x_receptions =cum_exp_receptions, rec_yds = cum_rec_yds,
                rush_yds = cum_rush_yds, x_rec_yds = cum_exp_rec_yds, x_rush_yds = cum_exp_rush_yards, x_rush_td = cum_exp_rush_td, x_rec_td = cum_exp_rec_td, off_share = cum_off_share, games)
  
players2324 <- opp_new %>%
  filter(season %in% c(2023, 2024)) %>%
  group_by(player, player_id) %>%
  mutate(scored = ifelse(rec_touchdown > 0 | rush_touchdown > 0, 1, 0),
         cum_rush = lag(cummean(rush_attempt)),
         cum_rec_air_yards = lag(cummean(rec_air_yards)),
         cum_receptions = lag(cummean(receptions)),
         cum_targets = lag(cummean(rec_attempt)),
         cum_exp_receptions = lag(cummean(receptions_exp)),
         cum_rec_yds = lag(cummean(rec_yards_gained)),
         cum_rush_yds = lag(cummean(rush_yards_gained)),
         cum_exp_rec_yds = lag(cummean(rec_yards_gained_exp)),
         cum_exp_rush_yards = lag(cummean(rush_yards_gained_exp)),
         cum_exp_rush_td = lag(cummean(rush_touchdown_exp)),
         cum_exp_rec_td = lag(cummean(rec_touchdown_exp)),
         cum_off_share = lag(cummean(offense_pct)),
         games = lag(cumsum(!is.na(player)), default = 0)) %>%
  ungroup() %>%
  filter(season == 2024) %>%
  dplyr::select(game_id, player, player_id, posteam,scored,  rush = cum_rush, adot = cum_rec_air_yards, receptions = cum_receptions, targets = cum_targets, x_receptions =cum_exp_receptions, rec_yds = cum_rec_yds,
                rush_yds = cum_rush_yds, x_rec_yds = cum_exp_rec_yds, x_rush_yds = cum_exp_rush_yards, x_rush_td = cum_exp_rush_td, x_rec_td = cum_exp_rec_td, off_share = cum_off_share, games)

players2425 <- opp_new %>%
  filter(season %in% c(2024, 2025)) %>%
  group_by(player, player_id) %>%
  mutate(scored = ifelse(rec_touchdown > 0 | rush_touchdown > 0, 1, 0),
         cum_rush = lag(cummean(rush_attempt)),
         cum_rec_air_yards = lag(cummean(rec_air_yards)),
         cum_receptions = lag(cummean(receptions)),
         cum_targets = lag(cummean(rec_attempt)),
         cum_exp_receptions = lag(cummean(receptions_exp)),
         cum_rec_yds = lag(cummean(rec_yards_gained)),
         cum_rush_yds = lag(cummean(rush_yards_gained)),
         cum_exp_rec_yds = lag(cummean(rec_yards_gained_exp)),
         cum_exp_rush_yards = lag(cummean(rush_yards_gained_exp)),
         cum_exp_rush_td = lag(cummean(rush_touchdown_exp)),
         cum_exp_rec_td = lag(cummean(rec_touchdown_exp)),
         cum_off_share = lag(cummean(offense_pct)),
         games = lag(cumsum(!is.na(player)), default = 0)) %>%
  ungroup() %>%
  filter(season == 2025) %>%
  dplyr::select(game_id, player, player_id, posteam,scored,  rush = cum_rush, adot = cum_rec_air_yards, receptions = cum_receptions, targets = cum_targets, x_receptions =cum_exp_receptions, rec_yds = cum_rec_yds,
                rush_yds = cum_rush_yds, x_rec_yds = cum_exp_rec_yds, x_rush_yds = cum_exp_rush_yards, x_rush_td = cum_exp_rush_td, x_rec_td = cum_exp_rec_td, off_share = cum_off_share, games)

players <- rbind(players2223, players2324, players2425)

df_train <- players %>%
  left_join(offense, by = c("posteam", "game_id")) %>%
  left_join(spreads, by = "game_id") %>%
  mutate(spread_line = ifelse(home_team == posteam, spread_line, -spread_line))

dt <- sample(nrow(df_train), .7 * nrow(df_train))
train <- df_train[dt,]
test <- df_train[-dt,]

simple <- glm(scored ~  epa_pp + x_rec_yds + x_rec_td + x_rush_yds + x_rush_td + spread_line + total_line, data = train, family = "binomial")
summary(simple)
complex <- mgcv::gam(scored ~  spread_line + total_line + s(x_rec_yds, x_rec_td) + s(x_rush_td, x_rush_yds) , data = train, family = "binomial")
summary(complex)

test$prob_td <- predict(complex, test, type = "response")

Metrics::logLoss(test$scored[!is.na(test$prob_td)], test$prob_td[!is.na(test$prob_td)])

grid <- expand.grid(total_line = c(45), spread_line = c(-100:100)/10, x_rec_td = c(.4), x_rush_td = c(.1), x_rec_yds = c(50), x_rush_yds = c(3))
grid$prob_td <- predict(complex, grid, type = "response")

ggplot(grid, aes(spread_line, prob_td)) +
  geom_line() +
  theme_bw()

ggplot(grid, aes(spread_line, total_line, color = prob_td)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient2(high = "red", low = "blue", mid = "white", midpoint = median(grid$prob_td))


########## Odds history
get_oddsjam<- function(){
  db_connection <- DBI::dbConnect(odbc::odbc(),
                                  Driver="{SnowflakeDSIIDriver}",
                                  Server="DRAFTKINGS-DRAFTKINGS.snowflakecomputing.com",
                                  Database="ODDSJAM",
                                  SCHEMA="DBO",
                                  UID="b.cashman",
                                  authenticator = 'externalbrowser',
                                  WAREHOUSE="QUERY_WH"
  )
  
  
  query <- "with ranked as (
select *,
ROW_NUMBER() over (PARTITION by FIXTURE_ID, ODDS_SELECTION order by ODDS_TIMESTAMP asc) as opening,
ROW_NUMBER() over (PARTITION by FIXTURE_ID, ODDS_SELECTION order by ODDS_TIMESTAMP desc) as closing
from ODDSJAM.DBO.COMPETITOR_ODDS_NFL
where (ODDS_BET_TYPE = 'Player Touchdowns' or ODDS_MARKET = 'Player Touchdowns')
and odds_is_live = FALSE
and odds_bet_points = 0.5
and odds_selection_line = 'over'
and odds_sportsbook_name != 'Kalshi'
)

select * from ranked 
where (opening = 1 or closing = 1)
order by Fixture_ID, ODDS_SELECTION


"

df <- dbGetQuery(db_connection, query)

return(df)
}

oj <- get_oddsjam()

oj_filtered <- oj %>%
  dplyr::select(FIXTURE_START_DATE_EST,HOME_TEAM, AWAY_TEAM, player = ODDS_SELECTION, ODDS_PRICE_AMERICAN, OPENING, CLOSING ) %>%
  mutate(date = as.Date(FIXTURE_START_DATE_EST),
         HOME_TEAM = toupper(HOME_TEAM),
         AWAY_TEAM = toupper(AWAY_TEAM)) %>%
  left_join(abbr, by = c("HOME_TEAM" = "team")) %>%
  left_join(abbr, by = c("AWAY_TEAM" = "team"), suffix = c("_home", "_away"))

american_to_prob <- function(odds) {
  ifelse(
    odds > 0,
    100 / (odds + 100),   # For positive odds
    -odds / (-odds + 100) # For negative odds
  )
}

open_test <- test %>%
  left_join(oj_filtered %>% filter(OPENING == 1), by = c("home_team" = "team_abbr_mapping_home", "game_date" = "date", "player")) %>%
  mutate(implied_prob = american_to_prob(ODDS_PRICE_AMERICAN))

close_test <- test %>%
  left_join(oj_filtered %>% filter(CLOSING == 1), by = c("home_team" = "team_abbr_mapping_home", "game_date" = "date", "player")) %>%
  mutate(implied_prob = american_to_prob(ODDS_PRICE_AMERICAN))

Metrics::logLoss(open_test$scored[!is.na(open_test$implied_prob) & !is.na(open_test$prob_td)], open_test$implied_prob[!is.na(open_test$implied_prob) & !is.na(open_test$prob_td)])
Metrics::logLoss(open_test$scored[!is.na(open_test$prob_td) & !is.na(open_test$implied_prob)], open_test$prob_td[!is.na(open_test$prob_td) & !is.na(open_test$implied_prob)])

units <- open_test %>%
  mutate(units = ifelse(scored == 0, -1, ifelse(ODDS_PRICE_AMERICAN > 0, ODDS_PRICE_AMERICAN/100, 100/abs(ODDS_PRICE_AMERICAN)) ),
         units_win = ifelse(ODDS_PRICE_AMERICAN > 0, ODDS_PRICE_AMERICAN/100, 100/abs(ODDS_PRICE_AMERICAN)),
         ratio = prob_td/implied_prob,
         ev = prob_td * units_win - (1 - prob_td),
         diff = prob_td - implied_prob)

model <- lm(units ~ ratio, data = units)
summary(model)

grid <- expand.grid(ratio = c(0)/10, ev = c(-7:30)/10)
grid$eu <- predict(model, grid)

ggplot(grid, aes(ev, eu)) +
  geom_line() +
  theme_bw()


model_td_prob <- complex
model_td_scorer_eu <- model

save(model_td_prob, file = "C:/Users/b.cashman/Documents/GitHub/Research/model_td_prob.RDS")
save(model_td_scorer_eu, file = "C:/Users/b.cashman/Documents/GitHub/Research/model_td_scorer_eu.RDS")

# skill2223 <- data %>%
#   filter(season %in% c(2022,2023)) %>%
#   group_by(fantasy_player_id, fantasy_player_name) %>%
#   mutate(rushes = lag(cumsum(rusher_player_id == fantasy_player_id & !is.na(rusher_player_id)), default = 0),
#          targets = lag(cumsum(receiver_player_id == fantasy_player_id & !is.na(receiver_player_id)), default = 0),
#          high_td_prob_rush = lag(
#            cumsum(
#              if_else(td_prob > 0.75 & rusher_player_id == fantasy_player_id, 1L, 0L, missing = 0L)
#            ),
#            default = 0
#          ),
#          sum_adot = lag(cumsum(ifelse(is.na(air_yards), 0 , air_yards))),
#          adot_denom = lag(cumsum(!is.na(air_yards))),
#          adot = sum_adot/adot_denom
#          ) %>%
#   ungroup() %>%
#   group_by(game_id, season, fantasy_player_id, fantasy_player_name) %>%
#   dplyr::summarize(rushes = first(rushes),
#                    targets = first(targets),
#                    high_td_prob_rush = first(high_td_prob_rush),
#                    adot = first(adot)
#                    ) %>% 
#   filter(season == 2023)

