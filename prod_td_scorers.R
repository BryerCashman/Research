library(nflreadr)
library(tidyverse)
library(mgcv)
library(odbc)

load("C:/Users/b.cashman/Documents/GitHub/Research/model_td_prob.RDS")
load("C:/Users/b.cashman/Documents/GitHub/Research/model_td_scorer_eu.RDS")

b <- 0.964582
current_week <- 11

sched <- load_schedules(2024:2025)
data <- load_ff_opportunity(2024:2025)

sunday <- sched %>%
  filter(week == current_week, season == 2025) %>%
  group_by(date = gameday) %>%
  dplyr::summarize(count = n()) %>%
  ungroup() %>%
  slice_max(order_by = count,n = 1) %>%
  pull(date) %>%
  as.Date()

sunday <- Sys.Date()

data <- data %>% inner_join(sched %>% dplyr::select(game_id, gameday), by = "game_id")

df_players <- data %>%
  arrange(game_id) %>%
  mutate(days_diff = as.numeric(difftime(sunday,gameday, "days"))) %>%
  group_by(player_id, full_name) %>%
  dplyr::summarize(x_rush_td = sum(rush_touchdown_exp * b^days_diff, na.rm = T)/sum(b^days_diff, na.rm = T),
                   x_rec_td = sum(rec_touchdown_exp * b^days_diff, na.rm = T)/sum(b^days_diff, na.rm = T),
                   x_rec_yds = sum(rec_yards_gained_exp * b^days_diff, na.rm = T)/sum(b^days_diff, na.rm = T),
                   x_rush_yds = sum(rush_yards_gained_exp * b^days_diff, na.rm = T)/sum(b^days_diff, na.rm = T),
                   team = last(posteam))

snap_counts <- load_snap_counts() %>% inner_join(sched %>% dplyr::select(game_id, gameday), by = "game_id")

df_snaps <- snap_counts %>%
  arrange(game_id) %>%
  mutate(days_diff = as.numeric(difftime(sunday,gameday, "days"))) %>%
  group_by(player) %>%
  dplyr::summarize(off_share = sum(offense_pct * b^days_diff, na.rm = T)/sum(b^days_diff, na.rm = T))

weekly1 <- sched %>% filter(week == current_week, season == 2025) %>%
  dplyr::select(game_id, team = home_team, spread_line, total_line)

weekly2 <- sched %>% filter(week == current_week, season == 2025) %>%
  mutate(spread_line = -spread_line) %>%
  dplyr::select(game_id, team = away_team, spread_line, total_line)

weekly <- rbind(weekly1, weekly2)


df_predict <- weekly %>%
  inner_join(df_players, by = "team") %>%
  left_join(df_snaps, by = c( "full_name" = "player"))

df_predict$prob_td <- predict(model_td_prob, df_predict, type = "response")


get_oddsjam_scorers <- function(){
  db_connection <- DBI::dbConnect(odbc::odbc(),
                                  Driver="{SnowflakeDSIIDriver}",
                                  Server="DRAFTKINGS-DRAFTKINGS.snowflakecomputing.com",
                                  Database="DWSPORTSBOOK",
                                  SCHEMA="TEN",
                                  UID="b.cashman",
                                  authenticator = 'externalbrowser',
                                  WAREHOUSE="QUERY_WH"
  )
  
  
  query <- paste0("with ranked as (
select *,
ROW_NUMBER() over (PARTITION by FIXTURE_ID, ODDS_SELECTION, ODDS_SPORTSBOOK_NAME order by ODDS_TIMESTAMP desc) as closing
from ODDSJAM.DBO.COMPETITOR_ODDS_NFL
where (ODDS_BET_TYPE = 'Player Touchdowns' or ODDS_MARKET = 'Player Touchdowns')
and odds_is_live = FALSE
and odds_bet_points = 0.5
and odds_selection_line = 'over'
and ODDS_SPORTSBOOK_NAME in ('FanDuel', 'Circa Sports', 'Caesars', 'BetMGM')
and FIXTURE_START_DATE_EST >'", Sys.Date(),"'
)

select * from ranked 
where (closing = 1)
order by Fixture_ID, ODDS_SELECTION")

df <- dbGetQuery(db_connection, query)
}

odds <- get_oddsjam_scorers()

foruse <- odds %>%
  dplyr::select(player = ODDS_SELECTION, ODDS_PRICE_AMERICAN, ODDS_SPORTSBOOK_NAME) %>%
  group_by(player) %>%
  slice_max(ODDS_PRICE_AMERICAN, n = 1, with_ties = F) 



df_ratios <- df_predict %>%
  left_join(foruse , by = c("full_name" = "player"))

american_to_prob <- function(odds) {
  ifelse(
    odds > 0,
    100 / (odds + 100),   # For positive odds
    -odds / (-odds + 100) # For negative odds
  )
}

df_ratios$implied_prob <- american_to_prob(df_ratios$ODDS_PRICE_AMERICAN)
df_ratios$ratio <- df_ratios$prob_td / df_ratios$implied_prob
df_ratios$eu <- predict(model_td_scorer_eu, df_ratios)

recs <- df_ratios %>%
  filter(ratio > 1) %>%
  arrange(desc(eu)) %>%
  dplyr::select(game_id, team, full_name, prob_td, implied_prob, ratio, ODDS_PRICE_AMERICAN, ODDS_SPORTSBOOK_NAME, eu)
