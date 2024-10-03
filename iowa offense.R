if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/sportsdataverse-R")

season <- cfbfastR:::most_recent_cfb_season()

season <- cfbfastR::load_cfb_pbp(2023)

leaders <- season %>%
  group_by(pos_team) %>%
  dplyr::summarize(epe_pp = round(mean(EPA,na.rm = T),2),
                   plays = n(),
                   conf = last(offense_conference)) %>%
  filter(plays > 500)

power5 <- leaders %>%
  filter(conf %in% c("Big Ten","Big 12","SEC","Pac-12","ACC"))


ggplot(power5, aes(conf,epe_pp,fill = pos_team)) +
  geom_point(shape = 21,size = 3) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y.left  = element_text("Offensive EPA per Play")) +
  scale_fill_manual(values = c("Iowa" = "#FFCD00" )) 




