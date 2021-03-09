library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(total_pa = sum(pa), mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  #filter(total_pa >= 100) %>%
  select(playerID, mean_singles, mean_bb)


bat_9901 %>% filter(mean_singles>0.2) %>% nrow()
bat_9901 %>% filter(mean_bb>0.2) %>% nrow()


inner_join(bat_02, bat_9901, copy = FALSE, by="playerID") %>%
  summarize(single=cor(singles, mean_singles), bb = cor(bb, mean_bb))


singles_plot <- inner_join(bat_02, bat_9901, copy = FALSE, by="playerID") %>% 
  ggplot(aes(mean_singles, singles))+
    geom_point()

bb_plot <- inner_join(bat_02, bat_9901, copy = FALSE, by="playerID") %>% 
  ggplot(aes(mean_bb, bb))+
  geom_point()

grid.arrange(singles_plot, bb_plot, ncol=2)


temp <- inner_join(bat_02, bat_9901, copy = FALSE, by="playerID")
lm(singles ~ mean_singles, data = temp)
lm(bb ~ mean_bb, data = temp)


