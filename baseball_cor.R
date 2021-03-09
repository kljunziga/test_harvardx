library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


data(Teams)

Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(RG = R/G, ABG = AB/G, WG = W/G, EG = E/G, X2G = X3B/G, X3G = X2B/G) %>% ggplot(aes(X3G,X2G))+
  geom_point()

new_teams <- Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(RG = R/G, ABG = AB/G, WG = W/G, EG = E/G, X2G = X3B/G, X3G = X2B/G)

cor(new_teams$X2G, new_teams$X3G)