library(dslabs)
data("research_funding_rates")
research_funding_rates

table <- matrix(c(sum(research_funding_rates$awards_men),sum(research_funding_rates$applications_men)-sum(research_funding_rates$awards_men),
                  sum(research_funding_rates$awards_women),sum(research_funding_rates$applications_women)-sum(research_funding_rates$awards_women)), 
                ncol=2)
colnames(table) <- c('Man', 'Woman')
rownames(table) <- c('Awarded', 'Not')
table <- as.table(table)
table



table[1,1]/(table[2,1]+table[1,1])

table[1,2]/(table[2,2]+table[1,2])

table %>% chisq.test()


dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat


dat %>% ggplot(aes(success,discipline,col=gender)) +
  geom_point()


#tako bi lepše spravil v 2x2
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two


