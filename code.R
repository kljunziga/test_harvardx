library(tidyverse)
library(dslabs)

co2

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))


co2_tidy <- co2_wide %>% gather(month, co2, -year)
head(co2_tidy)

co2_tidy <- gather(co2_wide,month,co2,-year)
head(co2_tidy)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()


library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

dat %>% spread(gender,admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp %>% unite(column_name, gender, key) %>% spread(column_name, value)

tmp2 <- unite(tmp, column_name, c(key, gender))

