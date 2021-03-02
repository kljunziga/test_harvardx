library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

#system2("open", args = fn)

txt <- pdf_text(fn)

x <- txt[[9]] %>% str_split("\n")

x %>% length()

s <- x[[1]]
class(s)
length(s)

s <- str_trim(s)
s[[2]]


str_which(s, "2015")

header_index <- 2

header <- s[[header_index]]

temp <- header %>% str_split("    |  ") %>% unlist() %>% str_trim()

month <- temp[1]
header <- temp[-1]


tail_index <- str_which(s, "Total")

sum(str_count(s, "\\d+")==1)

temp <- s[header_index+4:tail_index-3] 
index <- str_count(temp, "\\d+")!=1
s <- temp[index] #%>% count()


s <- str_remove_all(s, "[^\\d\\s]")

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

colnames(s) <- c("day", header)
class(s)
storage.mode(s) <- "numeric"


s[,"2017"] %>% tail(11) %>% mean()
tab <- s

tab <- as.data.frame(tab)

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab %>% ggplot(aes(day,deaths,col=year))+
  geom_line() +
  geom_vline(xintercept = 20)
