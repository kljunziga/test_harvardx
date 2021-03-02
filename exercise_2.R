library(rvest)
library(dplyr)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

#nodes[[22]]

#html_table(nodes[[1]])
html_table(nodes[[21]])
html_table(nodes[[20]])
html_table(nodes[[21]])

length(nodes)

tab_1 <- html_table(nodes[[10]]) %>% as_data_frame %>% select(-X1) 
tab_2 <- html_table(nodes[[19]]) %>% as_data_frame 


tab_1 <- tab_1[-1,] %>% rename(Team=X2, Payroll=X3, Average=X4)
tab_2 <- tab_2[-1,] %>% rename(Team=X1, Payroll=X2, Average=X3)

tab_1 %>% full_join(tab_2, by="Team") %>% nrow()