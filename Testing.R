Market <- read_excel("~/Projects/StartUpScoring/Market.xlsx", sheet = 1 ,col_names = TRUE, na='na')
View(Market)
class(Market)
colnames(Market)
colnames( Market ) <- str_replace_all(colnames( Market ), c(" " = "", "-" = ".","%"=".perc"))
i <- sapply(Market, is.character)
Market[i] <- lapply(Market[i], as.factor)

print(i)


View(Market)

summary(Market)

 Test <- tidyr::spread(Market, Species, Production)
 
 View(Test)
 
 
 dplyr::group_by(Market, Year)
 library(dplyr)
 Market %>% group_by(Year) %>% summarise(sum(Production))
 



