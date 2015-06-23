library(dplyr)
library(readxl)
library(xts)
library(dygraphs)
Market <- read_excel("~/Projects/StartUpScoring/Market.xlsx", sheet = 1 ,col_names = TRUE, na='na')
#View(Market)
#class(Market)
#colnames(Market)
colnames( Market ) <- str_replace_all(colnames( Market ), c(" " = "", "-" = ".","%"=".perc"))
i <- sapply(Market, is.character)
Market[i] <- lapply(Market[i], as.factor)

#print(i)
#View(Market)
#summary(Market)

#Test <- tidyr::spread(Market, Species, Production)
#View(Test)
#dplyr::group_by(Market, Year)
#Market %>% group_by(Year) %>% summarise(sum(Production))
 
 MarketTots <- function(t1){
   #Mark <- Market()
   #Mark <- MarketFiltered()
   ToForecast <- Market %>% group_by(Year) %>% summarise(sum(Production))
   i <- sapply(ToForecast, is.factor)
   ToForecast[i] <- lapply(ToForecast[i], as.character)
   i <- sapply(ToForecast, is.character)
   ToForecast[i] <- lapply(ToForecast[i], as.integer)
   #View(ToForecast)
   #print(class(ToForecast))
   #print(summary(ToForecast))
   ToForecast.ts <- ts(ToForecast[,-1], start = t1, frequency = 1)
   dates <- ToForecast$Year #ToForecast[[1]]    
   #View(dates)
   values <- ToForecast[,-1]
   #View(values)
   ToForecast.xts <-as.xts(values, order.by = as.Date(
    paste0(dates,"-01-01",format="%Y-01-01")
   ))
   return(ToForecast.ts)
   #return(ToForecast.xts)
   
 }

Market <-  MarketTots(1990)
class(Market)
M <-as.xts(Market)
View(M)
  hwtest <- HoltWinters(Market,gamma=FALSE)
  hwtest
  p <- predict(hwtest, n.ahead = 10, prediction.interval = TRUE)
  p
  fit <- as.xts(p[,'fit'])
  lwr <-as.xts(p[,'lwr'])
  upr<- as.xts(p[,'upr'])
  
all <- cbind( mk = M,fit=fit,lwr=lwr,upr=upr)
View(all)

dygraph(all, "Fish Production") %>%
  dySeries("sum.Production.", label = "Actual") %>%
  dySeries(c("lwr", "fit", "upr"), label = "Predicted")  








