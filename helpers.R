library('shiny')
library('shinyFiles')
#library('ggplot2')
library('dygraphs')
library('xts')
library('shiny')
library('shinydashboard')
library('shinyFiles')
library('readxl')
library('DT')
library('stringr')
library('rpivotTable')
library('dplyr')



Bass.Default.Model <- function(M,P,Q,t1,t2,Inc){
  
  t <- (t1-t1):(t2-t1)
  #Sales <- M*(((P+Q)^2/P) * exp(-(P+Q)*t) / (1+(Q/P)*exp(-(P+Q)*t))^2
  Sales <- M*(((P+Q)^2/P) * exp(-(P+Q)*t)) / (1+(Q/P)*exp(-(P+Q)*t))^2
  #print(Sales)
  Sales <- round(Sales)
  Sales.frame <- data.frame( 'Years'= t1:t2,'Sales'= Sales,'CumSales'= cumsum(Sales),'Income' = Sales*Inc)
  #Sales.frame <- Sales.frame[-1,]
  #View(Sales.frame)
  Sales.ts <- ts(Sales.frame[,-1], start = t1, frequency = 1)
  dates <- Sales.frame$Years
  values <- Sales.frame[,-1]
  
  #Sales.ts <- ts(Sales, start = t1, frequency = 1 )
  #sales.xts <-as.xts(Sales.ts)
  Sales.xts <-as.xts(values, order.by = as.Date(
    paste0(dates,"-01-01",format="%Y-01-01")
  ))
 
  #return(sales.xts)
  return(Sales.xts)
}

# for testing 
#Sales<-Bass.Default.Model(16000,0.03,0.38,2016,2030)
#Sales
#dygraph(Sales)

#for testing
#t1 <- 2015
#t2<- 2030
#(t1-t1):(t2-t1)