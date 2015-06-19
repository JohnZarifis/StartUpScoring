library(metricsgraphics)
library(RColorBrewer)

tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))

tmp %>%
  mjs_plot(x=year, y=uspop) %>%
  mjs_line() %>%
  mjs_add_marker(1850, "Something Wonderful") %>%
  mjs_add_baseline(150, "Something Awful")


tmp %>%
  mjs_plot(x=year, y=uspop, width=600) %>%
  mjs_line(area=TRUE)

tmp %>%
  mjs_plot(x=uspop, y=year, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')


mtcars %>%
  mjs_plot(x=wt, y=mpg, width=600, height=500) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")


mtcars %>%
  mjs_plot(x=wt, y=mpg, width=600, height=500) %>%
  mjs_point(color_accessor=cyl,
            x_rug=TRUE, y_rug=TRUE,
            size_accessor=carb,
            size_range=c(5, 10),
            color_type="category",
            color_range=brewer.pal(n=11, name="RdBu")[c(1, 5, 11)]) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")


mtcars %>%
  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
  mjs_point(least_squares=TRUE) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")


set.seed(1492)
dat <- data.frame(date=seq(as.Date("2014-01-01"),
                           as.Date("2014-01-31"),
                           by="1 day"),
                  value=rnorm(n=31, mean=0, sd=2))

dat %>%
  mjs_plot(x=date, y=value) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format = "date")

# Custom rollovers

dat %>%
  mjs_plot(x=date, y=value) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format = "date") %>%
  mjs_add_mouseover("function(d, i) {
                    $('{{ID}} svg .mg-active-datapoint')
                    .text('custom text : ' + d.date + ' ' + i);
                    }")

# also works for scatterplots with a slight mod

set.seed(1492)
dat <- data.frame(value=rnorm(n=30, mean=5, sd=1),
                  value2=rnorm(n=30, mean=4, sd=1),
                  test = c(rep(c('test', 'test2'), 15)))
dat %>%
  mjs_plot(x = value, y = value2) %>%
  mjs_point() %>%
  mjs_add_mouseover("function(d, i) {
                    $('{{ID}} svg .mg-active-datapoint')
                    .text('custom text : ' + d.point.test + ' ' + i);
                    }")

set.seed(1492)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4))

stocks %>%
  mjs_plot(x=time, y=X) %>%
  mjs_line() %>%
  mjs_axis_x(show=FALSE) %>%
  mjs_axis_y(show=FALSE)

stocks %>%
  mjs_plot(x=time, y=X) %>%
  mjs_line() %>%
  mjs_add_line(Y) %>%
  mjs_add_line(Z) %>%
  mjs_axis_x(xax_format="date")

mjs_plot(rnorm(10000)) %>%
  mjs_histogram(bins=30, bar_margin=1)

movies <- ggplot2::movies[sample(nrow(ggplot2::movies), 1000), ]

mjs_plot(movies$rating) %>% mjs_histogram()

mjs_plot(movies, rating) %>% 
  mjs_histogram() %>% 
  mjs_labs(x_label="Histogram of movie ratings", 
           y_label="Frequency")

mjs_plot(movies$rating) %>% mjs_histogram(bins=30)

mjs_plot(runif(10000)) %>% 
  mjs_labs(x_label="runif(10000)") %>%
  mjs_histogram()


mjs_plot(rbeta(10000, 2, 5)) %>%
  mjs_labs(x_label="rbeta(10000, 2, 3)") %>%
  mjs_histogram(bins=100) %>% 
  mjs_axis_y(extended_ticks=TRUE)

bimod <- c(rnorm(1000, 0, 1), rnorm(1000, 3, 1))
mjs_plot(bimod) %>% mjs_histogram() 
mjs_plot(bimod) %>% mjs_histogram(bins=30) 

bimod %>% mjs_hist(30)

library(shiny)
library(metricsgraphics)

ui = shinyUI(fluidPage(
  h3("MetricsGraphics Example", style="text-align:center"),
  metricsgraphicsOutput('mjs1'),
  br(),
  metricsgraphicsOutput('mjs2')
))

server = function(input, output) {
  
  mtcars %>%
    mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
    mjs_point(color_accessor=carb, size_accessor=carb) %>%
    mjs_labs(x="Weight of Car", y="Miles per Gallon") -> m1
  
  set.seed(1492)
  stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4))
  
  stocks %>%
    mjs_plot(x=time, y=X) %>%
    mjs_line() %>%
    mjs_add_line(Y) %>%
    mjs_add_line(Z) %>%
    mjs_axis_x(xax_format="date") %>%
    mjs_add_legend(legend=c("X", "Y", "Z")) -> m2
  
  output$mjs1 <- renderMetricsgraphics(m1)
  
  output$mjs2 <- renderMetricsgraphics(m2)
  
}

shinyApp(ui = ui, server = server)