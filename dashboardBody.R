body <- dashboardBody(tags$head(tags$style(
  type = 'text/css',
  '#MarketPivot{ overflow-x: scroll; }'
)),
  tabItems(
#######tabname dashboard ####
    tabItem(tabName = 'dashboard'
           # ,h1("Bass Diffusion Model.")
             ,fluidRow(
              column(width = 9,
                     tabBox(
                        title = 'Bass Diffusion Model'
                        ,width = NULL
                        ,status = 'warning'
                       # The id lets us use input$tabset1 on the server to find the current tab
                       ,id = 'tabset1', height = '600px'
                       ,selected = 'Graph'
                       ,tabPanel('Graph'
                                
                                ,dygraphOutput('dygraph', height = 500)
                                )
                       
                       
                       ,tabPanel('Data'
                                 
                                 ,DT::dataTableOutput('Sales')
                                 #,verbatimTextOutput('x5') # for debugging reasons
                                 )
                       
                       
                          ) # end of tabBox
             
              ) # end column
              , column(width = 3,
                      box( title = 'Criteria'
                           ,status = "warning"
                           ,width = NULL
                           ,solidHeader = TRUE
                           ,collapsible = TRUE
                           ,sliderInput('marketSize','Market Size:', min = 500,max = 500000, value = 10000, step = 500, sep = '.')
                           ,sliderInput('pSize',' P-Innovation:', min = 0.01,max = 1, value = 0.03, step = 0.01)
                           ,sliderInput('qSize','Q-Imitation:', min = 0.2,max = 0.8, value = 0.38, step = 0.01)
                           ,sliderInput('Years','Years:'
                                        ,min = as.integer(format(Sys.Date(), format="%Y")) +1
                                        ,max = as.integer(format(Sys.Date(), format="%Y"))+25
                                        ,value = c(as.integer(format(Sys.Date(), format="%Y"))+1,as.integer(format(Sys.Date(), format="%Y"))+25), step = 1, sep = '')
                           ,sliderInput('Price','Price Policy (Euro/Ton)',min = 0 , max = 20, value = 4, step = 1)
                           ,checkboxGroupInput("checkGroup", inline = 'TRUE'
                                               ,label = h3("New/Cummulative Sales") 
                                               ,choices = list("New Sales" = 'Sales' 
                                                               ,"Cummulative Sales" = 'CumSales'
                                                               ,"Income" = 'Income')
                                               ,selected = 'Sales')
                           ,hr()
                           ,checkboxInput("showgrid", label = "Show Grid", value = TRUE)
                           ,checkboxInput("fillGraph", label = "Fill Graph", value = TRUE)
                           ,shinyFilesButton('file', 'Upload Market Size Data', 'Please select a file', FALSE)
                           
                           ) # end box
                           
                       ) # end column
                )# end fluidRow
    )
            
#######end tabItem dashboard #####
    ,tabItem(tabName = 'Market'
             ,fluidRow(
               column(width = 12,
                      tabBox(
                         title = 'Estimate Market Size'
                        ,width = NULL
                        ,status = 'warning'
                        # The id lets us use input$tabset1 on the server to find the current tab
                        ,id = 'tabset2' , height = '600px'
                        ,selected = 'Market Data'
                        ,tabPanel('Market Data'
                        ,DT::dataTableOutput('Market')
                        
                            )# end tabpanel
                        ,tabPanel('Market Pivot'
                                  
                        ,rpivotTableOutput("MarketPivot", height = "800px") 
                        )
                        
                        ,tabPanel('Predict Market Future'
                        #,rpivotTableOutput("MarketFilter", height = "800px")  
                        #,dygraphOutput('dygraphPred', height = 500)
                        ,DT::dataTableOutput('dygraphPred')
                                  ) # end tabPanel
                        
                      )  #end tabbox 
            ) # end column
          ) # end fluidrow
    ) 
##### end tabItem  #####
    
  )#end tabItems
)


