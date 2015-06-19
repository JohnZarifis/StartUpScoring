# ML app for Scoring Startups...


shinyServer(function(input, output, session) {
  
  
  volumes <- c('Project Files' = getwd())  #getVolumes() #c('R Installation'=R.home())
  fileChoose<- shinyFileChoose(input, 'file', roots=volumes, session=session, restrictions=system.file(package='base'))
  #print(fileChoose)
  
  Market <- reactive({
    if (is.null(fileChoose)){return()}
      
    fileSelected <- parseFilePaths(volumes, input$file)
    Market <- read_excel(as.character(fileSelected$datapath), sheet = 1 ,col_names = TRUE, na='na')
    colnames( Market ) <- str_replace_all(colnames( Market ), c(" " = "", "-" = ".","%"=".perc"))
    i <- sapply(Market, is.character)
    Market[i] <- lapply(Market[i], as.factor)
    #View(Market)
    
    return(Market)
  })
  MarketFiltered <- reactive({
     MarketFiltered <- input$Market_rows_all
     View(MarketFiltered)
     return(MarketFiltered)
  })
  
  predicted <- reactive({
    
            M = input$marketSize 
            P = input$pSize
            Q = input$qSize
            t1 = as.integer(input$Years[1])
            t2 = as.integer(input$Years[2])
            #level = as.numeric(input$interval)
            Sales <- Bass.Default.Model(M,P,Q,t1,t2)
            return(Sales)
  })
  
  #---------------------------------------------------------------------------------------------------
  #     Dygraph (Data)
  #---------------------------------------------------------------------------------------------------

  output$dygraph <- renderDygraph({
    DataToPlot <- predicted()
    
    if(is.null(input$checkGroup))
      return()
    
    DataToPlot <- DataToPlot[,input$checkGroup]
    dygraph(DataToPlot, main = "Predicted New Sales (Tons/Year)") %>%
    dyOptions(drawGrid = input$showgrid,strokeWidth = 3,fillGraph = input$fillGraph, fillAlpha = 0.4)%>% 
    dyRangeSelector() %>%
    dyAxis( 
        name="x"
      ,axisLabelFormatter = "function(d){ return d.getFullYear() }"
      )
      
  })
  
  #---------------------------------------------------------------------------------------------------
  #     Dislpay dataset (Data)
  #---------------------------------------------------------------------------------------------------
  # Dislpay dataset
  
  output$Sales <- DT::renderDataTable({
    Sales <- predicted() 
    # convert it to data frame
    Sales <- data.frame(Year = substr(index(Sales), 1, 4), coredata(Sales))
    Sales <- Sales[c('Year',input$checkGroup)]
    DT::datatable(  Sales
                  , class='compact'
                  , rowname = FALSE, caption='Predicted Sales'
                  , filter = 'top'
                  , options=list(autoWidth=TRUE) ) 
  })
  
##### Market Size Data table.
  output$Market <- DT::renderDataTable({
    Market <- Market() 

    DT::datatable(Market 
                  , class='compact'
                  , rowname = FALSE
                  , caption='Market Size'
                  , filter = 'top'
                  , options=list(autoWidth=TRUE) )
  })              
  
  ###### Pivot #########
  output$MarketPivot <- renderRpivotTable({
    if(is.null(Market())) {
      return()
    }
    rpivotTable(data =   Market()   
#                 ,rows = c( "Party",    "Province")
#                 ,vals = "votes"
#                 ,aggregatorName = "Sum"
#                 , rendererName = "Treemap"
                )
  } )
  
#   output$MarketFilter <- DT::renderDataTable({
#     
#     DT::datatable(MarketFiltered()  
#                   , class='compact', rowname = FALSE, caption='Predicted Sales',
#                   filter = 'top', options=list(lengthChange = FALSE) )
#     #server = FALSE
#   })              



})
