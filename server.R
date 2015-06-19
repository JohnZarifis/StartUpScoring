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
#   MarketFiltered <- reactive({
#      MarketFiltered <- input$Market_rows_all
#      View(MarketFiltered)
#      return(MarketFiltered)
#   })
  
  predicted <- reactive({
    
            M = input$marketSize 
            P = input$pSize
            Q = input$qSize
            t1 = as.integer(input$Years[1])
            t2 = as.integer(input$Years[2])
            Inc = as.numeric(input$Price)
            #level = as.numeric(input$interval)
            Sales <- Bass.Default.Model(M,P,Q,t1,t2,Inc)
            return(Sales)
  })
  
  #---------------------------------------------------------------------------------------------------
  #     Dygraph (Data)
  #---------------------------------------------------------------------------------------------------

  output$dygraph <- renderDygraph({
    DataToPlot <- predicted()
    
    if(is.null(input$checkGroup))
      return()
    #if(length(input$checkGroup)== 1){
      DataToPlot <- DataToPlot[,input$checkGroup]
      dygraph(DataToPlot, main = "Predicted New Sales (Tons/Year)") %>%
      dyOptions(drawGrid = input$showgrid,strokeWidth = 3,fillGraph = input$fillGraph, fillAlpha = 0.4)%>% 
      dyRangeSelector() %>%
      dyAxis( 
            name="x"
            ,axisLabelFormatter = "function(d){ return d.getFullYear() }")
#     } # if i want secondary axis
#     else {
#       DataToPlot <- DataToPlot[,input$checkGroup]
#       firstAx <- head(input$checkGroup,n=-1)
#       SecondAx <-tail(input$checkGroup,1)
#       #print(firstAx)
#       #print(SecondAx)
#       #DataToPlot <- DataToPlot[,firstAx]
#       dygraph(DataToPlot, main = "Predicted New Sales (Tons/Year)") %>%
#       dyOptions(drawGrid = input$showgrid,strokeWidth = 3,fillGraph = input$fillGraph, fillAlpha = 0.4)%>% 
#       dyRangeSelector() %>%
#       dyAxis( 
#         name="x"
#         ,axisLabelFormatter = "function(d){ return d.getFullYear() }"
#       
#        )%>%
#        dyAxis("y", label = "Tons of Production") %>%
#        dyAxis("y2", label = "Euro/Year", independentTicks = TRUE) %>%
#        dySeries(SecondAx, axis = 'y2')
#       
#     }
    
      
  })
  
  #---------------------------------------------------------------------------------------------------
  #     Dislpay dataset (Data)
  #---------------------------------------------------------------------------------------------------
  # Dislpay dataset
  SalesToDF <- function(){
    Sales <- predicted() 
    # convert it to data frame
    Sales <- data.frame(Year = substr(index(Sales), 1, 4), coredata(Sales))
    Sales <- Sales[c('Year',input$checkGroup)]
    return(Sales)
  }
  
  output$Sales <- DT::renderDataTable(
    
       SalesToDF()
       ,server = FALSE
       , class='compact'
       , rowname = FALSE, caption='Predicted Sales'
       , filter = 'top'
       #,extensions = 'FixedHeader' # not working . double headers
       ,extensions = 'ColVis'
       , options = list(
           dom = 'C<"clear">lfrtip',
           colVis = list(exclude = c(0))
           ,autoWidth=TRUE,pageLength = 15
                        )
       )
       
  
##### Market Size Data table.
  output$Market <- DT::renderDataTable(
                   Market() 
                  , class='compact'
                  , rowname = FALSE
                  , caption='Market Size'
                  , filter = 'top'
                  ,extensions = 'ColVis'
                  , options = list(
                      dom = 'C<"clear">lfrtip'
                     ,colVis = list(exclude = c(0))
                     ,autoWidth=TRUE,pageLength = 15
                                  ) 
                  )
                 
  
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

# Debugging
# output$x5 = renderPrint({ # for debugging reasons
#   cat('\nAll rows (same as rows_current in the server mode):\n\n')
#   cat(input$Sales_rows_all, sep = ' | ')
#   cat('\n')
#   cat('Rows on the current page:\n\n')
#   cat(input$Sales_rows_current, sep = ' | ')
#   cat('\n')
#   cat('\nSelected rows:\n\n')
#   cat(input$Sales_rows_selected, sep = ' | ')
  
})


#})
