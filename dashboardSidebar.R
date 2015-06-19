sidebar <- dashboardSidebar(
  img(src='logo.jpg',class ='img-responsive')
  ,sidebarMenu(
    menuItem("Bass Diffsion Model", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("MarketSize", icon = icon("bar-chart-o"), tabName = "Market",
             badgeLabel = "Data!", badgeColor = "green")
  )
  ,hr()
  
  
  
)
