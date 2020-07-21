sidebarMenu(
  id = "tabs",
  
  #Tab for introduction about the app
  menuItem("About", tabName = "about", icon = icon("address-card")),
  
  #Tab for data process
  menuItem("Data Preparation", tabName = "datapreparation", icon = icon("wrench")),
  
  #View your data
  menuItem("View your data", tabName = "", icon = icon("binoculars"),
           menuSubItem("Trade dataset", tabName = "data_trade", icon = icon("globe")),
           menuSubItem("Revision table", tabName = "data_revision", icon = icon("calendar-alt")),
           menuSubItem("Product table", tabName = "data_product", icon = icon("shopping-cart"))),

  #Choose the parameters
  menuItem("Parameters", tabName = "parameters",icon=icon("filter")),
  
  #Tab for results and analysis
  menuItem("Analysis", tabName = "", icon = icon("cogs"),
           menuSubItem("Table", tabName = "table_analysis", icon = icon("table")),
           menuSubItem("Graph", tabName = "graph_analysis", icon = icon("chart-line"))),
  
  #Explanation for the app
  menuItem("Help", tabName = "help", icon = icon("question"))
)