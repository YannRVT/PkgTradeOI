###########################################################################
##Selection variable trade dataset
###########################################################################
varnames = reactive({
  data = rawInputData()
  if(!is.null(data)) 
    v = c(names(data))
  else 
    v = NULL
  return(v)
})

###############################################################################
##Trade dataset##
###############################################################################
output$responseUI1 = renderUI({
  data = rawInputData();
  #check if the data is loaded first
  if(is.null(data)){
    return(helpText("Choose an input trade dataset"))
  } else {
    return(selectInput("var_reporter","Reporter variable name",varnames(),selected=varnames()[2]));
  }
})
###############################################################################
output$responseUI2 = renderUI({
  data = rawInputData();
  if(is.null(data)){
  } else {
    return(selectInput("var_partner","Partner variable name",varnames(), selected=varnames()[3]));
  }
})


###############################################################################
output$responseUI3 = renderUI({
  
  data = rawInputData();
  if(is.null(data)){
  } else {
    return(selectInput("var_product","Product variable name",varnames(),selected=varnames()[1]));  
  }
})

###############################################################################
output$responseUI4 = renderUI({
  
  data = rawInputData();
  if(is.null(data)){
  } else {
    return(selectInput("var_value","Value variable name",varnames(),selected=varnames()[7]));
  }
})
###############################################################################
output$responseUI5 = renderUI({
  
  data = rawInputData();
  if(is.null(data)){
  } else {
    return(selectInput("var_quantity","Quantity variable name",varnames(),selected=varnames()[9]));
  }
})
###############################################################################
output$responseUI6 = renderUI({
  
  data = rawInputData();
  if(is.null(data)){
  } else {
    return(selectInput("var_quantity_unit","Quantity unit variable name",varnames(),selected=varnames()[5]));
  }
})
###############################################################################
output$responseUI7 = renderUI({
  
  data = rawInputData();
  if(is.null(data)){
  } else {
    return(selectInput("var_time","Time variable name",varnames(),selected=varnames()[4]));
  }
})

###########################################################################
##Selection variable product table
###########################################################################
varnames2 = reactive({
  data2 = rawInputData2()
  if(!is.null(data2)) 
    v = names(data2) 
  else 
    v = NULL
  v
})

###############################################################################
##Product Table##
##############################################################################

output$responseUI8 = renderUI({
  
  data = rawInputData2();
  if(is.null(data)){
    return(helpText("Choose a product table"))
  } else {
    return(selectInput("var_product_PDT","Product variable name in product table",varnames2(),selected=varnames2()[1]));
  }
})
###############################################################################
output$responseUI9 = renderUI({
  
  data = rawInputData2();
  if(is.null(data)){
  } else {
    return(selectInput("var_revision_PDT","Revision variable name in product table",varnames2(),selected=varnames2()[2]));
  }
})

########################################################################################
##Multiple selection
########################################################################################
output$selectDeselectUI = renderUI({
  data = rawInputData()
  if(!is.null(data)){
    actionButton("selectDeselect", "Select/Deselect all", class="btn-block btn-success",
                 style="color: #fff; background-color: #0066ff; border-color: #2e6da4")
  }
})
########################################################################################
output$explanatoryUI <- renderUI({
  if(!is.null(input$selectDeselect)){
    if(input$selectDeselect %% 2 == 0){
      selectInput("var_key_add","Additional variables for further breakdown",varnames(),selected=NULL,multiple=T)
    } else {
      selectInput("var_key_add","Additional variables for further breakdown",varnames(),selected=varnames(),multiple=T)
    }
  }
})


###########################################################################
##Selection product trade dataset
###########################################################################
varnames3 = reactive({
  data = rawInputData()
  names(data)[names(data) == input$var_product] <- "PRODUCT_CD"
  data = data$PRODUCT_CD
  if(!is.null(data)) 
    v = unique(data)
  else 
    v = NULL
  return(v)
})

########################################################################################
##Multiple selection for product
########################################################################################
output$selectDeselectUI2 = renderUI({
  data = rawInputData()
  if(is.null(data)){
    return(helpText("Choose a trade table"))
  }else{
    actionButton("selectDeselect2", "Select/Deselect all", class="btn-block btn-success",
                 style="color: #fff; background-color: #0066ff; border-color: #2e6da4")
  }
})
########################################################################################
output$productUI <- renderUI({
  if(!is.null(input$selectDeselect)){
    if(input$selectDeselect2 %% 2 == 0){
      selectInput("var_key_add2","Product code",varnames3(),selected=NULL,multiple=T)
    } else {
      selectInput("var_key_add2","Product code",varnames3(),selected=varnames3(),multiple=T)
    }
  }
})

###########################################################################
##Selection partner trade dataset
###########################################################################
varnames4 = reactive({
  data = rawInputData()
  names(data)[names(data) == input$var_partner] <- "PARTNER_CD"
  data = data$PARTNER_CD
  if(!is.null(data)) 
    v = unique(data)
  else 
    v = NULL
  return(v)
})

########################################################################################
##Multiple selection for partner
########################################################################################
output$selectDeselectUI3 = renderUI({
  data = rawInputData()
  if(is.null(data)){
    return(helpText("Choose a trade table"))
  }else{
    actionButton("selectDeselect3", "Select/Deselect all", class="btn-block btn-success",
                 style="color: #fff; background-color: #0066ff; border-color: #2e6da4")
  }
})
########################################################################################
output$partnerUI <- renderUI({
  if(!is.null(input$selectDeselect)){
    if(input$selectDeselect3 %% 2 == 0){
      selectInput("var_key_add3","Partner",varnames4(),selected=NULL,multiple=T)
    } else {
      selectInput("var_key_add3","Partner",varnames4(),selected=varnames4(),multiple=T)
    }
  }
})


###########################################################################
##Selection variable product indice dataset
###########################################################################
varnames5 = reactive({
  data = indices_table()
  data = data$Product
  if(!is.null(data)) 
    v = unique(data)
  else 
    v = NULL
  return(v)
})

###############################################################################
##Trade dataset##
###############################################################################
output$indicesUIproduct = renderUI({
  data = indices_table()
  #check if the data is loaded first
  if(is.null(data)){
    return(helpText("You do not compute indices"))
  } else {
    return(selectInput("var_indices_product","Product code on indices tables",varnames5(),selected=varnames5()[1]));
  }
})

###########################################################################
##Selection variable partner indice dataset
###########################################################################
varnames6 = reactive({
  data = indices_table()
  data = select(data, Product, Partner)
  data = filter(data, Product == input$var_indices_product)
  data = data$Partner
  if(!is.null(data)) 
    v = unique(data)
  else 
    v = NULL
  return(v)
})

###############################################################################
##Trade dataset##
###############################################################################
output$indicesUIpartner = renderUI({
  data = indices_table()
  #check if the data is loaded first
  if(is.null(data)){
    return(helpText("You do not compute indices"))
  } else {
    return(selectInput("var_indices_partner","Partner code on indices tables",varnames6(),selected=varnames6()[1]));
  }
})

###########################################################################
##Selection variable reporter indice dataset
###########################################################################
varnames7 = reactive({
  data = indices_table()
  data = select(data, Product, Partner, Reporter)
  data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner)
  data = data$Reporter
  if(!is.null(data)) 
    v = unique(data)
  else 
    v = NULL
  return(v)
})

###############################################################################
##Trade dataset##
###############################################################################
output$indicesUIreporter = renderUI({
  data = indices_table()
  #check if the data is loaded first
  if(is.null(data)){
    return(helpText("You do not compute indices"))
  } else {
    return(selectInput("var_indices_reporter","Reporter code on indices tables",varnames7(),selected=varnames7()[1]));
  }
})