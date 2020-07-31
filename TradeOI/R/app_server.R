#' Function server to run TradeOi app
#'
#' Some options are programmed
#' Suppress missing test error
#' Maximum importable file size
#' Feature in parallel
#'
#' @param input file in
#' @param output file create
#' @param session for reactive and observe
#'
#' @import filehash
#' @import sqldf
#' @import stringr
#' @import data.table
#' @import robustbase
#' @import shiny
#' @import shinydashboard
#' @import doParallel
#' @import gsubfn
#' @import proto
#' @import RSQLite
#' @import iterators
#' @import shinycssloaders
#' @import shinyjs
#' @import dplyr
#' @import optiRum
#' @import compiler
#' @import shinyalert
#' @import DT
#' @import stringi
#' @import highcharter
#' @import parallel
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export

#SERVER
app_server = function(input, output, session) {

  memory.limit(100000)

  no_cores = detectCores() - 1
  cl = makeCluster(no_cores, setup_strategy = "sequential")
  registerDoParallel(cl)
  
  ##########################################################################################
  #Server data preparation
  ##########################################################################################

  #reactive object, responsible for loading the main data
  ####################################################
  ##On importe la table des produits
  rawInputData = reactive({
    rawData = input$myData
    headerTag = input$headerUI1;
    sepTag = input$sepUI1;

    if(!is.null(rawData)) {
      myData = fread(rawData$datapath, sep = sepTag, stringsAsFactors=FALSE, header = headerTag, na.strings=".", dec=".", colClasses = "character")
      return(as.data.frame(myData))
    } else {
      return(NULL);
    }
  });

  ##On importe la table des produits
  rawInputData2 = reactive({
    rawData = input$product_table
    headerTag = input$headerUI2;
    sepTag = input$sepUI2;

    if(!is.null(rawData)) {
      product_table = fread(rawData$datapath, header=headerTag, sep=sepTag, na.strings=".", dec=".", stringsAsFactors=FALSE, colClasses= "character")
      return(product_table)
    } else {
      return(NULL);
    }
  });

  ##On importe la table des revisions
  rawInputData3 = reactive({
    rawData = input$revision_table
    headerTag = input$headerUI3;
    sepTag = input$sepUI3;
    
    if(!is.null(rawData)) {
      revision_table = fread(rawData$datapath, header=headerTag, sep=sepTag, na.strings=".", dec=".", stringsAsFactors=FALSE, colClasses= "character")
      return(revision_table)
    } else {
      return(NULL);
    }
  })

  
  ##########################################################################################
  #Variable name
  ##########################################################################################

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

  
  ##########################################################################################
  #Input table
  ##########################################################################################

  output$input_trade_table <- DT::renderDT(datatable(rawInputData()))

  output$input_product_table <- DT::renderDT(datatable(rawInputData2()))

  output$input_revision_table <- DT::renderDT(datatable(rawInputData3()))

  
  ##########################################################################################
  #Computation
  ##########################################################################################

  ## Input reactive

      observeEvent(input$goButton,{
        newtab <- switch(input$tabs,
                         "parameters" = "table_analysis",
                         "table_analysis" = "parameters")

        updateTabItems(session, "tabs", newtab)

      })

      ##On execute toutes les actions de traitement et de calcul en appelant les fonctions codees en debut de programme.
      proc_report <- reactive({

        gbl_var_key_add <<- paste(input$var_key_add, collapse = ", ")

        test_parameters1(
          input_dataset = rawInputData(),
          product_table = rawInputData2(),
          revision_table = rawInputData3(),
          var_productUI = input$var_key_add2,
          var_partnerUI = input$var_key_add3,
          var_reporter = input$var_reporter,
          var_partner = input$var_partner,
          var_product = input$var_product,
          var_value = input$var_value,
          var_quantity = input$var_quantity,
          var_quantity_unit = input$var_quantity_unit,
          var_time = input$var_time,
          var_product_PDT = input$var_product_PDT,
          var_revision_PDT = input$var_revision_PDT,
          time_breakdown = input$time_break,
          date_range = input$dateRange2,
          indices_type = input$indices_bc,
          year_to_date = input$date_to_date,
          var_key_add = input$var_key_add,
          agr_partner = input$agr_partner,
          checkbox1 = input$checkbox1,
          checkbox2 = input$checkbox2,
          checkbox3 = input$checkbox3
        )
      })

        input_error <- reactive({
        are_error_in_DS(proc_report())
      })

      input_dataset2 <- reactive({
          change_names1(
            input_dataset = rawInputData(),
            product_table = rawInputData2(),
            var_productUI = input$var_key_add2,
            var_partnerUI = input$var_key_add3,
            var_reporter = input$var_reporter,
            var_partner = input$var_partner,
            var_product =input$var_product,
            var_value = input$var_value,
            var_quantity = input$var_quantity,
            var_quantity_unit = input$var_quantity_unit,
            var_time = input$var_time,
            var_product_PDT = input$var_product_PDT,
            var_revision_PDT = input$var_revisison_PDT,
            date_range = input$dateRange2,
            input_error()
          )
      })


      ## Verification que la table 2 n'est pas vide
      IE2 <- reactive({
        is_empty_parameter1(input_dataset2())
      })

      proc_report1 <- reactive({
        is_empty_output1(
          proc_report(),
          note_txt="input datasets",
          IE2(),
          input_error(),
          input_error()
        )
      })

      ## Teste que tous les produits sont bien des codes HS
      nonHS_free <- reactive({
        verif_HS1(
          input_dataset2(),
          rawInputData2(),
          IE2()
        )
      })

      IE2bis <- reactive({
        is_empty_parameter1(nonHS_free())
      })

      proc_report1bis <- reactive({
        is_empty_output1(
          proc_report1(),
          note_txt="Product code belonging to HS",
          IE2bis(),
          IE2(),
          input_error()
        )
      })

      ## Count difference bf & af HS verification
      proc_report1ter <- reactive({
        non_HS1(
          input_dataset2(),
          nonHS_free(),
          proc_report1bis(),
          IE2bis()
        )
      })

      ## suppression des doublons
      input_dataset3 <- reactive({
        doublons1(
          nonHS_free(),
          IE2bis()
        )
      })

      IE3 <- reactive({
        is_empty_parameter1(input_dataset3())
      })

      ## Homogeneite des units de quantite
      input_dataset4 <- reactive({
        unit_quant1(
          input_dataset3(),
          IE3()
        )
      })

      ## Verification que la table 4 n'est pas vide
      IE4 <- reactive({
        is_empty_parameter1(input_dataset4())
      })

      proc_report2 <- reactive({
        is_empty_output1(
          proc_report1ter(),
          note_txt="quantity unit homogeneity",
          IE4(),
          IE3(),
          input_error()
        )
      })

      ## Changement de revision
      input_dataset5 <- reactive({
        chang_rev1(
          input_dataset4(),
          rawInputData3(),
          IE4(),
          checkbox1 = input$checkbox1
        )
      })

      ## Verification que la table 5 n'est pas vide
      IE5 <- reactive({
        is_empty_parameter1(input_dataset5())
      })
      proc_report3 <- reactive({
        is_empty_output1(
          proc_report2(),
          note_txt="HS revisions",
          IE5(),
          IE4(),
          input_error()
        )
      })

      ## encode time variable
      input_dataset6 <- reactive({
        time_code1(
          input_dataset5(),
          as.numeric(input$pct_missdata),
          IE5(),
          checkbox1 = input$checkbox1
        )
      })

      ## Verification que la table 6 n'est pas vide
      IE6 <- reactive({
        is_empty_parameter1(input_dataset6())
      })
      proc_report4 <- reactive({
        is_empty_output1(
          proc_report3(),
          note_txt="number of missing data",
          IE6(),
          IE5(),
          input_error()
        )
      })

      ## outliers detection
      input_dataset7 <- reactive({
        outliers_detection1(
          input_dataset6(),
          var_time = input$var_time,
          IE6(),
          radio_outliers = input$radio_outliers
        )
      })

      ## Verification que la table 7 n'est pas vide
      IE7 <- reactive({
        is_empty_parameter1(input_dataset7())
      })
      proc_report5 <- reactive({
        is_empty_output1(
          proc_report4(),
          note_txt="outliers",
          IE7(),
          IE6(),
          input_error()
        )
      })

      ## Replacement
      input_dataset7bis <- reactive({
        replacement1(
          input_dataset7(),
          IE7(),
          radio_outliers = input$radio_outliers
        )
      })

      ## Verification que la table 7bis n'est pas vide
      IE7bis <- reactive({
        is_empty_parameter1(input_dataset7bis())
      })
      proc_report5bis <- reactive({
        is_empty_output1(
          proc_report5(),
          note_txt="replacement",
          IE7(),
          IE7bis(),
          input_error()
        )
      })

      ## heterogeneity
      input_dataset8 <- reactive({
        heterogeneity1(
          input_dataset7bis(),
          IE7bis(),
          checkbox2 = input$checkbox2,
          radio_outliers = input$radio_outliers

        )
      })

      ## Verification que la table 8 n'est pas vide
      IE8 <- reactive({
        is_empty_parameter1(input_dataset8())
      })
      proc_report6 <- reactive({
        is_empty_output1(
          proc_report5bis(),
          note_txt="time series heterogeneity",
          IE8(),
          IE7bis(),
          input_error()
        )
      })

      ## output table with detected outliers
      ## On passe des "isolate" pour ne pas reexecuter a tout bout de champ
      detected_outliers <<- reactive({
          out_outliers1(
            input_dataset7(),
            input_dataset8(),
            input_dataset7bis(),
            IE8(),
            radio_outliers = input$radio_outliers
          )
      })

      ## trie les series homogenes
      input_dataset9 <- reactive({
        non_heterogeneous1(
          input_dataset8(),
          IE8()
        )
      })

      ## output series heterogenes
      series_hetero <<- reactive({
          out_heterogeneous1(
            input_dataset8(),
            IE8(),
            checkbox2 = input$checkbox2
          )
      })

      IE9 <- reactive({
        is_empty_parameter1(input_dataset9())
      })

      ## compute indices
      input_dataset10 <- reactive({
        prepare_indices1(
          input_dataset9(),
          time_break = input$time_break,
          indices_bc = input$indices_bc,
          date_to_date = input$date_to_date,
          date_range = input$dateRange2,
          IE9(),
          checkbox3 = input$checkbox3
        )
      })

      IE10 <- reactive({
        is_empty_parameter1(input_dataset10())
      })

      ## output computed indices
      indices_table <<- reactive({

          unit_value_indices1(
            input_dataset10(),
            time_break = input$time_break,
            product_level=input$product_level,
            var_reporter=input$agr_reporter,
            var_partner=input$agr_partner,
            date_to_date = input$date_to_date,
            IE10(),
            checkbox3 = input$checkbox3
          )
      })

      ## Verification table complete
      IE11 <- reactive({
        is_empty_parameter1(indices_table())
      })

      proc_report7 <- reactive({
        is_empty_output1(
          proc_report6(),
          note_txt="unit value indices",
          IE11(),
          IE10(),
          input_error()
        )
      })

      ## output cover rate table
      cover_rate <<- reactive({
          covert_rate1(
            original_ds=input_dataset2(),
            nonHS_free(),
            doublons_ds=input_dataset3(),
            qty_unit_ds=input_dataset4(),
            chmt_rev_ds=input_dataset5(),
            incompl_series_ds=input_dataset6(),
            outliers_ds=input_dataset8(),
            non_homogeneous_ds=input_dataset9(),
            indices_ds=input_dataset10(),
            detected_outliers=detected_outliers(),
            series_hetero=series_hetero(),
            IE11(),
            checkbox1 = input$checkbox1,
            checkbox2 = input$checkbox2,
            checkbox3 = input$checkbox3,
            radio_outliers = input$radio_outliers
          )
      })

      IE12 <- reactive({
        is_empty_parameter1(cover_rate())
      })

      proc_report9 <<- reactive({
        is_empty_output1(
          proc_report7(),
          note_txt="cover rate",
          IE12(),
          IE11(),
          input_error()
        )
      })

  observeEvent(input$RIQ, {
        # Show a modal when the button is pressed
    shinyalert("RIQ", "Relative Interquartile coefficient", type = "info")
    })

  observeEvent(input$RSD, {
        # Show a modal when the button is pressed
    shinyalert("RSD", "Relative Standart Deviation", type = "info")
    })

  observeEvent(input$Revision, {
    # Show a modal when the button is pressed
    shinyalert("Revision", "In each revision (every 5 years), product codes may be created, removed, reallocated. If the code has not been modified 1 is applied, if the code has been modified 2 is applied, otherwise 0 is applied. ", type = "info")
  })

  
  ##########################################################################################
  #Result
  ##########################################################################################

  ## adding ended date on log
  output$proc_report <- DT::renderDT(datatable(proc_report9()))

  output$downloadPR <- downloadHandler(
    filename = paste('proc_report', Sys.Date(),'.csv'),
    content = function(file) {
      write.csv(proc_report9(),file)
    })
  ###########

  output$cover_rate <- DT::renderDT(datatable(cover_rate()))

  output$downloadCR <- downloadHandler(
    filename = paste('cover_rate_', Sys.Date(),'.csv'),
    content = function(file) {
      write.csv(cover_rate(),file)
    })
  ###########

  output$detected_outliers <- DT::renderDT(datatable(detected_outliers()))

  output$downloadOL <- downloadHandler(
    filename = paste('detected_outliers', Sys.Date(),'.csv'),
    content = function(file) {
      write.csv(detected_outliers(),file)
    })
  ###########

  output$series_hetero <- DT::renderDT(datatable(series_hetero()))

  output$downloadHS <- downloadHandler(
    filename = paste('heteorgeneous_series_', Sys.Date(),'.csv'),
    content = function(file) {
      write.csv(series_hetero(),file)
    })
  ###########

  output$indices_table <- DT::renderDT(datatable(indices_table()))

  output$downloadIT <- downloadHandler(
    filename = paste('trade_indices_', Sys.Date(),'.csv'),
    content = function(file) {
      write.csv(indices_table(),file)
    })

  
  ##########################################################################################
  #Graph_output
  ##########################################################################################

  output$plot_product <- renderHighchart({
    data = indices_table()

    if(!is.null(data) & input$time_break == "year"){

      data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
      
      data$Month = "12"
      data$Day = "15"
      
      data$Date <-paste(data$Year,data$Month, data$Day,sep="-")
      data$Date <- as.Date(data$Date, format="%Y-%m-%d")
      
      data = select(data, -c(Year, Month, Day))
      
      dataLP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataLP$Indices = "Laspeyres price"
      dataLP = rename(dataLP, Value = "Laspeyres price")
      
      dataLQ = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataLQ$Indices = "Laspeyres quantity"
      dataLQ = rename(dataLQ, Value = "Laspeyres quantity")
      
      dataPP = select(data, -c("Laspeyres quantity", "Laspeyres price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataPP$Indices = "Paasche price"
      dataPP = rename(dataPP, Value = "Paasche price")
      
      dataPQ = select(data, -c("Laspeyres price", "Paasche price", "Laspeyres quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataPQ$Indices = "Paasche quantity"
      dataPQ = rename(dataPQ, Value = "Paasche quantity")
      
      dataFP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Laspeyres price", "Index value"))
      dataFP$Indices = "Fisher price"
      dataFP = rename(dataFP, Value = "Fisher price")
      
      dataFQ = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Laspeyres price", "Fisher price", "Index value"))
      dataFQ$Indices = "Fisher quantity"
      dataFQ = rename(dataFQ, Value = "Fisher quantity")
      
      dataIV = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Laspeyres quantity", "Fisher price", "Fisher quantity"))
      dataIV$Indices = "Index value"
      dataIV = rename(dataIV, Value = "Index value")
      
      data = rbind(dataFP, dataFQ, dataPP, dataPQ, dataLP, dataLQ, dataIV)
      
      hchart(data, "line", hcaes(x = Date, y = c(Value), group = Indices)) %>%
      hc_exporting(
        enabled = TRUE
      )
      
    } else if(!is.null(data) & input$time_break == "quarter"){
      
      data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
      
      data$Quarter = as.character(data$Quarter)
      data$Quarter =ifelse(data$Quarter == "1", "03", data$Quarter)
      data$Quarter =ifelse(data$Quarter == "2", "06", data$Quarter)
      data$Quarter =ifelse(data$Quarter == "3", "09", data$Quarter)
      data$Quarter =ifelse(data$Quarter == "4", "12", data$Quarter)
      data$Day = "15"
      
      data$Date <-paste(data$Year,data$Quarter, data$Day,sep="-")
      data$Date <- as.Date(data$Date, format="%Y-%m-%d")
      
      data = select(data, -c(Year, Quarter, Day))
      
      dataLP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataLP$Indices = "Laspeyres price"
      dataLP = rename(dataLP, Value = "Laspeyres price")
      
      dataLQ = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataLQ$Indices = "Laspeyres quantity"
      dataLQ = rename(dataLQ, Value = "Laspeyres quantity")
      
      dataPP = select(data, -c("Laspeyres quantity", "Laspeyres price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataPP$Indices = "Paasche price"
      dataPP = rename(dataPP, Value = "Paasche price")
      
      dataPQ = select(data, -c("Laspeyres price", "Paasche price", "Laspeyres quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataPQ$Indices = "Paasche quantity"
      dataPQ = rename(dataPQ, Value = "Paasche quantity")
      
      dataFP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Laspeyres price", "Index value"))
      dataFP$Indices = "Fisher price"
      dataFP = rename(dataFP, Value = "Fisher price")
      
      dataFQ = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Laspeyres price", "Fisher price", "Index value"))
      dataFQ$Indices = "Fisher quantity"
      dataFQ = rename(dataFQ, Value = "Fisher quantity")
      
      dataIV = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Laspeyres quantity", "Fisher price", "Fisher quantity"))
      dataIV$Indices = "Index value"
      dataIV = rename(dataIV, Value = "Index value")
      
      data = rbind(dataFP, dataFQ, dataPP, dataPQ, dataLP, dataLQ, dataIV)
      
      hchart(data, "line", hcaes(x = Date, y = c(Value), group = Indices))%>%
        hc_exporting(
          enabled = TRUE
        )
      
    } else if(!is.null(data) & input$time_break == "month"){
      
      data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
      
      data$Month = as.character(data$Month)
      data$Reporter = as.character(data$Reporter)
      data$Partner = as.character(data$Partner)
      data$Product = as.character(data$Product)
      
      data$Month =ifelse(data$Month == "1", "01", data$Month)
      data$Month =ifelse(data$Month == "2", "02", data$Month)
      data$Month =ifelse(data$Month == "3", "03", data$Month)
      data$Month =ifelse(data$Month == "4", "04", data$Month)
      data$Month =ifelse(data$Month == "5", "05", data$Month)
      data$Month =ifelse(data$Month == "6", "06", data$Month)
      data$Month =ifelse(data$Month == "7", "07", data$Month)
      data$Month =ifelse(data$Month == "8", "08", data$Month)
      data$Month =ifelse(data$Month == "9", "09", data$Month)
      data$Day = "15"
      
      data$Date <-paste(data$Year,data$Month, data$Day,sep="-")
      data$Date <- as.Date(data$Date, format="%Y-%m-%d")
      
      data = select(data, -c(Year, Month, Day))
      
      dataLP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataLP$Indices = "Laspeyres price"
      dataLP = rename(dataLP, Value = "Laspeyres price")
      
      dataLQ = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataLQ$Indices = "Laspeyres quantity"
      dataLQ = rename(dataLQ, Value = "Laspeyres quantity")
      
      dataPP = select(data, -c("Laspeyres quantity", "Laspeyres price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataPP$Indices = "Paasche price"
      dataPP = rename(dataPP, Value = "Paasche price")
      
      dataPQ = select(data, -c("Laspeyres price", "Paasche price", "Laspeyres quantity", "Fisher quantity", "Fisher price", "Index value"))
      dataPQ$Indices = "Paasche quantity"
      dataPQ = rename(dataPQ, Value = "Paasche quantity")
      
      dataFP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Laspeyres price", "Index value"))
      dataFP$Indices = "Fisher price"
      dataFP = rename(dataFP, Value = "Fisher price")
      
      dataFQ = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Laspeyres price", "Fisher price", "Index value"))
      dataFQ$Indices = "Fisher quantity"
      dataFQ = rename(dataFQ, Value = "Fisher quantity")
      
      dataIV = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Laspeyres quantity", "Fisher price", "Fisher quantity"))
      dataIV$Indices = "Index value"
      dataIV = rename(dataIV, Value = "Index value")
      
      data = rbind(dataFP, dataFQ, dataPP, dataPQ, dataLP, dataLQ, dataIV)
      
      hchart(data, "line", hcaes(x = Date, y = c(Value), group = Indices))%>%
        hc_exporting(
          enabled = TRUE
        )
      }
    })

  output$table_product <- renderDataTable({
    data = indices_table()
    data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
    data
  })

  
  ##########################################################################################
  #Server data preparation
  ##########################################################################################

  stopCluster(cl)

}
