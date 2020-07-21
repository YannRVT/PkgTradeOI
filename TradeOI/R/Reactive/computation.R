  ## Input reactive
    observeEvent(input$goButton,{
      newtab <- switch(input$tabs,
                       "parameters" = "table_analysis",
                       "table_analysis" = "parameters")
      
      updateTabItems(session, "tabs", newtab)
      
    })

    ##On execute toutes les actions de traitement et de calcul en appelant les fonctions codees en debut de programme.
    proc_report <- reactive({
      
      progress <- Progress$new(session, min=1, max=5)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')
      
      for (i in 1:5) {
        progress$set(value = i)
        Sys.sleep(0.5)
      }

        gbl_var_key_add <<- input$var_key_add
        gbl_var_key_add <<- paste(gbl_var_key_add, collapse = ", ")
        
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
          agr_partner = input$agr_partner
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
        IE4()
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
        IE5()
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
        IE6()
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
        IE7()
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
        IE7bis()
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
          IE8()
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
        out_heterogeneous(
          input_dataset8(),
          IE8()
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
        IE9()
      )
    })
    
    IE10 <- reactive({
      is_empty_parameter1(input_dataset10())
    })
    
    ## output computed indices
    indices_table <<- reactive({
      
      progress <- Progress$new(session, min=1, max=10)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation  in progress',
                   detail = 'This may take a while...')
      
      for (i in 1:10) {
        progress$set(value = i)
        Sys.sleep(0.5)
      }
      
        unit_value_indices1(
          input_dataset10(),
          time_break = input$time_break,
          product_level=input$product_level,
          var_reporter=input$agr_reporter,
          var_partner=input$agr_partner,
          date_to_date = input$date_to_date,
          IE10()
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
          IE11()
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