#' Function to verify and create some errors during all the procedure
#'
#' @param input_dataset laste dataset
#' @param product_table table product import
#' @param revision_table table revsion import
#' @param var_productUI name of variable product
#' @param var_partnerUI name of variable partner
#' @param var_reporter name of variable reporter
#' @param var_partner name of variable partner
#' @param var_product name of variable product
#' @param var_value name of variable value
#' @param var_quantity name of variable quantity
#' @param var_quantity_unit name of variable quantity unit
#' @param var_time name of variable time
#' @param var_product_PDT name of variable product in the product table
#' @param var_revision_PDT name of variable revision in the revision table
#' @param var_key_add variable add
#' @param time_breakdown the chosen date (end)
#' @param date_range the chosen date
#' @param indices_type indices choose
#' @param agr_partner aggregate or not on partner
#' @param year_to_date year to date or not
#' @param checkbox1 compute "unstable HS revision
#' @param checkbox2 compute "delete heterogenious time series
#' @param checkbox3 compute "indice"
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
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export
#'

test_parameters1 <- compiler::cmpfun(test_parameters <- function(
  input_dataset,
  product_table,
  revision_table,
  var_productUI,
  var_partnerUI,
  var_reporter,
  var_partner,
  var_product,
  var_value,
  var_quantity,
  var_quantity_unit,
  var_time,
  var_product_PDT,
  var_revision_PDT,
  var_key_add,
  time_breakdown,
  date_range,
  indices_type,
  agr_partner,
  year_to_date,
  checkbox1,
  checkbox2,
  checkbox3
)
{

  print("Staring computation")

  error <- 0
  error_time <- 0

  start_time <- as.character(Sys.time())
  log <- data.table(step = "Started at", note = start_time, type="info" , stringsAsFactors = F)

  if ((checkbox1 == FALSE) && (checkbox2 == FALSE) && (checkbox3 == FALSE)){
    shinyalert("Warning!", "You cannot choose nothing to compute", type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("Choose on action to compute in the parameters section"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (is.null(input_dataset)){
    shinyalert("Warning!", "You need to import the trade Dataset", type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You miss the trade dataset in the data preparation section"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (is.null(product_table)){
    shinyalert("Warning!", "You need to import the product table", type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You miss the product table in the data preparation section"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (is.null(revision_table)){
    shinyalert("Warning!", "You need to import the revision table", type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You miss the revision table in the data preparation section"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  ## Est ce que la table de base est trop grosse ?
  if(R.Version()$os == "mingw32")
  {
    if(object.size(input_dataset)>as.numeric(system("wmic OS get freephysicalmemory", intern=TRUE)[2])*1024^10){
      shinyalert("Warning!", "Input table too big for the free space on your disk" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step="Error Input size", note = paste("Input table too big for the free space on your disk."),type="error", stringsAsFactors = F)
      log <- rbind(log,log1)
      error <- error+1
    }
  }

  # Est ce que les noms de variables entrees sont vraiment dans les jeux de donnees  #InputDataSet #ProductTable
  # InputdataSet
  if(!exists(var_reporter,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_reporter,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_reporter,"is missing in the trade input dataset."), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  if(!exists(var_partner,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_partner,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_partner,"is missing in the trade input dataset."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }
  if(!exists(var_product,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_product,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_product,"is missing in the trade input dataset."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }
  if(!exists(var_value,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_value,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_value,"is missing in the trade input dataset."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }
  if(!exists(var_quantity,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_quantity,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_quantity,"is missing in the trade input dataset."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }
  if(!exists(var_quantity_unit,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_quantity_unit,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_quantity_unit,"is missing in the trade input dataset."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }
  if(!exists(var_time,where=input_dataset,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_time,"is missing in the trade input dataset" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",var_time,"is missing in the trade input dataset."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  #Est-ce que les variables supplementaires pour la clef primaire sont bien dans la table InputDataSet
  liste_var<-unlist(strsplit(gsub(" ","",gbl_var_key_add,fixed=TRUE), split=","))
  for (current_var in liste_var){
    if(!exists(current_var,where=input_dataset,inherits=TRUE)){
      shinyalert("Warning!", "The variable",current_var,"is missing in the input input dataset" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Error in input trade dataset", note = paste("The variable",current_var,"is missing in the input input dataset"), type="error" , stringsAsFactors = F)
      log <- rbind(log,log1)
      error <- error+1
    }
  }

  # Est ce que #ProductTable et #RevisionTable ont le bon format
  # ProductTable
  if(!is.null(product_table)){
  if(!exists(var_product_PDT,where=product_table,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_product_PDT,"is missing in the input product table" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in product table", note = paste("The variable REVISION is missing in the input product table."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  if(!exists(var_revision_PDT,where=product_table,inherits=TRUE)){
    shinyalert("Warning!", "The variable",var_revision_PDT,"is missing in the input product table" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in product table", note = paste("The variable REVISION in your product table is missing in the input product table."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }
}

  #RevisionTable
  if(!is.null(revision_table)){
  if(!exists("REPORTER_CD",where=revision_table,inherits=TRUE)){
    shinyalert("Warning!", "The variable 'REPORTER_CD' is missing in the revision table" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in revision table", note = "The variable 'REPORTER_CD' is missing in the revision table.", type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  if(!exists("year",where=revision_table,inherits=TRUE)){
    shinyalert("Warning!", "The variable 'year' is missing in the revision table" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in revision table", note = "The variable 'year' is missing in the revision table.", type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  if(!exists("class",where=revision_table,inherits=TRUE)){
    shinyalert("Warning!", "The variable 'class' is missing in the revision table" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in revision table", note = "The variable 'class' is missing in the revision table.", type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  # Est ce que toutes les revisions demandees dans revision_table sont presentes dnas product_table ?
  if(exists("class",where=revision_table,inherits=TRUE)&exists(var_revision_PDT,where=product_table,inherits=TRUE)){
    max_data_rev <- max(substr(revision_table$class,2,2))
    max_prod_rev <- max(str_length(product_table[,names(product_table) == var_revision_PDT]))
    if(max_data_rev>max_prod_rev){
      shinyalert("Warning!", "The 5th revision is missing in Product table's flag, thus, 4th revision will be repited for next steps" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Warning in product table", note = "The 5th revision is missing in Product table's flag, thus, 4th revision will be repited for next steps", type="warning" , stringsAsFactors = F)
      log <- rbind(log,log1)
    }
  }
}

  # Est ce que dans #inputDataSet la variable de temps est au bon format
  test1 <- data.table(v1 = "Empty" , stringsAsFactors = F)
  test1 <- input_dataset[,var_time]
  if (nchar(test1[1])==6){
    T_break <- "monthly"
    print("monthly")
    min <- min(test1)
    year_min <- substr(min,1,4)
    period_min <- substr(min,5,6)
    max <- max(test1)
    year_max <- substr(max,1,4)
    period_max <- substr(max,5,6)
  }
  else if(nchar(test1[1])==5){
    T_break <- "quarterly"
    print("quarterly")
    min <- min(test1)
    year_min <- substr(min,1,4)
    period_min <- substr(min,5,5)
    max <- max(test1)
    year_max <- substr(max,1,4)
    period_max <- substr(max,5,5)
  }
  else if(nchar(test1[1])==4){
    T_break <- "yearly"
    print("yearly")
    min <- min(test1)
    year_min <- substr(min,1,4)
    period_min <- 1
    max <- max(test1)
    year_max <- substr(max,1,4)
    period_max <- 1
  }
  else{
    shinyalert("Warning!", "Please make sure the time variable follows the following format: YYYY for yearly data, YYYYQ for quarterly data, YYYYMM for monthly data" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error in input trade dataset", note = "Please make sure the time variable follows the following format: YYYY for yearly data, YYYYQ for quarterly data, YYYYMM for monthly data.", type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    error_time <- 1
  }

  # Lien logique entre les variables
  year_inf <- substr(as.character(str_split(date_range," ")[1]),1,4)
  month_inf <- substr(as.character(str_split(date_range," ")[1]),6,7)
  year_sup <- substr(as.character(str_split(date_range," ")[2]),1,4)
  month_sup <- substr(as.character(str_split(date_range," ")[2]),6,7)

  if((year_inf>year_sup) | (year_inf==year_sup & month_inf>month_sup)){
    shinyalert("Warning!", "The 'from' date must be smaller than the 'to' date in date range selection." , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = "The 'from' date must be smaller than the 'to' date in date range selection.", type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    error_time <- 1
  }

  if (error_time == 0){
    if (T_break =="yearly"){
      period_inf <- 1
      period_sup <- 1
    }
    else if(T_break =="quarterly"){
      if(month_inf %in% c("01","02","03")){
        period_inf <- 1
      }
      else if(month_inf %in% c("04","05","06")){
        period_inf <- 2
      }
      else if(month_inf %in% c("04","08","09")){
        period_inf <- 3
      }
      else if (month_inf %in% c("10","11","12")){
        period_inf <- 4
      }
      if (month_sup %in% c("01","02","03")){
        period_sup <- 1
      }
      else if (month_sup %in% c("04","05","06")){
        period_sup <- 2
      }
      else if (month_sup %in% c("04","08","09")){
        period_sup <- 3
      }
      else if (month_sup %in% c("10","11","12")){
        period_sup <- 4
      }
    }
    else if (T_break =="monthly"){
      period_inf <- month_inf
      period_sup <- month_sup
    }

    # Si des annees sont dans input Ds et pas dans revision
    min_rt <- min(revision_table$year)
    max_rt <- max(revision_table$year)
    min_g <- max(year_min,year_inf)
    max_g <- min(year_max,year_sup)

    if (min_rt>min_g | max_rt<max_g){
      shinyalert("Warning!", "Revision Table is missing one (or more) year(s) which can be found in the input trade dataset" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Error in revision table", note = "Revision Table is missing one (or more) year(s) which can be found in the input trade dataset.", type="error" , stringsAsFactors = F)
      log2 <- rbind(log,log1)
      log <- log2
      error <- error+1
    }

    # var_time et date_range
    if((year_inf < year_min)|
       (year_sup > year_max)|
       (year_inf == year_min & period_inf < period_min)|
       (year_sup == year_max & period_sup > period_max)){
      shinyalert("Warning!", "The date range to analyse is larger than the one found in your input dataset" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Error", note = "The date range to analyse is larger than the one found in your input dataset.", type="error" , stringsAsFactors = F)
      log <- rbind(log,log1)
      error <- error+1
    }

    # Time_breakdown et data_range
    if(time_breakdown == "month" & T_break !="monthly"){
      shinyalert("Warning!", "Monthly indices. Please provide monthly data to compute monthly indices" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Error", note = "Please provide monthly data to compute monthly indices.", type="error" , stringsAsFactors = F)
      log <- rbind(log,log1)
      error <- error+1
    }
    else if(time_breakdown == "quarter" & T_break == "yearly"){
      shinyalert("Warning!", "Quarterly indices. Please provide monthly or quarterly data to compute quarterly indices" , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Error", note = "Please provide monthly or quarterly data to compute quarterly indices.", type="error" , stringsAsFactors = F)
      log <- rbind(log,log1)
      error <- error+1
    }

    ## Savoir si les dates limites de temps demandes par l'utilisateur correspondes a des dates de debut/fin de trimstres/d'annees
    if(time_breakdown == "quarter" & T_break == "monthly"){
      if(period_inf != "01" & period_inf != "04" & period_inf != "07" & period_inf != "10"){
        shinyalert("Warning!", "Quarterly indices. The date range to analyse must start with the first month of a quarter" , type = "error", closeOnClickOutside = TRUE)

        log1 <- data.table(step = "Error", note = "You asked for quarterly indices. Consequently the date range to analyse must start with the first month of a quarter.", type="error" , stringsAsFactors = F)
        log <- rbind(log,log1)
        error <- error+1
      }
      if(period_sup != "03" & period_sup != "06" & period_sup != "09" & period_sup != "12"){
        shinyalert("Warning!", "Quarterly indices. The date range to analyse must end with the last month of a quarter" , type = "error", closeOnClickOutside = TRUE)

        log1 <- data.table(step = "Error", note = "You asked for quarterly indices. Consequently the date range to analyse must end with the last month of a quarter.", type="error" , stringsAsFactors = F)
        log <- rbind(log,log1)
        error <- error+1
      }
    }

    if(time_breakdown == "year" & T_break != "yearly"){
      if (T_break == "monthly"){
        if(period_inf != "01"){
          shinyalert("Warning!", "Yearly indices. The date range to analyse must start with January" , type = "error", closeOnClickOutside = TRUE)

          log1 <- data.table(step = "Error", note = "You asked for yearly indices. Consequently the date range to analyse must start with January.", type="error" , stringsAsFactors = F)
          log <- rbind(log,log1)
          error <- error+1
        }
        if(period_sup != "12"){
          shinyalert("Warning!", "Yearly indices. The date range to analyse must end with December" , type = "error", closeOnClickOutside = TRUE)

          log1 <- data.table(step = "Error", note = "You asked for yearly indices. Consequently the date range to analyse must end with December.", type="error" , stringsAsFactors = F)
          log <- rbind(log,log1)
          error <- error+1
        }
      }
      else if (T_break == "quarterly"){
        if(period_inf != 1){
          shinyalert("Warning!", "Quarterly indices. The date range to analyse must start with January (1st quarter)" , type = "error", closeOnClickOutside = TRUE)

          log1 <- data.table(step = "Error", note = "You asked for quarterly indices. Consequently the date range to analyse must start with January (1st quarter).", type="error" , stringsAsFactors = F)
          log <- rbind(log,log1)
          error <- error+1
        }
        if(period_sup != 4){
          shinyalert("Warning!", "Quarterly indices. The date range to analyse must end with December (4th quarter)" , type = "error", closeOnClickOutside = TRUE)

          log1 <- data.table(step = "Error", note = "You asked for quarterly indices. Consequently the date range to analyse must end with December (4th quarter).", type="error" , stringsAsFactors = F)
          log <- rbind(log,log1)
          error <- error+1
        }
      }
    }

    # Year_to_date et yearly data
    if (time_breakdown == "year" & year_to_date=="TRUE"){
      shinyalert("Warning!", "Do not tick Year-to-date while asking for yearly indices." , type = "error", closeOnClickOutside = TRUE)

      log1 <- data.table(step = "Error", note = "To produce meaningful results, do not tick Year-to-date while asking for yearly indices.", type="error" , stringsAsFactors = F)
      log <- rbind(log,log1)
      error <- error+1
    }
  }

  if (error != 0){
    log1 <- data.table(step = "Error", note = paste(error, "error(s) has/have been detected in you datasets."), type="error" , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
  }

  if (var_reporter == var_partner | var_reporter == var_product | var_reporter == var_value | var_reporter == var_quantity | var_reporter == var_quantity_unit | var_reporter == var_time){
    shinyalert("Warning!", "Do not use variables twice" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You use some variables twice"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (var_partner == var_product | var_partner == var_value | var_partner == var_quantity | var_partner == var_quantity_unit | var_partner == var_time){
    shinyalert("Warning!", "Do not use variables twice" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You use some variables twice"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (var_product == var_value | var_product == var_quantity | var_product == var_quantity_unit | var_product == var_time){
    shinyalert("Warning!", "Do not use variables twice" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You use some variables twice"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (var_value == var_quantity | var_value == var_quantity_unit | var_value == var_time){
    shinyalert("Warning!", "Do not use variables twice" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You use some variables twice"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if (var_quantity == var_quantity_unit | var_quantity == var_time){
    shinyalert("Warning!", "Do not use variables twice" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You use some variables twice"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if(var_quantity_unit == var_time){
    shinyalert("Warning!", "Do not use variables twice" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You use some variables twice"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  if(!is.null(var_partnerUI) & agr_partner == TRUE){
    shinyalert("Warning!", "You can not choose a specific partner and check agregated on partner variable" , type = "error", closeOnClickOutside = TRUE)

    log1 <- data.table(step = "Error", note = paste("You can not choose a specific partner and check agregated on partner variable"), type="error"  , stringsAsFactors = F)
    log <- rbind(log,log1)
    error <- error+1
    return(log)
  }

  rm(input_dataset, product_table, revision_table)
  gc()

  print("Fin TEST_PARAMETERS")
  return(log)
}
)
