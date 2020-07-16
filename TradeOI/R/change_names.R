#' Function to check and change the names
#'
#' @param input_dataset last dataset
#' @param product_table product dataset
#' @param var_productUI variable
#' @param var_partnerUI variable
#' @param var_reporter variable for reporter
#' @param var_partner variable for partner
#' @param var_product variable for product
#' @param var_value variable for value
#' @param var_quantity variable for quantity
#' @param var_quantity_unit variable for quantity unit
#' @param var_time variable for time
#' @param var_product_PDT variable for product in product table
#' @param var_revision_PDT variable for revision in product table
#' @param date_range the chosen date
#' @param input_error error dataset
#'
#' @import filehash
#' @import sqldf
#' @import stringr
#' @import data.table
#' @import robustbase
#' @import gsubfn
#' @import proto
#' @import RSQLite
#' @import iterators
#' @import dplyr
#' @import optiRum
#' @import compiler
#' @import shinyalert
#' @import DT
#' @import stringi
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export

change_names1 <- compiler::cmpfun(change_names <- function(
  input_dataset,
  product_table,
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
  date_range,
  input_error){

  print("Staring change_names")

  if(input_error == 1){

    input_dataset <- data.table(NA)

  } else if(!is.null(var_productUI) & is.null(var_partnerUI)){

    names(input_dataset)[names(input_dataset) == var_reporter] <- "REPORTER_CD"
    names(input_dataset)[names(input_dataset) == var_partner] <- "PARTNER_CD"
    names(input_dataset)[names(input_dataset) == var_product] <- "PRODUCT_CD"
    names(input_dataset)[names(input_dataset) == var_value] <- "VALUE"
    names(input_dataset)[names(input_dataset) == var_quantity] <- "QUANTITY"
    names(input_dataset)[names(input_dataset) == var_quantity_unit] <- "QUANTITY_UNIT_CD"
    names(input_dataset)[names(input_dataset) == var_time] <- "MONTH_CD"

    input_dataset = filter(input_dataset, PRODUCT_CD == var_productUI)

    #On convertit VALUE et QUANTITY en numerique
    input_dataset$QUANTITY<-as.numeric(input_dataset$QUANTITY)
    input_dataset$VALUE<-as.numeric(input_dataset$VALUE)*1000

    #### Si l'input DS a des donnees mensuelles
    year_inf <- substr(as.character(str_split(date_range," ")[1]),1,4)
    month_inf <- substr(as.character(str_split(date_range," ")[1]),6,7)

    year_sup <- substr(as.character(str_split(date_range," ")[2]),1,4)
    month_sup <- substr(as.character(str_split(date_range," ")[2]),6,7)

    ## Si on a des donnees mensuelles
    if (nchar(input_dataset$MONTH_CD)[1]==6){
      date_inf <- str_c(year_inf,month_inf)
      date_sup <- str_c(year_sup,month_sup)
      input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
    }
    ## Si on a des donnees trimestielles
    else if(nchar(input_dataset$MONTH_CD)[1]==5){
      if(month_inf == "01"){
        quarter_inf <- 1
      }else if(month_inf == "04"){
        quarter_inf <- 2
      }else if(month_inf == "07"){
        quarter_inf <- 3
      }else if(month_inf == "10"){
        quarter_inf <- 4
      }else{
        print("Has to start by the first month of a quarter")
      }

      if(month_sup == "03"){
        quarter_sup <- 1
      }else if(month_sup == "06"){
        quarter_sup <- 2
      }else if(month_sup == "09"){
        quarter_sup <- 3
      }else if(month_sup == "12"){
        quarter_sup <- 4
      }else{
        print("Has to end by the last month of a quarter")
      }

      date_inf <- str_c(year_inf,quarter_inf)
      date_sup <- str_c(year_sup,quarter_sup)
      input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
    }

    ## Si on a des donnees annuelles
    else if(nchar(input_dataset$MONTH_CD)[1]==4){
      input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= year_inf & input_dataset$MONTH_CD <= year_sup),]
    }

    names(product_table)[names(product_table) == var_product_PDT] <- "PRODUCT_CD"
    names(product_table)[names(product_table) == var_revision_PDT] <- "FG_REVISION"

    #On definit la clef primaire pour le traitement des donnees (doublons, outliers et calcul d indices).
    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    input_dataset <- data.table(dplyr::select(input_dataset, primary_vector, "VALUE", "QUANTITY", "QUANTITY_UNIT_CD", "MONTH_CD"))

    input_dataset <- input_dataset[(input_dataset$VALUE!=0 & input_dataset$QUANTITY!=0),]

    } else if(!is.null(var_productUI) & !is.null(var_partnerUI)){


  names(input_dataset)[names(input_dataset) == var_reporter] <- "REPORTER_CD"
  names(input_dataset)[names(input_dataset) == var_partner] <- "PARTNER_CD"
  names(input_dataset)[names(input_dataset) == var_product] <- "PRODUCT_CD"
  names(input_dataset)[names(input_dataset) == var_value] <- "VALUE"
  names(input_dataset)[names(input_dataset) == var_quantity] <- "QUANTITY"
  names(input_dataset)[names(input_dataset) == var_quantity_unit] <- "QUANTITY_UNIT_CD"
  names(input_dataset)[names(input_dataset) == var_time] <- "MONTH_CD"

  input_dataset = filter(input_dataset, PRODUCT_CD == var_productUI, PARTNER_CD == var_partnerUI)

  #On convertit VALUE et QUANTITY en numerique
  input_dataset$QUANTITY<-as.numeric(input_dataset$QUANTITY)
  input_dataset$VALUE<-as.numeric(input_dataset$VALUE)*1000

  #### Si l'input DS a des donnees mensuelles
  year_inf <- substr(as.character(str_split(date_range," ")[1]),1,4)
  month_inf <- substr(as.character(str_split(date_range," ")[1]),6,7)

  year_sup <- substr(as.character(str_split(date_range," ")[2]),1,4)
  month_sup <- substr(as.character(str_split(date_range," ")[2]),6,7)

  ## Si on a des donnees mensuelles
  if (nchar(input_dataset$MONTH_CD)[1]==6){
    date_inf <- str_c(year_inf,month_inf)
    date_sup <- str_c(year_sup,month_sup)
    input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
  }
  ## Si on a des donnees trimestielles
  else if(nchar(input_dataset$MONTH_CD)[1]==5){
    if(month_inf == "01"){
      quarter_inf <- 1
    }else if(month_inf == "04"){
      quarter_inf <- 2
    }else if(month_inf == "07"){
      quarter_inf <- 3
    }else if(month_inf == "10"){
      quarter_inf <- 4
    }else{
      print("Has to start by the first month of a quarter")
    }

    if(month_sup == "03"){
      quarter_sup <- 1
    }else if(month_sup == "06"){
      quarter_sup <- 2
    }else if(month_sup == "09"){
      quarter_sup <- 3
    }else if(month_sup == "12"){
      quarter_sup <- 4
    }else{
      print("Has to end by the last month of a quarter")
    }

    date_inf <- str_c(year_inf,quarter_inf)
    date_sup <- str_c(year_sup,quarter_sup)
    input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
  }

  ## Si on a des donnees annuelles
  else if(nchar(input_dataset$MONTH_CD)[1]==4){
    input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= year_inf & input_dataset$MONTH_CD <= year_sup),]
  }

  names(product_table)[names(product_table) == var_product_PDT] <- "PRODUCT_CD"
  names(product_table)[names(product_table) == var_revision_PDT] <- "FG_REVISION"

  #On definit la clef primaire pour le traitement des donnees (doublons, outliers et calcul d indices).
  primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
  primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

  input_dataset <- data.table(dplyr::select(input_dataset, primary_vector, "VALUE", "QUANTITY", "QUANTITY_UNIT_CD", "MONTH_CD"))

  input_dataset <- input_dataset[(input_dataset$VALUE!=0 & input_dataset$QUANTITY!=0),]

    } else if(is.null(var_productUI) & !is.null(var_partnerUI)){


      names(input_dataset)[names(input_dataset) == var_reporter] <- "REPORTER_CD"
      names(input_dataset)[names(input_dataset) == var_partner] <- "PARTNER_CD"
      names(input_dataset)[names(input_dataset) == var_product] <- "PRODUCT_CD"
      names(input_dataset)[names(input_dataset) == var_value] <- "VALUE"
      names(input_dataset)[names(input_dataset) == var_quantity] <- "QUANTITY"
      names(input_dataset)[names(input_dataset) == var_quantity_unit] <- "QUANTITY_UNIT_CD"
      names(input_dataset)[names(input_dataset) == var_time] <- "MONTH_CD"

      input_dataset = filter(input_dataset, PARTNER_CD == var_partnerUI)

      #On convertit VALUE et QUANTITY en numerique
      input_dataset$QUANTITY<-as.numeric(input_dataset$QUANTITY)
      input_dataset$VALUE<-as.numeric(input_dataset$VALUE)*1000

      #### Si l'input DS a des donnees mensuelles
      year_inf <- substr(as.character(str_split(date_range," ")[1]),1,4)
      month_inf <- substr(as.character(str_split(date_range," ")[1]),6,7)

      year_sup <- substr(as.character(str_split(date_range," ")[2]),1,4)
      month_sup <- substr(as.character(str_split(date_range," ")[2]),6,7)

      ## Si on a des donnees mensuelles
      if (nchar(input_dataset$MONTH_CD)[1]==6){
        date_inf <- str_c(year_inf,month_inf)
        date_sup <- str_c(year_sup,month_sup)
        input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
      }
      ## Si on a des donnees trimestielles
      else if(nchar(input_dataset$MONTH_CD)[1]==5){
        if(month_inf == "01"){
          quarter_inf <- 1
        }else if(month_inf == "04"){
          quarter_inf <- 2
        }else if(month_inf == "07"){
          quarter_inf <- 3
        }else if(month_inf == "10"){
          quarter_inf <- 4
        }else{
          print("Has to start by the first month of a quarter")
        }

        if(month_sup == "03"){
          quarter_sup <- 1
        }else if(month_sup == "06"){
          quarter_sup <- 2
        }else if(month_sup == "09"){
          quarter_sup <- 3
        }else if(month_sup == "12"){
          quarter_sup <- 4
        }else{
          print("Has to end by the last month of a quarter")
        }

        date_inf <- str_c(year_inf,quarter_inf)
        date_sup <- str_c(year_sup,quarter_sup)
        input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
      }

      ## Si on a des donnees annuelles
      else if(nchar(input_dataset$MONTH_CD)[1]==4){
        input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= year_inf & input_dataset$MONTH_CD <= year_sup),]
      }

      names(product_table)[names(product_table) == var_product_PDT] <- "PRODUCT_CD"
      names(product_table)[names(product_table) == var_revision_PDT] <- "FG_REVISION"

      #On definit la clef primaire pour le traitement des donnees (doublons, outliers et calcul d indices).
      primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
      primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

      input_dataset <- data.table(dplyr::select(input_dataset, primary_vector, "VALUE", "QUANTITY", "QUANTITY_UNIT_CD", "MONTH_CD"))

      input_dataset <- input_dataset[(input_dataset$VALUE!=0 & input_dataset$QUANTITY!=0),]

    } else {
    names(input_dataset)[names(input_dataset) == var_reporter] <- "REPORTER_CD"
    names(input_dataset)[names(input_dataset) == var_partner] <- "PARTNER_CD"
    names(input_dataset)[names(input_dataset) == var_product] <- "PRODUCT_CD"
    names(input_dataset)[names(input_dataset) == var_value] <- "VALUE"
    names(input_dataset)[names(input_dataset) == var_quantity] <- "QUANTITY"
    names(input_dataset)[names(input_dataset) == var_quantity_unit] <- "QUANTITY_UNIT_CD"
    names(input_dataset)[names(input_dataset) == var_time] <- "MONTH_CD"

    #On convertit VALUE et QUANTITY en numerique
    input_dataset$QUANTITY<-as.numeric(input_dataset$QUANTITY)
    input_dataset$VALUE<-as.numeric(input_dataset$VALUE)*1000

    #### Si l'input DS a des donnees mensuelles
    year_inf <- substr(as.character(str_split(date_range," ")[1]),1,4)
    month_inf <- substr(as.character(str_split(date_range," ")[1]),6,7)

    year_sup <- substr(as.character(str_split(date_range," ")[2]),1,4)
    month_sup <- substr(as.character(str_split(date_range," ")[2]),6,7)

    ## Si on a des donnees mensuelles
    if (nchar(input_dataset$MONTH_CD)[1]==6){
      date_inf <- str_c(year_inf,month_inf)
      date_sup <- str_c(year_sup,month_sup)
      input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
    }
    ## Si on a des donnees trimestielles
    else if(nchar(input_dataset$MONTH_CD)[1]==5){
      if(month_inf == "01"){
        quarter_inf <- 1
      }else if(month_inf == "04"){
        quarter_inf <- 2
      }else if(month_inf == "07"){
        quarter_inf <- 3
      }else if(month_inf == "10"){
        quarter_inf <- 4
      }else{
        print("Has to start by the first month of a quarter")
      }

      if(month_sup == "03"){
        quarter_sup <- 1
      }else if(month_sup == "06"){
        quarter_sup <- 2
      }else if(month_sup == "09"){
        quarter_sup <- 3
      }else if(month_sup == "12"){
        quarter_sup <- 4
      }else{
        print("Has to end by the last month of a quarter")
      }

      date_inf <- str_c(year_inf,quarter_inf)
      date_sup <- str_c(year_sup,quarter_sup)
      input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= date_inf & input_dataset$MONTH_CD <= date_sup),]
    }

    ## Si on a des donnees annuelles
    else if(nchar(input_dataset$MONTH_CD)[1]==4){
      input_dataset <- input_dataset[which(input_dataset$MONTH_CD >= year_inf & input_dataset$MONTH_CD <= year_sup),]
    }

    names(product_table)[names(product_table) == var_product_PDT] <- "PRODUCT_CD"
    names(product_table)[names(product_table) == var_revision_PDT] <- "FG_REVISION"

    #On definit la clef primaire pour le traitement des donnees (doublons, outliers et calcul d indices).
    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    input_dataset <- data.table(dplyr::select(input_dataset, primary_vector, "VALUE", "QUANTITY", "QUANTITY_UNIT_CD", "MONTH_CD"))

    input_dataset <- input_dataset[(input_dataset$VALUE!=0 & input_dataset$QUANTITY!=0),]

  }

  print("Ending change_names")
  return(input_dataset)
}
)
