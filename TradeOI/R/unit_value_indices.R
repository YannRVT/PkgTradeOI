#' Function to feature indices
#'
#' @param input_dataset last dataset
#' @param product_level level for product code
#' @param var_reporter variable reporter country
#' @param var_partner variable partner country
#' @param date_to_date if date to date is check
#' @param empty_parameter data empty
#' @param time_break the chosen date
#' @param checkbox3 compute "indice"
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


unit_value_indices1 <- compiler::cmpfun(unit_value_indices <- function(
  input_dataset,
  product_level,
  var_reporter,
  var_partner,
  date_to_date,
  empty_parameter,
  time_break,
  checkbox3){

  print("Starting unit_value_indices")

  if(empty_parameter == 1){
    input_dataset  <- data.frame(Info = "No data available in table")
    }

  else if (checkbox3 == TRUE){

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    k <- max(input_dataset$period)


    # Calculer les P0Q0 / PtQt / P0Qt / PtQ0 + fixer a 100 pour les bases
    input_dataset <- input_dataset %>%
      mutate(P0Qt = P0*SUM_Q,
             P0Q0 = P0*Q0,
             PtQ0 = UNIT_VALUE*Q0,
             PtQt = UNIT_VALUE*SUM_Q) %>%
      select(primary_vector, "time", "year", "period", "P0Qt", "P0Q0", "PtQ0", "PtQt")

    # Agreger le dataset selon les variables necessaires (SP0Q0 / SPtQt / SP0Qt / SPtQ0)
    if (product_level==0){
      Product_Level <- rep("ALL",nrow(input_dataset))
      input_dataset <- data.table(cbind(input_dataset,Product_Level))
    }
    else if(product_level==2 | product_level==4 | product_level==6 | product_level==12){
      Product_Level <- substr(input_dataset$PRODUCT_CD,1,product_level)
      input_dataset <- data.table(cbind(input_dataset,Product_Level))
    }
    if (var_reporter == TRUE){
      input_dataset$REPORTER_CD <- "ALL"
    }
    if (var_partner == TRUE){
      input_dataset$PARTNER_CD <- "ALL"
    }


    ## If first de la serie alors fixer 100
    primary_key<-strKey("REPORTER_CD", "Product_Level", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))
    input_dataset <- input_dataset[,.(SP0Qt=sum(P0Qt,na.rm=TRUE),SP0Q0=sum(P0Q0,na.rm=TRUE),SPtQt=sum(PtQt,na.rm=TRUE),SPtQ0=sum(PtQ0,na.rm=TRUE)),by=eval(append(primary_vector,c("year","period","time")))]

    ## Remplacer les 0 par Na ##
    input_dataset[input_dataset == 0]<- NA

    if (date_to_date == TRUE){
      aa <- as.list(names(input_dataset))
      input_dataset[which(time %in% aa),c("SP0Q0", "SPtQt", "SP0Qt", "SPtQ0")]<-100
      print("year_to_date is TRUE")
    }
    else if (date_to_date == FALSE){
      input_dataset[which(time == 1),c("SP0Q0", "SPtQt", "SP0Qt", "SPtQ0")]<-100
      print("year_to_date is FALSE")
    }

    input_dataset <- input_dataset[,.(PAASp=round(SPtQt/SP0Qt*100,0),LASp=round(SPtQ0/SP0Q0*100,0),PAASq=round(SPtQt/SPtQ0*100,0),LASq=round(SP0Qt/SP0Q0*100,0)),
                   by=eval(append(primary_vector,c("year","period")))]

    input_dataset <- mutate(input_dataset,
                            Fisher.Px = round(sqrt(PAASp*LASp)),
                            Fisher.Qt = round(sqrt(PAASq*LASq)),
                            Idx.Value = round(PAASp*LASq/100))

    if(time_break=="year"){
      input_dataset = data.table(input_dataset) %>%
        select(primary_vector, "year", "Fisher.Px", "PAASp", "LASp", "Fisher.Qt", "PAASq", "LASq", "Idx.Value") %>%
        rename(Year = year, Paasche.Px = PAASp , Laspeyres.Px = LASp ,Paasche.Qt = PAASq , Laspeyres.Qt = LASq)
    }
    else if(time_break=="quarter"){
      input_dataset <- data.table(input_dataset) %>%
        select(primary_vector, "year", "period", "Fisher.Px", "PAASp", "LASp", "Fisher.Qt", "PAASq", "LASq", "Idx.Value") %>%
        rename(Year = year, Quarter = period, Paasche.Px = PAASp , Laspeyres.Px = LASp ,Paasche.Qt = PAASq , Laspeyres.Qt = LASq )
    }
    else if(time_break=="month"){
      input_dataset <- data.table(input_dataset) %>%
        select(primary_vector, "year", "period", "Fisher.Px", "PAASp", "LASp", "Fisher.Qt", "PAASq", "LASq", "Idx.Value") %>%
        rename(Year = year, Month = period, Paasche.Px = PAASp , Laspeyres.Px = LASp ,Paasche.Qt = PAASq , Laspeyres.Qt = LASq )
    }

    input_dataset <- rename(input_dataset, "Reporter" = "REPORTER_CD", "Product" = "Product_Level","Partner" = "PARTNER_CD", "Fisher price" = "Fisher.Px", "Paasche price" = "Paasche.Px", "Laspeyres price" = "Laspeyres.Px", "Fisher quantity" = "Fisher.Qt", "Paasche quantity" = "Paasche.Qt", "Laspeyres quantity" = "Laspeyres.Qt", "Index value" = "Idx.Value")

  }else{

    log <- data.table(step="Info", note = paste("No indices calculation requested"), stringsAsFactors = F)
    log1 <- data.table(step="Message", note = paste("If you want prices and quantities indices, make sure you check the box"), stringsAsFactors = F)
    input_dataset <- rbind(log,log1)
  }

  print("Ending unit_value_indices")
  return(input_dataset)
})

