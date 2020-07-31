#' Function to detect heterogeneity in the time series
#'
#' @param input_dataset last dataset
#' @param empty_parameter empty data
#' @param checkbox2 compute "delete heterogenious time series
#' @param radio_outliers define the type of computation for outliers
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
#'

heterogeneity1 <- compiler::cmpfun(heterogeneity <- function(
  input_dataset,
  empty_parameter,
  checkbox2,
  radio_outliers){

  print("Starting heterogeneity")

  if(empty_parameter == 1){
    data <- data.table()

  }else if (checkbox2 == TRUE){
    print("Starting heterogeneity else")

    if(radio_outliers == 3 | radio_outliers == 4){
      input_dataset$VALUE = ifelse(!is.na(input_dataset$FG_OUTLIERS),input_dataset$VALUE2, input_dataset$VALUE)
      input_dataset$UNIT_VALUE = ifelse(!is.na(input_dataset$FG_OUTLIERS),input_dataset$UNIT_VALUE2, input_dataset$UNIT_VALUE)
      input_dataset$QUANTITY = ifelse(!is.na(input_dataset$FG_OUTLIERS),input_dataset$QUANTITY2, input_dataset$QUANTITY)
      input_dataset = input_dataset %>%
        select(-VALUE2,-QUANTITY2,-UNIT_VALUE2)

    }else{
      input_dataset = input_dataset
    }

    data <- data.table(input_dataset)


    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    data <- data[,.(RIQ = (quantile(UNIT_VALUE, na.rm=TRUE, probs = 0.75) - quantile(UNIT_VALUE, na.rm=TRUE, probs = 0.25)) / median(UNIT_VALUE, na.rm=TRUE),
                                        RSD = sd(UNIT_VALUE, na.rm = TRUE)/mean(UNIT_VALUE, na.rm=TRUE),
                                        year,time,period,UNIT_VALUE,VALUE,QUANTITY),
                                        by=primary_vector]

    data5 <- data[which(RIQ > 2 | RSD > 1.75),]

    if (nrow(data5) != 0){

      data5$FG_HETEROGEN <- 1

      data <- merge(input_dataset,data, all.x=TRUE, by=append(primary_vector, c("time","year","period","QUANTITY","VALUE","UNIT_VALUE")))
      data <- merge(data,data5, all.x=TRUE, by=append(primary_vector, c("time","year","period","QUANTITY","VALUE","UNIT_VALUE","RSD","RIQ")))

      rm(data5, input_dataset)
      gc()
    }
    else{

      data <- merge(input_dataset,data, all.x=TRUE, by=append(primary_vector, c("time","year","period","QUANTITY","VALUE","UNIT_VALUE")))
      data$FG_HETEROGEN <- NA

      rm(data5, input_dataset)
      gc()
    }
  }else{
    df <- data.table(
      RIQ = as.numeric(""),
      RSD = as.numeric(""),
      FG_HETEROGEN = as.logical("")
    )

    data <- cbind(input_dataset, df)
    rm(input_dataset, df)
    gc()

    }

  print("Ending heterogeneity")
  return(data)
})
