#' Function to verify if the table revision and the data set are compatibles
#'
#' @param input_dataset last dataset
#' @param out_verif_table table clear
#' @param log_table table with error
#' @param empty_parameter empty data
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

non_HS1 <- compiler::cmpfun(non_HS <- function(
  input_dataset,
  out_verif_table,
  log_table,
  empty_parameter){

  print("Staring non_HS")
  if(empty_parameter == 1){
    log <- log_table
  }
  else{
    data_diff <- sum(input_dataset$VALUE, na.rm=TRUE)-sum(out_verif_table$VALUE, na.rm=TRUE)
    if (data_diff>0){
      x <- round(data_diff,2)
      log1 <- data.table(step = "Warning", note = paste("Some product codes do not belong to any HS revision. It corresponds to $", format(x, digits=nchar(as.character(x)), decimal.mark=",", big.mark = " "),"."), type="warng" , stringsAsFactors = F)
      log <- rbind(log_table,log1)
    }
    else{
      log <- log_table
      rm(input_dataset, log_table)
    }
  }

  print("Ending non_HS")
  return(log)
})
