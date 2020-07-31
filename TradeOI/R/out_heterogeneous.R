#' Function to delete all the heterogenous time series
#'
#' @param input_dataset last dataset
#' @param empty_parameter empty data
#' @param checkbox2 compute "delete heterogenious time series
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

out_heterogeneous1 <- compiler::cmpfun(out_heterogeneous <- function(
  input_dataset,
  empty_parameter,
  checkbox2){

  print("Starting out_heterogeneous")
  if(empty_parameter == 1){
    input_dataset <- data.table(Info = "No data available in table")
  }
  else if (checkbox2 == TRUE){

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    input_dataset <- input_dataset %>%
      filter(input_dataset$FG_HETEROGEN == 1) %>%
      select(primary_vector, RIQ, RSD) %>%
      rename("Reporter" = "REPORTER_CD", "Product" = "PRODUCT_CD", "Partner" = "PARTNER_CD")

  }else{
    input_dataset <- data.table(Info = "No heterogeneous series detection requested")
  }

  print("Ending out_heterogeneous")
  return(input_dataset)
})
