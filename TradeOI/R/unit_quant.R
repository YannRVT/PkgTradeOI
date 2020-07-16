#' Function to check and change the unit quantity
#'
#' @param input_dataset last dataset
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


unit_quant1 <- compiler::cmpfun(unit_quant <- function(
  input_dataset,
  empty_parameter){


  if(empty_parameter == 1){
    print("in unit_quant empty_parameter==1")
    input_dataset <- data.table()

  }else{
    print("in unit_quant empty_parameter!=1")

    ## On ecarte les series dont les unites de quantite changent dans le temps.
    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    raw_dbln<- sqldf(paste0("
                         select distinct ", primary_key, ", QUANTITY_UNIT_CD
                         from input_dataset
                         where QUANTITY_UNIT_CD <>''
                         group by ", primary_key, ", QUANTITY_UNIT_CD
                         "))

    raw_dbln <- sqldf(paste0("
                              select distinct ", primary_key, " , count(*) as cnt
                              from raw_dbln
                              group by ", primary_key, "
                              having cnt > 1
                              "))

    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))
    if (length(raw_dbln)> 0){

      reconstruct <- data.table(merge(input_dataset,raw_dbln, by=primary_vector))
      reconstruct <- reconstruct[,c(1:8)]

      fun.12 <- function(input_dataset, reconstruct){
          x.1p <- do.call("paste", input_dataset)
          x.2p <- do.call("paste", reconstruct)
        input_dataset[! x.1p %in% x.2p, ]
      }

      input_dataset <- fun.12(input_dataset,reconstruct)

      rm(reconstruct, raw_dbln)
      gc()
    }
  }

  print("Ending unit_quant")
  return(input_dataset)
})
