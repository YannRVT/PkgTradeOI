#' Function to generate the primary key/vector
#'
#' @param reporter variable reporter
#' @param product variable product
#' @param partner variable partner
#' @param var_key_add variable add
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

strKey <- function(
  reporter,
  product,
  partner,
  var_key_add
){
  primary_key<-paste0(reporter, ", ", product, ", ", partner)
  if(var_key_add != ""){
    primary_key<-paste0(primary_key, ", ", var_key_add)
  }
  return(primary_key)
}
