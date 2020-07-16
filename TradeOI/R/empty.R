#' Function to check if there is no empty table during all the features
#'
#' @param log1_dataset dataset with error info
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

are_error_in_DS1 <- compiler::cmpfun(are_error_in_DS <- function(log1_dataset){
  print("in are_error_in_DS")
  if(length(which(log1_dataset$type=="error"))>0){
    error_input <- 1
  }
  else{
    error_input <- 0
  }
  return(error_input)
})
#########################################################################
#########################################################################
is_empty_parameter1 <- compiler::cmpfun(is_empty_parameter <- function(input_dataset){
  print("in is_empty_parameter")

  if(nrow(input_dataset)<2){
    TK_emplty <- 1
  }
  else{
    TK_emplty <- 0
  }
  return(TK_emplty)
}
)
#########################################################################
#########################################################################
is_empty_output1 <- compiler::cmpfun(is_empty_output <- function(
  log_dataset,
  note_txt,
  IEx,
  IEx_1,
  error_input){

  print("in is_empty_output")
  if(IEx==1){
    if(error_input == 1 | IEx_1 == 1){
      log1 <- data.frame(step = "Error", note = paste("The table",note_txt,"is empty because of previous errors."), type="error" , stringsAsFactors = F)
      log <- rbind(log_dataset,log1)

    }else{
      log1 <- data.frame(step = "Error", note = paste("After",note_txt,"verification, all data have been deleted from the dataset."), type="error" , stringsAsFactors = F)
      log <- rbind(log_dataset,log1)
    }
  }
  else{
    log <- log_dataset
  }

  return(log)
})
