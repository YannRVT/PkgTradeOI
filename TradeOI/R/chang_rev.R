#' Function to check and change the revision
#'
#' @param input_dataset last dataset
#' @param table_revision revision dataset
#' @param empty_parameter empty dataset
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

chang_rev1 <- compiler::cmpfun(chang_rev <- function(
  input_dataset,
  table_revision,
  empty_parameter){

  print("Staring chang_rev")

  if(empty_parameter == 1){
    input_dataset <- data.table(NA)
  }
  else if (input$checkbox1 == TRUE){

    min_time<-min(input_dataset$MONTH_CD)
    max_time<-max(input_dataset$MONTH_CD)
    year_min <-substr(min_time,1,4)
    year_max <-substr(max_time,1,4)

    pos_1_min <- min(substr(table_revision[which(table_revision$year==year_min),"class"],2,2))
    pos_1_max <- max(substr(table_revision[which(table_revision$year == year_max),"class"],2,2))


    if(1 >= pos_1_min & 1 <= pos_1_max){v1 <- 1}else{v1 <- "_"}
    if(2 >= pos_1_min & 2 <= pos_1_max){v2 <- 1}else{v2 <- "_"}
    if(3 >= pos_1_min & 3 <= pos_1_max){v3 <- 1}else{v3 <- "_"}
    if(4 >= pos_1_min & 4 <= pos_1_max){v4 <- 1}else{v4 <- "_"}
    if(5 >= pos_1_min & 5 <= pos_1_max){v5 <- 1}else{v5 <- "_"}

    rm(table_revision)
    gc()

    rev_order_vect <- str_c(v1, v2, v3, v4, v5)
    rev_order_vect<- rep(rev_order_vect,nrow(input_dataset))

    input_dataset <- data.table(cbind(input_dataset,rev_order_vect))

    input_dataset <- sqldf("select *
                               from input_dataset
                               where FG_REVISION LIKE rev_order_vect")
    rm(rev_order_vect)
    gc()

  }else{

    input_dataset <- data.table(input_dataset,
                                rev_order_vect = as.character(""))
  }

  print("Ending chang_rev")
  return(input_dataset)
})
