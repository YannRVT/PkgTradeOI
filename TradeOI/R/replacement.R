#' Function to replace or delete the outliers
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
#'

replacement1 = compiler::cmpfun(replacement <- function(
  input_dataset,
  empty_parameter){

  print("Starting replacement")

  if(empty_parameter == 1){
    data <- data.table()

  }else if(input$radio_outliers == 1){

    input_dataset <- data.table(input_dataset)
    input_dataset[which(input_dataset$FG_OUTLIERS==1),c("VALUE","QUANTITY","UNIT_VALUE")] <- 0
    final = input_dataset %>%
      select(-LOG_UV, -LOG_V, -LOG_Q, -res_val, -res_uv, -res_qty)

  }else if(input$radio_outliers == 2){

    final = input_dataset %>%
      select(-res_val, -res_uv, -res_qty)

  }else if(input$radio_outliers == 3){

    input_dataset = input_dataset %>%
      select(-res_val, -res_uv, -res_qty)

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    input_dataset$outlier_unitvalue = ifelse(is.na(input_dataset$outlier_unitvalue),0,input_dataset$outlier_unitvalue)
    input_dataset$outlier_quantity = ifelse(is.na(input_dataset$outlier_quantity),0,input_dataset$outlier_quantity)
    input_dataset$outlier_value = ifelse(is.na(input_dataset$outlier_value),0,input_dataset$outlier_value)
    input_dataset$VALUE2 = NA
    input_dataset$QUANTITY2 = NA
    input_dataset$UNIT_VALUE2 = NA
    input_dataset$ORDER = NA

    final = input_dataset[0,]

    uni = select(input_dataset, primary_vector)
    uni = unique(uni)

    for (j in 1:nrow(uni)) {

      y = uni[j,]
      data = filter(input_dataset, REPORTER_CD == y$REPORTER_CD, PRODUCT_CD == y$PRODUCT_CD, PARTNER_CD == y$PARTNER_CD)
      data = data[order(data$MONTH_CD),]
      data$ORDER = 1:max(nrow(data))
      data2 = filter(data, outlier_value != 1, outlier_quantity != 1)

      for (i in 1L:nrow(data)) {
        x = data[i, ]
        y = rbind(data2,x)
        y = y[order(y$ORDER),]

        if (x$outlier_value == 1 & x$outlier_quantity == 1) {

          if (i-1 == 0) {
            x$VALUE2 = (y[i+1,]$VALUE + y[i+2,]$VALUE)/2
            x$QUANTITY2 = (y[i+1,]$QUANTITY + y[i+2,]$QUANTITY)/2
            x$UNIT_VALUE2 = x$VALUE2 / x$QUANTITY2

          } else if (i+1 > max(nrow(y))) {
            x$VALUE2 = (y[i-1,]$VALUE + y[i-2,]$VALUE)/2
            x$QUANTITY2 = (y[i-1,]$QUANTITY + y[i-2,]$QUANTITY)/2
            x$UNIT_VALUE2 = x$VALUE2 / x$QUANTITY2

          } else {
            x$VALUE2 = (y[i-1,]$VALUE + y[i + 1,]$VALUE)/2
            x$QUANTITY2 = (y[i-1,]$QUANTITY + y[i+1,]$QUANTITY)/2
            x$UNIT_VALUE2 = x$VALUE2 / x$QUANTITY2
          }

        } else if (x$outlier_value == 1) {

          if (i-1 == 0) {
            x$VALUE2 = (y[i+1,]$VALUE + y[i+2,]$VALUE)/2
            x$UNIT_VALUE2 = x$VALUE2 / x$QUANTITY

          } else if (i+1 > max(nrow(y))) {
            x$VALUE2 = (y[i-1,]$VALUE + y[i-2,]$VALUE)/2
            x$UNIT_VALUE2 = x$VALUE2 / x$QUANTITY

          } else {
            x$VALUE2 = (y[i-1,]$VALUE + y[i+1,]$VALUE)/2
            x$UNIT_VALUE2 = x$VALUE2 / x$QUANTITY
          }

        } else if (x$outlier_quantity == 1) {

          if (i-1 == 0) {
            x$QUANTITY2 = (y[i+1,]$QUANTITY + y[i+2,]$QUANTITY)/2
            x$UNIT_VALUE2 = x$VALUE / x$QUANTITY2

          } else if (i+1 > max(nrow(y))) {
            x$QUANTITY2 = (y[i-1,]$QUANTITY + y[i-2,]$QUANTITY)/2
            x$UNIT_VALUE2 = x$VALUE / x$QUANTITY2

          } else {
            x$QUANTITY2 = (y[i-1,]$QUANTITY + y[i+1,]$QUANTITY)/2
            x$UNIT_VALUE2 = x$VALUE / x$QUANTITY2
          }

        } else {
          x = x
        }

        final = rbind(x, final)

      }
    }


  }else if(input$radio_outliers == 4){

    final = input_dataset

    final$VALUE2 = ifelse(final$outlier_value == 1, final$VALUE*final$res_val, final$VALUE)
    final$VALUE2 = ifelse(final$VALUE2 < 0, final$VALUE2 * -1, final$VALUE2)
    final$QUANTITY2 = ifelse(final$outlier_quantity == 1, final$QUANTITY*final$res_qty, final$QUANTITY)
    final$QUANTITY2 = ifelse(final$QUANTITY2 < 0, final$QUANTITY2 * -1, final$QUANTITY2)
    final$UNIT_VALUE2 = final$VALUE2/final$QUANTITY2

  }else{
    final = input_dataset

  }

  return(final)

  }
)
