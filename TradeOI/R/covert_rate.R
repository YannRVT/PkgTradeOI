#' Function to create the covert rate
#'
#' @param original_ds original dataset
#' @param non_HS_ds non HS dataset
#' @param doublons_ds doublon dataset
#' @param qty_unit_ds quantity unit dataset
#' @param chmt_rev_ds revision change dataset
#' @param incompl_series_ds incomplete series dataset
#' @param outliers_ds outliers dataset
#' @param non_homogeneous_ds non homogeneous dataset
#' @param indices_ds indices dataset
#' @param detected_outliers outliers detected dataset
#' @param series_hetero series hetero dataset
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

covert_rate1 <- compiler::cmpfun(covert_rate <- function(
  original_ds,
  non_HS_ds,
  doublons_ds,
  qty_unit_ds,
  chmt_rev_ds,
  incompl_series_ds,
  outliers_ds ,
  non_homogeneous_ds,
  indices_ds,
  detected_outliers,
  series_hetero,
  empty_parameter){

  if(empty_parameter == 1){
    output <- data.frame(Info = "No data available in table")}

  else if(empty_parameter == 0){

  # Compute the TOTAL
  Information <- paste("Original dataset")
  Trade_Value_Total <- sum(original_ds$VALUE, na.rm=TRUE)
  Trade_Value_Total_pct <- 1

  primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
  all_series <- sqldf(paste0("
                             select distinct ", primary_key,"
                             from original_ds
                             group by ", primary_key
  ))
  Series_Number <- nrow(all_series)

  data1 <- cbind(Information,Trade_Value_Total,Trade_Value_Total_pct,Series_Number)

  ##########################################################################
  if (input$checkbox1 == TRUE){

    data_in <- chmt_rev_ds
    Information <- paste("After removing series with unstable HS revision")

    trade_value <- sum(data_in$VALUE, na.rm=TRUE)
    trade_value_pct <- round(trade_value/Trade_Value_Total,2)

    all_series <- sqldf(paste0("
                               select distinct ", primary_key, "
                               from data_in
                               group by ", primary_key
    ))
    nb_series <- nrow(all_series)

    data4 <- cbind(Information,trade_value,trade_value_pct,nb_series)
  }else{

    Information <- paste("No revision check requested")
    trade_value <- Trade_Value_Total
    trade_value_pct <- Trade_Value_Total_pct
    nb_series <- Series_Number
    data4 <-cbind(Information,trade_value,trade_value_pct,nb_series)}

  ##########################################################################
  data_in <- non_HS_ds
  Information <- paste("After removing non-HS product code")

  trade_value <- sum(data_in$VALUE, na.rm=TRUE)
  trade_value_pct <- round(trade_value/Trade_Value_Total,2)

  all_series <- sqldf(paste0("
                             select distinct ", primary_key, "
                             from data_in
                             group by ", primary_key
  ))
  nb_series <- nrow(all_series)

  data1bis <- cbind(Information,trade_value,trade_value_pct,nb_series)

  ##########################################################################
  data_in <- doublons_ds
  Information <- paste("After aggregating duplicate records")

  trade_value <- sum(data_in$VALUE, na.rm=TRUE)
  trade_value_pct <- round(trade_value/Trade_Value_Total,2)

  all_series <- sqldf(paste0("
                             select distinct ", primary_key, "
                             from data_in
                             group by ", primary_key
  ))
  nb_series <- nrow(all_series)

  data2 <- cbind(Information,trade_value,trade_value_pct,nb_series)

  ##########################################################################
  data_in <- qty_unit_ds
  Information <- paste("After quantity unit homogeneity verification")

  trade_value <- sum(data_in$VALUE, na.rm=TRUE)
  trade_value_pct <- round(trade_value/Trade_Value_Total,2)

  all_series <- sqldf(paste0("
                             select distinct ", primary_key, "
                             from data_in
                             group by ", primary_key
  ))
  nb_series <- nrow(all_series)

  data3 <- cbind(Information,trade_value,trade_value_pct,nb_series)

  ##########################################################################

  if (input$checkbox2 == TRUE){

    data_in <- chmt_rev_ds
    Information <- paste("After removing series with unstable HS revision")

    trade_value <- sum(data_in$VALUE, na.rm=TRUE)
    trade_value_pct <- round(trade_value/Trade_Value_Total,2)

    all_series <- sqldf(paste0("
                               select distinct ", primary_key, "
                               from data_in
                               group by ", primary_key
    ))
    nb_series <- nrow(all_series)

    data4 <- cbind(Information,trade_value,trade_value_pct,nb_series)
  }else{

    Information <- paste("No revision check requested")

    data4 <-cbind(Information,trade_value,trade_value_pct,nb_series)}
  ##########################################################################

  data_in <- incompl_series_ds
  Information <- paste("After removing incomplete series")

  trade_value <- sum(data_in$VALUE, na.rm=TRUE)
  trade_value_pct <- round(trade_value/Trade_Value_Total,2)

  all_series <- sqldf(paste0("
                             select distinct ", primary_key, "
                             from data_in
                             group by ", primary_key
  ))
  nb_series <- nrow(all_series)

  data5 <- cbind(Information,trade_value,trade_value_pct,nb_series)

  primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
  ##########################################################################

  if (input$radio_outliers == 1) {

    data_in <- outliers_ds
    Information <- paste("After removing outliers (Total outliers =", nrow(detected_outliers), ")")

    trade_value <- sum(data_in$VALUE, na.rm=TRUE)
    trade_value_pct <- round(trade_value/Trade_Value_Total,2)

    all_series <- sqldf(paste0("
                               select distinct ", primary_key, "
                               from data_in
                               group by ", primary_key
    ))
    nb_series <- nrow(all_series)

    data6 <- cbind(Information,trade_value,trade_value_pct,nb_series)

  }else{

    Information <- paste("No request to withdraw the outliers")
    data6 <- cbind(Information,trade_value,trade_value_pct,nb_series)}
    ##########################################################################
  if (input$checkbox2 == TRUE) {

    data_in <- non_homogeneous_ds
    Information <- paste("After removing heterogeneous series (Total heterogeneous series =", nrow(series_hetero), ")")

    trade_value <- sum(data_in$VALUE, na.rm=TRUE)
    trade_value_pct <- round(trade_value/Trade_Value_Total,2)

    all_series <- sqldf(paste0("
                               select distinct ", primary_key, "
                               from data_in
                               group by ", primary_key
    ))
    nb_series <- nrow(all_series)

    data7 <- cbind(Information,trade_value,trade_value_pct,nb_series)

  }else{

    Information <- paste("No request to withdraw the heterogeneous series")
    data7 <- cbind(Information,trade_value,trade_value_pct,nb_series)}

  if (input$checkbox3 == TRUE) {

    Information <- paste("After calculating indices")

    trade_value <- sum(data_in$VALUE, na.rm=TRUE)
    trade_value_pct <- round(trade_value/Trade_Value_Total,2)

    all_series <- sqldf(paste0("
                               select distinct ", primary_key, "
                               from data_in
                               group by ", primary_key
    ))
    nb_series <- nrow(all_series)
    data8 <- cbind(Information,trade_value,trade_value_pct,nb_series)

  }else{

    Information <- paste("No request for calculation of indices")
    data8 <- cbind(Information,trade_value,trade_value_pct,nb_series)}

  ##########################################################################
  output <- as.data.table(rbind(data1,data1bis,data2,data3,data4,data5,data6,data7,data8))
  output$Trade_Value_Total_pct = sprintf("%.1f %%", 100*as.numeric(output$Trade_Value_Total_pct))
  output$Trade_Value_Total = format(as.numeric(output$Trade_Value_Total), scientific=FALSE, big.mark=" ")
  output$Series_Number = format(as.numeric(output$Series_Number), scientific=FALSE, big.mark=" ")
  output <- rename(output, "Total trade value" = Trade_Value_Total, "Total in %" = Trade_Value_Total_pct , "Series number" = Series_Number)

  }
  return(output)
})
