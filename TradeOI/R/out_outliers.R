#' Function to delete the outliers
#'
#' @param input_dataset_outliers dataset without outliers
#' @param input_dataset_hetero dataset with heterogeneity
#' @param input_dataset_outliers2 dataset with outliers
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

out_outliers1 <- compiler::cmpfun(out_outliers <- function(
  input_dataset_outliers,
  input_dataset_hetero,
  input_dataset_outliers2,
  empty_parameter){

  print("Starting out_outliers")

  if(empty_parameter == 1){
    output <- data.table(Info = "No data available in table")

    } else if (input$radio_outliers == 1 | input$radio_outliers == 2){

    input_dataset_outliers = input_dataset_outliers %>%
      select(-res_val, -res_uv, -res_qty)
    input_dataset_hetero <- input_dataset_hetero[which(input_dataset_hetero$FG_OUTLIERS==1 & is.na(input_dataset_hetero$FG_HETEROGEN)),]

    primary_key <- strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_key_a <- paste0("a.", gsub(" ", "a.", primary_key, fixed=TRUE))
    primary_vector <- unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))
    str_sql_join <- paste0("a.", primary_vector[1], "=b.",primary_vector[1])

    for (current_var in primary_vector[2:length(primary_vector)]){
      str_sql_join <- paste0(str_sql_join, " and a.", current_var, "=b.", current_var)
    }

    input_dataset_outliers <- data.table(sqldf(paste0("
			select distinct ", primary_key_a, ", a.period, a.year, a.time, a.UNIT_VALUE, a.QUANTITY, a.VALUE, a.outlier_value, a.outlier_quantity
      from input_dataset_outliers as a join input_dataset_hetero as b on (", str_sql_join, ")
		")))

    # Calcul de la mediane
    output <- input_dataset_outliers[,.(median_uv=median(UNIT_VALUE, na.rm=TRUE)),by=primary_vector, ]
    output <- merge(input_dataset_outliers, output, by=primary_vector)

    output <- data.table(sqldf(paste0("
			select distinct ", primary_key_a, ", a.period, a.year, a.time, a.UNIT_VALUE, a.QUANTITY, a.VALUE, a.median_uv, a.outlier_value, a.outlier_quantity
      from output as a join input_dataset_hetero as b on (", str_sql_join, " and a.time=b.time)
		")))

    rm(input_dataset_hetero, input_dataset_outliers)
    gc()

    output <- data.table(sqldf(paste0("select *, abs(median_uv/UNIT_VALUE) as div_median_uv from output")))
    output <- output %>%
      rename("Reporter" = "REPORTER_CD", "Product" = "PRODUCT_CD", "Partner" = "PARTNER_CD", "Period" = "period", "Year" = "year", "Time"="time", "Unit value" = "UNIT_VALUE", "Quantity" = "QUANTITY", "Value" = "VALUE", "Median unit value" = "median_uv", "Outlier value" = "outlier_value", "Outlier quantity" = "outlier_quantity", "Deviation to the median unit value"= "div_median_uv")

###############################################################################################################################################

    }else if(input$radio_outliers == 3 | input$radio_outliers == 4){

      input_dataset_outliers = input_dataset_outliers %>%
        select(-res_val, -res_uv, -res_qty)
      input_dataset_hetero <- input_dataset_hetero[which(input_dataset_hetero$FG_OUTLIERS==1 & is.na(input_dataset_hetero$FG_HETEROGEN)),]

      primary_key <- strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
      primary_key_a <- paste0("a.", gsub(" ", "a.", primary_key, fixed=TRUE))
      primary_vector <- unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))
      str_sql_join <- paste0("a.", primary_vector[1], "=b.",primary_vector[1])

      for (current_var in primary_vector[2:length(primary_vector)]){
        str_sql_join <- paste0(str_sql_join, " and a.", current_var, "=b.", current_var)
      }

      input_dataset_outliers2 <- data.table(sqldf(paste0("
			select distinct ", primary_key_a, ", a.period, a.year, a.time, a.UNIT_VALUE, a.UNIT_VALUE2 ,a.QUANTITY, a.QUANTITY2, a.VALUE, a.VALUE2, a.outlier_value, a.outlier_quantity
      from input_dataset_outliers2 as a join input_dataset_hetero as b on (", str_sql_join, ")
		")))

      # Calcul de la mediane
      output <- input_dataset_outliers2[,.(median_uv=median(UNIT_VALUE, na.rm=TRUE)),by=primary_vector, ]
      output <- merge(input_dataset_outliers2, output, by=primary_vector)

      output <- data.table(sqldf(paste0("
			select distinct ", primary_key_a, ", a.period, a.year, a.time, a.UNIT_VALUE, a.UNIT_VALUE2 ,a.QUANTITY, a.QUANTITY2, a.VALUE, a.VALUE2, a.median_uv, a.outlier_value, a.outlier_quantity
      from output as a join input_dataset_hetero as b on (", str_sql_join, " and a.time=b.time)
		")))

      rm(input_dataset_hetero, input_dataset_outliers2)
      gc()

      output <- data.table(sqldf(paste0("select *, abs(median_uv/UNIT_VALUE) as div_median_uv from output")))
      output <- output %>%
          rename("Reporter" = "REPORTER_CD", "Product" = "PRODUCT_CD", "Partner" = "PARTNER_CD", "Period" = "period", "Year" = "year", "Time"="time",
                 "Original unit value" = "UNIT_VALUE", "Replacement unit value" = "UNIT_VALUE2", "Original quantity" = "QUANTITY", "Replacement quantity" = "QUANTITY2",
                 "Original value" = "VALUE", "Replacement value" = "VALUE2", "Median unit value" = "median_uv", "Outlier value" = "outlier_value",
                 "Outlier quantity" = "outlier_quantity", "Deviation to the median unit value"= "div_median_uv", )



  }else{
    output <- data.table(Info = "No outlier calculation requested")
  }

  print("Ending out_outliers")
  return(output)
})
