#' Function to standardize HS code
#'
#' @param input_dataset dataset to produce
#' @param product_table the product table than your import
#' @param empty_parameter, empty file
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


verif_HS1 <- compiler::cmpfun(verif_HS <- function(
  input_dataset,
  product_table,
  empty_parameter){

  print("Staring verif_HS")
  if(empty_parameter == 1){
    input_dataset <- data.table()
  }
  else{

    #Si la longueur de la table produit fait qu on est en HS4 plutot de 5, on recopie le HS4 sur le 5eme digit
    if(min(str_length(product_table[,"FG_REVISION"]))<5){
      vect <- product_table[,"FG_REVISION"]
      product_table[,"FG_REVISION"] <- str_c(vect,substr(vect,4,4))
    }

    ln_prod <- max(str_length(input_dataset$PRODUCT_CD))
    if (ln_prod > 6){
      input_dataset <- data.table(input_dataset, substr(input_dataset$PRODUCT_CD,1,6))
      colnames(input_dataset)[colnames(input_dataset)=="V2"] <- "PDCT_merge"
    }
    else{
      input_dataset <- data.table(input_dataset, input_dataset$PRODUCT_CD)
      colnames(input_dataset)[colnames(input_dataset)=="V2"] <- "PDCT_merge"
    }

    primary_key<-strKey("REPORTER_CD", "input_dataset.PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)

    input_dataset <- data.table(sqldf(paste0(
      "select ", primary_key, ",VALUE,QUANTITY,QUANTITY_UNIT_CD,MONTH_CD,FG_REVISION
				from input_dataset inner join product_table
				on input_dataset.PDCT_merge = product_table.PRODUCT_CD"
    )))
    rm(product_table)
    gc()
  }
  print("Ending verif_HS")
  return(input_dataset)
})
