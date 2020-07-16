#' Function to check and delete doublons
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

doublons1 <- compiler::cmpfun(doublons <- function(
  input_dataset,
  empty_parameter){

  print("Staring doublons")
  if(empty_parameter == 1){
    input_dataset <- data.table()
  }
  else{

    ## On recherche les doublons
    if (gbl_var_key_add == ""){
      prm_var_key_add<-"MONTH_CD"
    }
    else{
      prm_var_key_add<-paste0(gbl_var_key_add, ", MONTH_CD")
    }

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", prm_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    count_dblons <- select(input_dataset, primary_vector)
    count_dblons <- as.data.table(x = count_dblons)[, .N, by=count_dblons]
    colnames(count_dblons)[colnames(count_dblons)=="N"] <- "cnt"

    raw_dbln <- which(count_dblons$cnt != 1)

    ## Si un ou plusieurs doublons sont trouves, on les corriges
    if (length(raw_dbln)> 0){
      count_dblons_crash<-sqldf(paste0("
                                       select count(*) as nb
                                       from (
                                       select ", primary_key, "
                                       from input_dataset
                                       group by ", primary_key, ", QUANTITY_UNIT_CD
                                       )
                                       group by ", primary_key
      ))
      nb_dbl_crash <- which(count_dblons_crash$nb != 1)

      if (length(nb_dbl_crash)>0){

        #####Generation de l erreur (et sortie de la fonction ?)
        print("Duplicate records with different units of quantity detected. Unable to reconcile duplicates. Exiting.")
      }

      ## Si on a des doublons, on en fait la somme des valeurs et des quantites
      input_dataset <- sqldf(paste0("
                                    select ", primary_key, ", sum(VALUE) as VALUE, sum(QUANTITY) as QUANTITY, QUANTITY_UNIT_CD, FG_REVISION
                                    from input_dataset
                                    group by ", primary_key, ", QUANTITY_UNIT_CD, FG_REVISION
                                    "))

    }
    else{
      print("No duplicate record detected")
    }

    rm(count_dblons)
    gc()

  }

  print("Ending doublons")
  return(input_dataset)
})
