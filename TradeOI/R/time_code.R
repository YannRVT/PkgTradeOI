#' Function to modify and standardize the time variable
#'
#' @param input_dataset last dataset
#' @param pct_missdata % of chosen missing data
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

time_code1 <- compiler::cmpfun(time_code <- function(
  input_dataset,
  pct_missdata,
  empty_parameter){

  print("Starting time_code")

  if(empty_parameter == 1){
    input_dataset <- data.table()
  }
  else {

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    print("miss val 1/2")
    if(input$checkbox1 == TRUE){
    input_dataset <- input_dataset[,!(names(input_dataset) %in% c("FG_REVISION", "rev_order_vect"))]}
    else{
      input_dataset <- data.table(input_dataset[,-c("FG_REVISION", "rev_order_vect")])
    }

    len_mnth_cd <- max(nchar(input_dataset$MONTH_CD))
    if(len_mnth_cd == 6){
      time_sep <- 12
      min_time <- min(input_dataset$MONTH_CD)
      max_time <- max(input_dataset$MONTH_CD)
      period_min <- as.numeric(substr(min_time,5,7))
      period_max <- as.numeric(substr(max_time,5,7))
      year_min <- as.numeric(substr(min_time,1,4))
      year_max <- as.numeric(substr(max_time,1,4))

      time_arg <- (year_max-year_min)+1
      period_factor <- rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),time_arg)
    }
    else if(len_mnth_cd ==5){
      time_sep <- 4
      min_time <- min(input_dataset$MONTH_CD)
      max_time <- max(input_dataset$MONTH_CD)
      period_min <- as.numeric(substr(min_time,5,6))
      period_max <- as.numeric(substr(max_time,5,6))
      year_min <- as.numeric(substr(min_time,1,4))
      year_max <- as.numeric(substr(max_time,1,4))

      time_arg <- (year_max-year_min)+1
      period_factor <- rep(c("1","2","3","4"),time_arg)
    }
    else if(len_mnth_cd == 4){
      time_sep <- 1
      min_time <- min(input_dataset$MONTH_CD)
      max_time <- max(input_dataset$MONTH_CD)
      period_min <- 1
      period_max <- 1
      year_min <- as.numeric(substr(min_time,1,4))
      year_max <- as.numeric(substr(max_time,1,4))

      time_arg <- (year_max-year_min)+1
      period_factor <- rep(c("1"),time_arg)

      add_y <- rep(1,nrow(input_dataset))
      aa <- str_c(input_dataset$MONTH_CD,add_y)
      input_dataset[,"MONTH_CD"] <- aa
      rm(aa, add_y)
      gc()
    }
    else{
      print("Error in variable format: MONTH_CD")
    }

    time_arg <-  time_sep*(year_max-year_min+1)
    year <- rep(0,time_arg)

    j = year_min
    for (i in seq(1, time_sep*(year_max-year_min+1),by=time_sep)){
      year[i:(i+time_sep-1)]<-j
      j = j+1
    }

    time_corr <- data.table(period_factor,period_factor)
    names(time_corr) <- c("period","period_factor")
    time_corr[,1] <- as.numeric(time_corr$period)
    time_corr <- data.table(time_corr,year)

    ###Cas : commencement ou fin d'analyse (date_range) different d'une annee calendaire (debut janvier fin decembre) hors donnee annuelle
    if ((len_mnth_cd != 4) & (period_min != 1 | period_max != 12)){
      time_corr <- time_corr[-which((time_corr$period < period_min & time_corr$year <= year_min)|(time_corr$period > period_max & time_corr$year >= year_max)),]
    }
    else{
      time_corr
    }

    MONTH_CD <- as.character(str_c(time_corr$year,time_corr$period_factor))
    time_corr <- data.table(time_corr, MONTH_CD)

    prim_key <- select(input_dataset, primary_vector)
    prim_key <- unique(prim_key)
    prim_key <- data.table(prim_key)

    out <- CJ.dt(time_corr, prim_key)
    setcolorder(out, c(primary_vector,"period","year", "period_factor", "MONTH_CD"))

    rm(time_corr)
    gc()

    out[,"MONTH_CD"] <- as.character(out$MONTH_CD)
    out <- data.table(out)

    primary_key_time <- paste0(primary_key, ", MONTH_CD")
    primary_vector_time <- unlist(strsplit(gsub(" ", "", primary_key_time, fixed=TRUE), split=","))

    input_dataset <- left_join(out, input_dataset, by = primary_vector_time)
    setcolorder(input_dataset, c(primary_vector,"MONTH_CD", "period","year", "period_factor", "VALUE", "QUANTITY", "QUANTITY_UNIT_CD"))

    rm(out)
    gc()

    print("sql miss val bis 2/2")

    ## Supprimer les series avec plus de X pct de donnees manquantes
    input_dataset <- mutate(input_dataset,
                            UNIT_VALUE = VALUE/QUANTITY,
                            LOG_UV = log(VALUE/QUANTITY))
    input_dataset <- select(input_dataset, primary_vector, "year", "period", "VALUE", "QUANTITY", "UNIT_VALUE", "LOG_UV", "MONTH_CD")

    nb_occur <- (year_max - year_min - 1)* time_sep + period_max + (time_sep - period_min + 1)

    intermed <- na.omit(input_dataset, c("VALUE"))
    intermed <- select(intermed, primary_vector)
    intermed <- as.data.table(x = intermed)[, .N, by=intermed]
    colnames(intermed)[colnames(intermed)=="N"] <- "nb_reg"

    intermed <- intermed[nb_reg>=nb_occur*(1-pct_missdata/100)]

    prim_key <- select(intermed, primary_vector)
    prim_key <- unique(prim_key)
    rm(intermed)
    gc()

    input_dataset <- data.table(merge(prim_key, input_dataset, by=primary_vector))

    ## Faire un vecteur time 1:nrow()
    input_dataset <- setcolorder(input_dataset, primary_vector_time)

    time <- rep(1:nb_occur,nrow(prim_key))
    input_dataset <- data.table(input_dataset,time)
    rm(prim_key)
    gc()
  }

  print("Ending time_code")
  return(input_dataset)
})
