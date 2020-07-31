#' Function to prepare the data for feature indice
#'
#' @param input_dataset last dataset
#' @param time_break the chosen date
#' @param indices_bc indices chosen
#' @param date_to_date date to date or not
#' @param date_range month/year/quarter
#' @param empty_parameter empty data
#' @param checkbox3 compute "indice"
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

prepare_indices1 <- compiler::cmpfun(prepare_indices <- function(
  input_dataset,
  time_break,
  indices_bc,
  date_to_date,
  date_range,
  empty_parameter,
  checkbox3){

  print("Starting prepare_indices")
  if(empty_parameter == 1){
    data3 <- data.table()
  }
  else if (checkbox3 == TRUE){

    year_inf <- substr(as.character(str_split(date_range," ")[1]),1,4)

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    data3 <- input_dataset %>%
      select(primary_vector,c("year","UNIT_VALUE","period","time"))

    data3 = data3[0,]

    data3 <- data.frame(
      data3,
      SUM_V=numeric(),
      SUM_Q=numeric(),
      LOG_UV=numeric(),
      P0 =integer(),
      Q0 =integer(),
      stringsAsFactors=FALSE
    )

    if((time_break=="year") & (max(input_dataset$period)== 12)){


        input_dataset <- data.table(sqldf(paste0("
                                                   select ", primary_key, ", year, time, sum(VALUE) as SUM_V, sum(QUANTITY) as SUM_Q,
                                                   sum(VALUE)/sum(QUANTITY) as UNIT_VALUE, log(sum(VALUE)/sum(QUANTITY)) as LOG_UV, 1 as period
                                                   from input_dataset
                                                   group by ", primary_key, ", year
                                                   ")))
        input_dataset$time = input_dataset$time/12

        k <- 1

    }

    else if((time_break=="year") & (max(input_dataset$period)== 4)){
      input_dataset <- data.table(sqldf(paste0("
                                                   select ", primary_key, ", year, time, sum(VALUE) as SUM_V, sum(QUANTITY) as SUM_Q,
                                                   sum(VALUE)/sum(QUANTITY) as UNIT_VALUE, log(sum(VALUE)/sum(QUANTITY)) as LOG_UV, 1 as period
                                                   from input_dataset
                                                   group by ", primary_key, ", year
                                                   ")))
      input_dataset$time = input_dataset$time/4

      k <- 1

    }

    else if((time_break=="year") & (max(input_dataset$period)== 1)){
      input_dataset <- data.table(sqldf(paste0("
                                                   select ", primary_key, ", year, time, sum(VALUE) as SUM_V, sum(QUANTITY) as SUM_Q,
                                                   sum(VALUE)/sum(QUANTITY) as UNIT_VALUE, log(sum(VALUE)/sum(QUANTITY)) as LOG_UV, 1 as period
                                                   from input_dataset
                                                   group by ", primary_key, ", year
                                                   ")))

      k <- 1

    }
    else if(time_break=="quarter"){
      if (max(input_dataset$period)== 4){
        input_dataset <- data.table(sqldf(paste0("
                                                   select ", primary_key, ", year, period, time, sum(VALUE) as SUM_V,
                                                   sum(QUANTITY) as SUM_Q, sum(VALUE)/sum(QUANTITY) as UNIT_VALUE, log(sum(VALUE)/sum(QUANTITY)) as LOG_UV
                                                   from input_dataset
                                                   group by ", primary_key, ", year, period
                                                   ")))
        k <- 4

      }
      else if (max(input_dataset$period)== 12){
        #from month to quarter
        quarter <- rep(0,nrow(input_dataset))
        for (g in 1:nrow(input_dataset)){
          if(input_dataset[g,period] >= 1 & input_dataset[g,period] <= 3){
            quarter[g] <- 1
          }
          else if(input_dataset[g,period] >= 4 & input_dataset[g,period] <= 6){
            quarter[g] <- 2
          }
          else if(input_dataset[g,period] >= 7 & input_dataset[g,period] <= 9){
            quarter[g] <- 3
          }
          else if(input_dataset[g,period] >= 10 & input_dataset[g,period] <= 12){
            quarter[g] <- 4
          }
        }
        input_dataset$period <- quarter
        input_dataset <- data.table(sqldf(paste0("
                                                   select ", primary_key, ", year, period, time,  sum(VALUE) as SUM_V, sum(QUANTITY) as SUM_Q,
                                                   sum(VALUE)/sum(QUANTITY) as UNIT_VALUE, log(sum(VALUE)/sum(QUANTITY)) as LOG_UV
                                                   from input_dataset
                                                   group by ", primary_key, ", year, period
                                                   ")))

        input_dataset$time = input_dataset$time/3

        k <- 4

      }
      else{
        print("Only monthly and quarterly data can provide quarterly indices.")
      }
    }
    else if(time_break=="month"){
      if (max(input_dataset$period)== 12){
        input_dataset <- data.table(sqldf(paste0("
                                                   select ", primary_key, ", year, period, time, sum(VALUE) as SUM_V, sum(QUANTITY) as SUM_Q,
                                                   sum(VALUE)/sum(QUANTITY) as UNIT_VALUE, log(sum(VALUE)/sum(QUANTITY)) as LOG_UV
                                                   from input_dataset
                                                   group by ", primary_key, ", year, period
                                                   ")))
        k <- 12

      }
      else{
        print("Only monthly data can provide monthly indices.")
      }
    }
    else{
      print("Error: time_break parameter is not well initialized")
    }

    #######################################################################
    if(date_to_date == TRUE){
      if(k == 1){
        print("WARNING: year_to_date indices are not relevant for yearly time_break")
      }
      else{
        if(indices_bc == "base"){

          str_sql_join<-paste0("t0.", primary_vector[1], "=t1.",primary_vector[1])

          for (current_var in primary_vector[2:length(primary_vector)]){

            str_sql_join<-paste0(str_sql_join, " and t0.", current_var, "=t1.", current_var)

          }
		  #Dans le cas "year-to-date" il est necessaire de rajouter le mois dans la clef de fusion.
          str_sql_join<-paste0(str_sql_join," and t0.period=t1.period")
          tab1 <- subset(input_dataset, input_dataset$year == year_inf)
          P0 <- tab1$UNIT_VALUE
          Q0 <- tab1$SUM_Q
          tab1 <- data.table(tab1, P0, Q0)

          data3 <- sqldf(paste0("
                                select t0.*, t1.P0, t1.Q0
                                from input_dataset as t0 inner join tab1 as t1
                                on ", str_sql_join
          ))

          data3 <- data.table(sqldf(paste0("select * from data order by ", primary_key, ", year, period")))
        }

        if(indices_bc =="chained"){
          if((time_break=="month")){

          P0 <- input_dataset$UNIT_VALUE
          P0 <- shift(P0, n=12L)

          Q0 <- input_dataset$SUM_Q
          Q0<-shift(Q0, n=12L)

          data3 <- data.table(input_dataset, P0, Q0)

          data3 <- mutate(data3, Q0 = ifelse(year==year_inf, NA,Q0))
          data3 <- mutate(data3, P0 = ifelse(year==year_inf, NA,P0))
          }

          else if((time_break=="quarter")){

            P0 <- input_dataset$UNIT_VALUE
            P0<-shift(P0, n=4L)

            Q0 <- input_dataset$SUM_Q
            Q0<-shift(Q0, n=4L)

            data3 <- data.table(input_dataset, P0, Q0)

            data3 <- mutate(data3, Q0 = ifelse(year==year_inf, NA,Q0))
            data3 <- mutate(data3, P0 = ifelse(year==year_inf, NA,P0))
            }
          }
        }
      }

    else if(date_to_date == FALSE){
      if(indices_bc=="base"){

        str_sql_join<-paste0("t0.", primary_vector[1], "=t1.",primary_vector[1])

        for (current_var in primary_vector[2:length(primary_vector)]){

          str_sql_join<-paste0(str_sql_join, " and t0.", current_var, "=t1.", current_var)

        }

        tab1 <- subset(input_dataset, input_dataset$year == year_inf)
        P0 <- tab1$UNIT_VALUE
        Q0 <- tab1$SUM_Q
        tab1 <- data.table(tab1, P0, Q0)

        data3 <- sqldf(paste0("
                              select t0.*, t1.P0, t1.Q0
                              from input_dataset as t0 inner join tab1 as t1
                              on ", str_sql_join
        ))
      }

      if(indices_bc=="chained"){

        P0 <- input_dataset$UNIT_VALUE
        P0<-shift(P0, n=1L)

        Q0 <- input_dataset$SUM_Q
        Q0<-shift(Q0, n=1L)

        data3 <- data.table(input_dataset, P0, Q0)

        data3 <- mutate(data3, Q0 = ifelse(year==year_inf, NA,Q0))
        data3 <- mutate(data3, P0 = ifelse(year==year_inf, NA,P0))
      }
    }
    rm(input_dataset, tab1)
    gc()


    }else{

    data3 <- data.table(select(
                        input_dataset,
                        "REPORTER_CD",
                        "PRODUCT_CD",
                        "PARTNER_CD",
                        "year"),
                        time = as.numeric(""),
                        period = as.numeric(""),
                        LOG_UV = as.numeric(""),
                        UNIT_VALUE = as.numeric(""),
                        SUM_Q = as.numeric(""),
                        SUM_V = as.numeric(""),
                        P0 = as.numeric(""),
                        Q0 = as.numeric("")
                        )
    rm(input_dataset)
    gc()

  }

  print("Ending prepare_indices")
  return(data3)
})
