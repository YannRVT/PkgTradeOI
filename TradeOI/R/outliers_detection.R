#' Function to detect outlier with robust regression and Z-modify
#'
#' @param input_dataset last dataset
#' @param var_time variable for the time
#' @param empty_parameter empty data
#' @param radio_outliers define the type of computation for outliers
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

outliers_detection1 <- compiler::cmpfun(outliers_detection <- function(
  input_dataset,
  var_time,
  empty_parameter,
  radio_outliers){

  print("Starting outliers_detection")
  if(empty_parameter == 1){
    output <- data.table()
  }
  else if (radio_outliers == 1 | radio_outliers == 2 | radio_outliers == 3 | radio_outliers == 4){

    LOG_V <- ifelse(input_dataset$VALUE!=0,log(input_dataset$VALUE),NA)
    LOG_Q <- ifelse(input_dataset$QUANTITY!=0,log(input_dataset$QUANTITY),NA)

    input_dataset_1 <- mutate(input_dataset,
                              LOG_V = log(VALUE),
                              LOG_Q = log(QUANTITY))

    primary_key<-strKey("REPORTER_CD", "PRODUCT_CD", "PARTNER_CD", gbl_var_key_add)
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))

    count_reg <- na.omit(input_dataset_1, c("UNIT_VALUE"))
    count_reg <- select(count_reg, primary_vector)
    count_reg <- as.data.table(x = count_reg)[, .N, by=count_reg]
    colnames(count_reg)[colnames(count_reg)=="N"] <- "nb_reg"

    ## Pour que la regression LTS se passe bien on doit avoir au moins le moitie des observations avec une donnee.
    ##C'est pourquoi on les compte.
    ## Dans le cas ou l'on n'a pas assez d'obs valides (meme s'il y en a plus que 30) on passe en Z-modifies

    count = input_dataset_1 %>%
      filter(LOG_UV != 0) %>%
      filter(!is.na(LOG_UV))

    count_empty_uv <- select(count, primary_vector)
    count_empty_uv <- as.data.table(x = count_empty_uv)[, .N, by=count_empty_uv]
    colnames(count_empty_uv)[colnames(count_empty_uv)=="N"] <- "nb_reg_uv"

    count_empty_val <- select(count, primary_vector)
    count_empty_val <- as.data.table(x = count_empty_val)[, .N, by=count_empty_val]
    colnames(count_empty_val)[colnames(count_empty_val)=="N"] <- "nb_reg_val"

    count_empty_qty <- select(count, primary_vector)
    count_empty_qty <- as.data.table(x = count_empty_qty)[, .N, by=count_empty_qty]
    colnames(count_empty_qty)[colnames(count_empty_qty)=="N"] <- "nb_reg_qty"


    primary_key_a<-paste0("a.", gsub(" ", "a.", primary_key, fixed=TRUE))
    primary_vector<-unlist(strsplit(gsub(" ", "", primary_key, fixed=TRUE), split=","))
    str_sql_join_b<-paste0("a.", primary_vector[1], "=b.",primary_vector[1])
    str_sql_join_c<-paste0("a.", primary_vector[1], "=c.",primary_vector[1])
    str_sql_join_d<-paste0("a.", primary_vector[1], "=d.",primary_vector[1])

    for (current_var in primary_vector[2:length(primary_vector)]){
      str_sql_join_b<-paste0(str_sql_join_b, " and a.", current_var, "=b.", current_var)
      str_sql_join_c<-paste0(str_sql_join_c, " and a.", current_var, "=c.", current_var)
      str_sql_join_d<-paste0(str_sql_join_d, " and a.", current_var, "=d.", current_var)
    }

    data_count <- data.table(sqldf(paste0("
                                          select distinct ", primary_key_a, ", a.nb_reg, b.nb_reg_uv, c.nb_reg_val, d.nb_reg_qty
                                          from count_reg as a join count_empty_uv as b on (", str_sql_join_b, ")
                                          join count_empty_val as c on (", str_sql_join_c, ")
                                          join count_empty_qty as d on (", str_sql_join_d, ")
                                          ")))

    rm(count_empty_uv, count_empty_val, count_empty_qty)
    gc()

    data4_ante1<-data.table(input_dataset)
    data4_ante2<-data.table(
      LOG_V=numeric(),
      LOG_Q=numeric(),
      nb_reg=integer(),
      outlier_unitvalue=integer(),
      outlier_value =integer(),
      outlier_quantity=integer(),
      res_val = numeric(),
      res_uv = numeric(),
      res_qty = numeric(),
      stringsAsFactors=FALSE
    )
    data4_ante1 <- data4_ante1[0,]
    data4 <- data.table(data4_ante1,data4_ante2)

    data4_A <- data4
    data4_B <- data4

    DS_A <- data_count[which(nb_reg>30 & nb_reg_uv>nb_reg/2 & nb_reg_val>nb_reg/2 & nb_reg_qty>nb_reg/2),]
    DS_B <- data_count[which(nb_reg<=30 | nb_reg_uv<=nb_reg/2 | nb_reg_val<=nb_reg/2 | nb_reg_qty<=nb_reg/2),]

    rm(data_count, data4_ante1, data4_ante2, count_reg, input_dataset)
    gc()

    if (nrow(DS_A)>0){
      ## on pas aux M-Estimateurs
      ## Faire une boucle sur le trio Reporter/partner/product
      for (i in 1:nrow(DS_A)){

        boucle <- DS_A[i,]
        data <- as.data.frame(merge(input_dataset_1,boucle,by=primary_vector))
        data <- data.table(data[, !(colnames(data) %in% c("nb_reg_uv","nb_reg_val","nb_reg_qty"))])
        data[] <- lapply(data, function(x){
          x[is.nan(x)] <- NA
          x
        })
        data <- na.omit(data, c("LOG_UV", "LOG_V", "LOG_Q"))

        ###detection on UV
        X_uv <- as.matrix(model.matrix(LOG_UV ~ 1+time , data = data))
        Y_uv <- data$LOG_UV

        m0_uv <- ltsReg(X_uv[,-1], Y_uv)
        m1_uv <- lmrob..M..fit(
          X_uv, Y_uv,
          beta.initial = coef(m0_uv),
          scale = mad(m0_uv$residuals),
          control = lmrob.control(compute.outlier.stats="MM", psi = 'bisquare')
        )

        ###detection on value
        X_val <- as.matrix(model.matrix(LOG_V ~ 1+time , data = data))
        Y_val <- data$LOG_V

        m0_val <- ltsReg(X_val[,-1], Y_val)
        m1_val <- lmrob..M..fit(
          X_val, Y_val,
          beta.initial = coef(m0_val),
          scale = mad(m0_val$residuals),
          control = lmrob.control(compute.outlier.stats="MM", psi = 'bisquare'))

        ###detection on quantity
        X_qty <- as.matrix(model.matrix(LOG_Q ~ 1+time , data = data))
        Y_qty <- data$LOG_Q

        m0_qty <- ltsReg(X_qty[,-1], Y_qty)
        m1_qty <- lmrob..M..fit(
          X_qty, Y_qty,
          beta.initial = coef(m0_qty),
          scale =  mad(m0_qty$residuals),
          control = lmrob.control(compute.outlier.stats="MM", psi = 'bisquare'))

        ## changer les m1 en m
        outlier_value <- rep(0,nrow(data))
        outlier_quantity <- rep(0,nrow(data))
        outlier_unitvalue <- rep(0,nrow(data))

        outlier_value[which(abs(m1_val$residuals)>4.5*m1_val$scale)]<-1
        outlier_quantity[which(abs(m1_qty$residuals)>4.5*m1_qty$scale)]<-1
        outlier_unitvalue[which(abs(m1_uv$residuals)>4.5*m1_uv$scale)]<-1
        data$res_val = m1_val$residuals
        data$res_qty = m1_qty$residuals
        data$res_uv = m1_uv$residuals
        data <- data.table(data,outlier_unitvalue,outlier_quantity,outlier_value)

        data <- data[which(data$outlier_unitvalue ==1 & (data$outlier_value == 1 | data$outlier_quantity==1)),]
        data4_A <- rbind(data4,data,fill=TRUE)
        data4 <- data4_A
      }
    }
    else{
      print("No robust regression")
    }
    rm(DS_A)
    gc()

    print("End of robust regression")

    if (nrow(DS_B)>0){

      print("Start of Z-modified")

      dt <- data.table(merge(input_dataset_1,DS_B,by=primary_vector))

      dt$QUANTITY <- ifelse(dt$QUANTITY==1,dt$QUANTITY+10e-13,dt$QUANTITY)
      dt$VALUE <- ifelse(dt$VALUE==1,dt$VALUE+10e-13,dt$VALUE)
      dt$UNIT_VALUE <- ifelse(dt$UNIT_VALUE==1,dt$unit_VALUE+10e-13,dt$UNIT_VALUE)

      dt$LOG_Q <- ifelse(dt$QUANTITY!=0,log(dt$QUANTITY),NA)
      dt$LOG_V <- ifelse(dt$VALUE!=0,log(dt$VALUE),NA)
      dt$LOG_UV <- ifelse(dt$UNIT_VALUE!=0,log(dt$UNIT_VALUE),NA)

      Zmodif <- dt[,.(median_uv=median(LOG_UV,na.rm=TRUE),mad_uv=mad(LOG_UV,na.rm=TRUE),
                        median_v=median(LOG_V,na.rm=TRUE),mad_v=mad(LOG_V,na.rm=TRUE),
                        median_q=median(LOG_Q,na.rm=TRUE),mad_q=mad(LOG_Q,na.rm=TRUE),
                        year, period, time,VALUE,QUANTITY,UNIT_VALUE,LOG_UV,LOG_V,LOG_Q,nb_reg, MONTH_CD),
                     by=primary_vector]

      Zmodif <- mutate(Zmodif,
                       zms_UV = 0.6745*(LOG_UV-median_uv)/mad_uv,
                       zms_V = 0.6745*(LOG_V-median_v)/mad_v,
                       zms_Q = 0.6745*(LOG_Q-median_q)/mad_q)

      Zmodif <- select(Zmodif, "zms_UV", "zms_V", "zms_Q", primary_vector,"MONTH_CD", "year", "period",
                       "time", "VALUE", "QUANTITY", "UNIT_VALUE", "LOG_UV", "LOG_V", "LOG_Q", "nb_reg")


      outlier_value <- rep(0,nrow(Zmodif))
      outlier_quantity <- rep(0,nrow(Zmodif))
      outlier_unitvalue <- rep(0,nrow(Zmodif))

      outlier_value[which(abs(Zmodif$zms_V)>3.5)]<-1
      outlier_quantity[which(abs(Zmodif$zms_Q)>3.5)]<-1
      outlier_unitvalue[which(abs(Zmodif$zms_UV)>3.5)]<-1

      Zmodif <- data.table(Zmodif,outlier_unitvalue,outlier_quantity,outlier_value)


      data4_B <- Zmodif[which(abs(Zmodif$zms_UV)>3.5 & (abs(Zmodif$zms_V)>3.5 | abs(Zmodif$zms_Q)>3.5)),]
      data4_B <- data4_B %>%
        rename("res_uv" = "zms_UV", "res_val" = "zms_V", "res_qty" = "zms_Q")
      rm(Zmodif, dt, DS_B)
      gc()
      print("End of Z-modified")


    }else{
      print("No Z-modified")
    }

    output <- rbind(data4_A,data4_B)
    output$FG_OUTLIERS <- 1
    output <- full_join(input_dataset_1,output)

    rm(input_dataset_1, data4, data4_A, data4_B)
    gc()

  }else{
    df <- data.table(
      LOG_V = as.numeric(""),
      LOG_Q = as.numeric(""),
      nb_reg = as.integer(""),
      outlier_unitvalue = as.numeric(""),
      outlier_value = as.numeric(""),
      outlier_quantity = as.numeric(""),
      FG_OUTLIERS = as.numeric("")
    )

    output <- cbind(input_dataset, df)
    rm(input_dataset, df)
    gc()
    }

  print("Ending outliers_detection")
  return(output)
})
