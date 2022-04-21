k#---- Libraries ----
library(RODBC)
library(caret)
library(rlang)
library(tidyr)
library(zoo)
library(ggplot2)
library(dbplyr)
library(e1071)
library(dplyr)


#---- Functions ---- 
label_high <- function(pt_frame){
  stable_label <- "forestgreen" # Color green
  unstable_label <- "red3" # Color red
  rapid_diff_label <- "darkorange1" # color orange
  sensor_error <- "yellow2" # color yellow
  
  # Label SVM-instances
  pt_frame$label[pt_frame$label==TRUE] <- stable_label
  pt_frame$label[pt_frame$label==FALSE] <- unstable_label
  
  #---- Detect static abnormally high/low parameter - Heart-rate / Respiratory rate / mean Invasive blood pressure----
  pt_frame$label[pt_frame$mon_hr >= 200] <- unstable_label
  pt_frame$label[pt_frame$mon_rr >= 70] <- unstable_label
  pt_frame$label[pt_frame$mon_ibp_mean <= 30] <- unstable_label
  
  # label instances where avg maha_dist > 2*SD 
  # SD of the 80th percentile, stable population of 5-min corrected baseline: 1.715894
  
  # Handicap of 20% due to a stable factor: breath support
  pt_frame$label[(((pt_frame$glob_5_m - pt_frame$glob_12)*1) > (2*1.715894) )] <- rapid_diff_label
  
  # To allow for Baseline build-up, delete the first hour of the baseline
  pt_frame$label[1:3600] <- "black"
  return(pt_frame)
}
label_low <- function(pt_frame){
  stable_label <- "forestgreen" # Color green
  unstable_label <- "red3" # Color red
  rapid_diff_label <- "darkorange1" # color orange
  sensor_error <- "yellow2" # color yellow
  
  # Label SVM-instances
  pt_frame$label[pt_frame$label==TRUE] <- stable_label
  pt_frame$label[pt_frame$label==FALSE] <- unstable_label
  
  #---- Detect static abnormally high/low parameter - Heart-rate / Respiratory rate / mean Invasive blood pressure----
  pt_frame$label[pt_frame$mon_hr >= 200] <- unstable_label
  pt_frame$label[pt_frame$mon_rr >= 70] <- unstable_label
  pt_frame$label[pt_frame$mon_ibp_mean <= 30] <- unstable_label
  
  # label instances where avg maha_dist > 2*SD hourly avg values
  # SD of the 80th percentile, stable population of 5-min corrected baseline: 1.806616
  
  # Handicap of 20% due to a stable factor: breath support
  pt_frame$label[(((pt_frame$glob_5_m - pt_frame$glob_12)*1) > (2*1.806616) )] <- rapid_diff_label
  
  # To allow for Baseline build-up, delete the first hour of the baseline
  pt_frame$label[1:3600] <- "black"
  return(pt_frame)
}
sensor_dysfunction_depricated <- function(pt_frame){
  for(x in 1:NROW(pt_frame$label)){
    if(x>60){delta <- 60} else{delta <- x}
    if(!is.na(pt_frame$mon_ibp_mean[x]==TRUE)){
      if( (pt_frame$mon_ibp_mean[x] > 110 || pt_frame$mon_ibp_mean[x] < 25)){
        pt_frame$label[(x-delta):(x+60)] <- sensor_error
      }
    }
    if(!is.na(pt_frame$nirs_mean[x]) == TRUE){
      if( (pt_frame$nirs_mean[x] > 94 || pt_frame$nirs_mean[x] < 16)){
        pt_frame$label[(x-delta):(x+60)] <- sensor_error
      }
    }
  }
  return(pt_frame)
}
sensor_dysfunction <- function(pt_frame){
  progress = txtProgressBar(min=0, max=nrow(pt_frame))
  
  sensor_label <- "yellow2"
  # mon_sat
  sat <- as.vector(pt_frame$mon_sat)
  ibp <- as.vector(pt_frame$mon_ibp_mean)
  nirs <- as.vector(pt_frame$nirs_mean)
  rr <- as.vector(pt_frame$mon_rr)
  ibp_non_empty <- as.vector(which(!is.na(ibp)))
  sat_non_empty <- as.vector(which(!is.na(sat)))
  new_label <- pt_frame$label
  for(x in 61:(nrow(pt_frame) - 60)){
    setTxtProgressBar(progress,x)
    if(!is.na(sat[x]) && !is.na(ibp[x])){
      sat_non_na <- sat_non_empty[(which(sat_non_empty == x) - 3)]
      ibp_non_na <- sat_non_empty[(which(ibp_non_empty == x) - 3)]
      if(abs(sat[x] - sat[sat_non_na])>20){
        new_label[(x-60):(x+60)] <- sensor_label
      } else if(abs(ibp[(x)] - ibp[ibp_non_na])>20){
        new_label[(x-60):(x+60)] <- sensor_label
      } else if(nirs[x] == 95 || nirs[x] == 15 || rr[x] <= 10){new_label[(x-60):(x+60)] <- sensor_label}
    }
  }
  pt_frame$label <- new_label
  close(progress)
  return(pt_frame)
}
label_replace <- function(pt_frame){
  label <- as.vector(pt_frame$label)
  progress_bar = txtProgressBar(min=0, max=NROW(label))
  new_label <- rep(NA,NROW(label))
  for(x in 180:NROW(label)){
    setTxtProgressBar(progress_bar, x)
    if(is.nan(mean(match(label[(x-180):x], "forestgreen", nomatch = 0))) == TRUE){
      new_label[(x-180):x] <- NA
    } else if(mean(match(label[(x-180):x], "forestgreen", nomatch = 0)) < 0.2){
      new_label[(x-180):x] <- "red3"
    }  else {new_label[(x-180):x] <- "forestgreen"}
  }
  close(progress_bar)
  new_label[1:3600] <- "black"
  new_label[is.na(pt_frame$pat_hosp_id)] <- NA
  pt_frame$label <- new_label
  return(pt_frame)
}
norm_maha_dir <- function(frame){
  progress = txtProgressBar(min=0, max=nrow(frame))
  z_frame <- z_frame_norm(frame)
  cov_vector <-   rbind( 
    c(1.00000000,  0.12100078,  -0.09072419, -0.21651711, -0.05940332),
    c(0.12100078 , 1.00000000 ,  0.00345409, -0.02515421 , 0.00945988),
    c(-0.09072419,  0.00345409 ,  1.00000000,  0.17382592 , 0.08150064),
    c(-0.21651711, -0.02515421,   0.17382592,  1.00000000,  0.16259491),
    c(-0.05940332 , 0.00945988,   0.08150064,  0.16259491,  1.00000000))
  
  temp_hr <- z_frame$mon_hr
  temp_sat <- z_frame$mon_sat
  temp_ibp <- z_frame$mon_ibp_mean
  temp_rr <- z_frame$mon_rr
  temp_nirs <- z_frame$nirs_mean
  temp_etco2 <- frame$mon_etco2
  output_mov <- NA
  frame$mov_maha <- NA
  for (x in 1:nrow(frame)) {
    setTxtProgressBar(progress, x)
    distance <- mahalanobis(c(temp_hr[x], temp_ibp[x], temp_rr[x],temp_sat[x],temp_nirs[x]),0,cov_vector)
    if(!is.na(temp_etco2[x]) && temp_etco2[x] == 1){
      distance <- distance *1.2
    } 
    output_mov[x] <- distance 
  }
  close(progress)
  frame$mov_maha <- output_mov
  return(frame)
}
meng_maha_dir <- function(frame){
  progress = txtProgressBar(min=0, max=nrow(frame))
  z_frame <- z_frame_meng(frame)
  cov_vector <- rbind(  c(1.00000000  ,0.03238790 , -0.06021655, -0.10987559, -0.1981539),
                        c(0.03238790,  1.00000000 ,  0.06019881, -0.01368552, -0.1051947),
                        c( -0.06021655 , 0.06019881   ,1.00000000, -0.07855231,  0.1396818),
                        c(-0.10987559, -0.01368552 , -0.07855231,  1.00000000,  0.3976323),
                        c(-0.19815388 ,-0.10519466 ,  0.13968180 , 0.39763234,  1.0000000))
  
  temp_hr <- z_frame$mon_hr
  temp_sat <- z_frame$mon_sat
  temp_ibp <- z_frame$mon_ibp_mean
  temp_rr <- z_frame$mon_rr
  temp_nirs <- z_frame$nirs_mean
  temp_etco2 <- frame$mon_etco2
  output_mov <- NA
  frame$mov_maha <- NA
  for (x in 1:nrow(frame)) {
    setTxtProgressBar(progress, x)
    distance <- mahalanobis(c(temp_hr[x], temp_ibp[x], temp_rr[x],temp_sat[x],temp_nirs[x]),0,cov_vector)
    if(!is.na(temp_etco2[x]) && temp_etco2[x] == 1){
      distance <- distance *1.2
    } 
    output_mov[x] <- distance 
  }
  close(progress)
  frame$mov_maha <- output_mov
  return(frame)
}
timeframe <- function(frame, t_start,t_end){
  frame$pat_datetime <- as.POSIXct(frame$pat_datetime, format="%Y-%m-%d %T")
  t_start <- as.POSIXct(t_start, format="%Y-%m-%d %T")
  t_end <- as.POSIXct(t_end, format="%Y-%m-%d %T")
  frame <- frame[frame$pat_datetime >= t_start & frame$pat_datetime <= t_end,]
  return(frame)
}
z_frame_norm <- function(norm_frame){
  norm_frame[,c("mon_hr")] <- (norm_frame[,c("mon_hr")] - 144.41) / 18.568
  norm_frame[,c("mon_ibp_mean")] <- (norm_frame[,c("mon_ibp_mean")] - 54.01) / 9.794
  norm_frame[,c("mon_rr")] <- (norm_frame[,c("mon_rr")] - 34.76) / 8.748
  norm_frame[,c("mon_sat")] <- (norm_frame[,c("mon_sat")] - 95.56) / 3.237
  norm_frame[,c("nirs_mean")] <- (norm_frame[,c("nirs_mean")] - 71.68) / 11.97
  return(norm_frame)
}
z_frame_meng <- function(meng_frame){
  meng_frame[,c("mon_hr")] <- (meng_frame[,c("mon_hr")] - 156.82) / 18.375
  meng_frame[,c("mon_ibp_mean")] <- (meng_frame[,c("mon_ibp_mean")] - 52.45) / 7.994
  meng_frame[,c("mon_rr")] <- (meng_frame[,c("mon_rr")] - 34.35) / 8.268
  meng_frame[,c("mon_sat")] <- (meng_frame[,c("mon_sat")] - 75.26) / 7.7196
  meng_frame[,c("nirs_mean")] <- (meng_frame[,c("nirs_mean")] - 55.68) / 10.136
  return(meng_frame)
}
sql_connect <- function(){
  result <- odbcConnect("****", uid="****", pwd = "****")
  return(result)
}
import_pt_frame <- function(pt_id){
  conn_id <- sql_connect()
  query <- paste("SELECT  [pat_hosp_id],[pat_datetime],[mon_hr],[mon_ibp_dia],[mon_ibp_mean],
  [mon_ibp_sys],[mon_rr],[mon_sat],[nirs_l] ,[nirs_r],[mon_etco2] FROM [***].[***].[***] WHERE pat_hosp_id =", pt_id, sep = " ")
  data  <- sqlQuery(conn_id, query)
  if(NROW(data)<3){
    close(conn_id)
    cat("Attempting char-import, no numeric pt-ID recognized", "\n")
    import_pt_frame_txt(pt_id)
  } else {
    cat("Nrow:",nrow(data), " & Ncol:", ncol(data), "\n")
    close(conn_id)
    return(data)
  }
}
import_pt_frame_txt <- function(pt_id){
  conn_id <- sql_connect()
  query <- paste("SELECT  [pat_hosp_id],[pat_datetime],[mon_hr],[mon_ibp_dia],[mon_ibp_mean],
  [mon_ibp_sys],[mon_rr],[mon_sat],[nirs_l] ,[nirs_r],[mon_etco2] FROM [PICURED].[dbo].[NIRS_Ruben_compl] WHERE pat_hosp_id = '", pt_id,"'", sep = "")
  data  <- sqlQuery(conn_id, query)
  cat("Nrow:",nrow(data), " & Ncol:", ncol(data))
  close(conn_id)
  return(data)
}
pre_proces_frame <- function(frame){
  colnames(frame)[1] <- "pat_hosp_id"
  frame <- as.data.frame(cbind(frame[,c("pat_hosp_id","pat_datetime")], prep_model(frame[,
                                                                                         c("mon_hr", 
                                                                                           "mon_ibp_mean", 
                                                                                           "mon_rr", 
                                                                                           "mon_sat", 
                                                                                           "nirs_l", 
                                                                                           "nirs_r", 
                                                                                           "mon_etco2")])))
  #colnames(frame) <- c("pat_hosp_id", "pat_datetime", "mon_hr", "mon_ibp_mean", "mon_rr", "mon_sat", "nirs_mean", "mon_etco2")
  frame$pat_datetime <- as.POSIXct(frame$pat_datetime, format="%Y-%m-%d %T")
  # Initiate label-column for colouring puroposes
  frame$label <- NA
  return(frame)
}
prep_model <- function(frame){
  # replace comma's to points, convert column-types to numeric
  frame <- as.data.frame(numerize(frame))
  # take mean of both nirs-values, if there are two present, lose both columns afterwards.
  frame <- cbind(frame[,c("mon_hr", "mon_ibp_mean", "mon_rr", "mon_sat", "mon_etco2")], nirs_simplify(frame[,c("nirs_l", "nirs_r")])[,3])
  colnames(frame)[6] <- "nirs_mean"
  
  # quantile_clean IBP_mean prior to normalization
  # - DEPRICATED - frame <- quantile_clean(frame)
  
  # lose both ibp Dia & sys
  #frame <- frame[,-which(colnames(frame)=="mon_ibp_dia")]
  # frame <- frame[,-which(colnames(frame) == "mon_ibp_sys")]
  
  # Dichotomise end-tidal CO2: 1 equals current breath-support, 0 equals no breath-support
  frame$mon_etco2[frame$mon_et_co2 > 0] <- 1
  frame$mon_etco2[is.na(frame$mon_etco2)] <- 0
  frame$mon_rr <- rollapplyr(frame$mon_rr,  (300),median,na.rm = TRUE,by = 1,partial = TRUE, fill = NA, align = "right")
  return(frame)
}
numerize <- function(frame){
  frame <- as.data.frame(lapply(frame, function(x){gsub(",", ".",x)}))
  frame <- as.data.frame(apply(frame, 2, as.numeric))  # Convert all variable types to numeric
  return(frame)
  
}
nirs_simplify <- function(nirs_frame){
  nirs_mean <- nirs_frame %>% dplyr::mutate(nirs_mean = rowMeans(across(where(is.numeric)), na.rm=TRUE))
  colnames(nirs_mean)[3] <- "nirs_mean"
  return(nirs_mean)
}
moving_averages_single_pt <- function(pt_frame){
  pt_frame$glob_5_m <- NA
  pt_frame$glob_12 <- NA
  
  cat("Starting 5m-frame \n")
  
  pt_frame$glob_5_m <- rollapplyr(pt_frame$mov_maha,
                                  (5*60),
                                  median,
                                  na.rm = TRUE, 
                                  by = 1,
                                  partial = TRUE, fill = NA, align = "right")
  
  cat("Starting 12h-frame \n")
  pt_frame$glob_12 <- rollapplyr(pt_frame$mov_maha,
                                 (12*60*60),
                                 median,
                                 na.rm = TRUE, 
                                 by = 1,
                                 partial = TRUE, fill = NA, align = "right")
  return(pt_frame)
}
predict_high_group <- function(svm_model, pt_frame){
  # drop_na: SVM does not know how to handle missing data
  pt_frame <- pt_frame%>% drop_na(mon_hr, mon_rr, mon_ibp_mean, nirs_mean, mon_sat)
  pt_frame$label <- predict(svm_model, z_frame_norm(pt_frame[,c("mon_hr", "mon_rr", "mon_ibp_mean", "nirs_mean", "mon_sat")]))
  # Complete the frame to account for missing values
  pt_frame <- pt_frame %>% complete(pat_datetime = seq.POSIXt(min(pat_datetime), max(pat_datetime), by = "secs"))
  return(pt_frame)
}
predict_low_group <- function(svm_model, pt_frame){
  # drop_na: SVM does not know how to handle missing data
  pt_frame <- pt_frame%>% drop_na(mon_hr, mon_rr, mon_ibp_mean, nirs_mean, mon_sat)
  pt_frame$label <- predict(svm_model, z_frame_meng(pt_frame[,c("mon_hr", "mon_rr", "mon_ibp_mean", "nirs_mean", "mon_sat")]))
  # Complete the frame to account for missing values
  pt_frame <- pt_frame %>% complete(pat_datetime = seq.POSIXt(min(pat_datetime), max(pat_datetime), by = "secs"))
  return(pt_frame)
}
  
