#README
In this script, data from separate NIRS-files are merged with data from the PICURED database.
Separate NIRS-files may have different formats differing in output column-length.
Currently, four known formats are included. Noteworthy: other formats may be known not included in this script.

#END README
library(RODBC)
library(ggplot2)


# -- create test-frame in order to get column-names
frame_data <- sqlQuery(sql_connect(), "SELECT TOP (1) * FROM [PICURED].[dbo].[DATA]")

# -- create empty dataframe to fill 
pt_total_data <- matrix(nrow=0, ncol=92)
colnames(pt_total_data) <- c(colnames(frame_data), "nirs_l", "nirs_r", "channel_1", "channel_2")

# -- import file-locations, create unique row 
complete_file_locations <- sqlQuery(sql_connect(), "SELECT * FROM [PICURED].[dbo].[NIRS_moshtach2]")

clean_file_locations <- matrix(nrow=0, ncol=2)
colnames(clean_file_locations) <-c("pat_id", "file_locus")

clean_file_locations <- filter_file_loc(complete_file_locations)

filter_file_loc <- function(file){
  for (x in 1:nrow(file)){
    if(is.na(file$patientenid[x]) == FALSE){
      pat_id <- file$patientenid[x]
      file_locus <- file$locus_bestand[x]
      
      new_row <- data.frame("pat_id"=pat_id, "file_locus"=file_locus)
      
      clean_file_locations <- rbind(clean_file_locations, new_row)
      cat('Processing file', x, 'of', nrow(complete_file_locations),'\n')
      
    }
  }
  return(clean_file_locations)
}


# ------- Start TESTING - Moshtach-10 files -------- #
clean_file_locations <- read.csv(file.choose(), sep=";")
colnames(clean_file_locations) <- c("pat_id", "file_locus")
# ------- End TESTING - Moshtach-10 files -------- #

#clean-file-locations -> unique list of pt-id's
unique_pat_id <- unique(clean_file_locations$pat_id)

# loop over list of unique id's, get nirs + total data, merge sets and add to matrix. 
for (current_patient in 1:NROW(unique_pat_id)){
  
  #generate list of files to merge for current patient
  files_current_patient <- file_list(unique_pat_id[current_patient], clean_file_locations)
  
  #create matrix containing all data from above file-list -> matrix contains pat_id / timestamp / nirs_l / nirs_r
  current_pt_data_nirs <- nirs_file_load(files_current_patient[,2], unique_pat_id[x])
          cat("nirs-data obtained \n")
  #create matrix containing all data from big-database for pat_id
  current_pt_data_total <- data_matrix(unique_pat_id[current_patient], "[DATA]")
          cat("Overall data obtained \n")
          cat('Starting merger: Patient ', current_patient, 'of ', NROW(unique_pat_id),'\n')
  
  # create new matrix, merging both nirs and total data according to timestamp (NIRS is lead!) 
  pt_complete_data <- merge_data(unique_pat_id[current_patient], current_pt_data_nirs, current_pt_data_total)
  
  # pt_complete_data now contains every nirs-entry with corresponding vital pm-data
          cat('\n Patient ', current_patient, 'of ', NROW(unique_pat_id),' merged.','\n', "Starting upload \n")
  upload_total_data(pt_complete_data, "[NIRS_Ruben]")
          cat("\n Upload complete, starting new patient \n")
  
}  

## --------- FUNCTIONS ----------- #

plot_frame <- function(specific_data_plot, xlabel){
  ggplot(specific_data_plot, aes(x=pat_datetime, omit.na=TRUE) )  +
    geom_line(aes(y=mon_hr, colour="Heart rate"), lwd=1) + labs (x=xlabel, y= "", colour="Legend") +
    geom_line(aes(y=mon_rr, colour="Resp. rate"), lwd=0.8) +
    geom_line(aes(y=mon_sat, colour="Blood sat."), lwd=0.8) +
    geom_line(aes(y=mon_ibp_mean, colour="Invasive mean bp"), lwd=0.8) +
    geom_line(aes(y=nirs_l, colour="Left nirs"), lwd=0.8) +
    geom_line(aes(y=nirs_r, colour="Right nirs"), lwd=0.8) +
    
    geom_line(aes(y=mon_ibp_dia), color="black", lwd=0.3) +
    geom_line(aes(y=mon_ibp_sys), color="black", lwd=0.3) +
    geom_ribbon(aes(ymin=mon_ibp_dia, ymax=mon_ibp_sys), alpha=0.2, fill="chartreuse3", color="transparent") +
    ylim(0, 250) + xlim(58000,72000)
  
}

## --- Connect to sql-server PICURED
sql_connect <- function(){
  result <- odbcConnect("PICURED", uid="**", pwd = "**")
  return(result)
}

## ---- acquire filelist from moshtach-files per pt-ID, return matrix with pat_id + file-list 
file_list <- function(pat_id, file_complete_list){
  #create empty data-frame for file list
  file_location <- matrix(nrow=0, ncol=2)
  colnames(file_location) <- c("pat_id", "file_loc")
  
  for (x in 1:nrow(file_complete_list)){
    #cat("Processing file", x, "of ", nrow(file_complete_list), "\n")
    if (file_complete_list$pat_id[x] == pat_id){
      new_row <- data.frame("pat_id"=file_complete_list$pat_id[x], "file_loc"=file_complete_list$file_loc[x])
      file_location <- rbind(file_location, new_row)
    }
  }
  return(file_location)
}
## --- acquire data from files from file_location, merging the nirs-data into a matrix
nirs_file_load <- function(file_location, pat_id) {
  progress = txtProgressBar(min=0, max=NROW(file_location))
  
  cat("loading", NROW(file_location), "files \n")
  nirs_matrix <- matrix(nrow=0, ncol=6) 
  colnames(nirs_matrix) <- c("pat_id", "timestamp", "nirs_l", "nirs_r", "channel_1", "channel_2")
  for (x in 1:NROW(file_location)) {
    setTxtProgressBar(progress, x)
    nirs_data_file <- load_file(file_location[x], pat_id)
    
    #add file to the overall matrix
    nirs_matrix <- rbind(nirs_matrix, nirs_data_file)
    
  }
  ## return matrix containing pt_id / timestamp / nirs_l / nirs_r
  close(progress)
  return(nirs_matrix)
}

## --- load said file, transform data and return format pat_id / timestamp / nirs_l / nirs_r
load_file <- function(file_location, pat_id){
  data <- read_nirs(file_location)
  data <- cbind(pat_id, data)
  return(data)
}

## --- acquire datalist from big data-files for pt-ID, return matrix containg total data for specific pat_id
data_matrix <- function(pat_id, database_name) {
  
  ## -> connect to SQL-database, return matrix with data for said patient
  query <- paste("SELECT * FROM [PICURED].[dbo].", database_name, " WHERE [pat_hosp_id]= ", pat_id)
  connection_string <- sql_connect()
  total_data <- sqlQuery(connection_string, query)
  if( (grepl("ERROR", total_data[2]) || is.na(total_data[1,1])) == TRUE){
    print("A query-error has occured or the query returned empty")
    print(total_data)
  } else return(total_data)
}

## --- merge both datasets according to time, return matrix with sets combined
merge_data <- function(pat_id, nirs_data, total_data_pt) {
  nirs_data$timestamp <- format(as.POSIXct(nirs_data$timestamp, format="%Y-%m-%d %H:%M:%S"), usetz=FALSE)
  total_data_pt$pat_datetime <- format(as.POSIXct (total_data_pt$pat_datetime, format="%Y-%m-%d %H:%M:%S"), usetz=FALSE)
  
  # round timestamp to the minute
  nirs_data$timestamp <- lubridate::floor_date(as.POSIXct(nirs_data$timestamp), unit = "minute")
  #nirs_data$timestamp <- format(as.POSIXct(nirs_data$timestamp, format="%Y-%m-%d %H:%M:%S"), usetz=FALSE)
  #remove duplicate times 
  nirs_data <- nirs_data[!duplicated(nirs_data$timestamp),]
  
  
  pt_complete <- matrix(nrow=0, ncol=(ncol(total_data_pt) + 4))
  colnames(pt_complete) <- c(colnames(total_data_pt),"nirs_l", "nirs_r", "channel_1", "channel_2" )
  
  progress = txtProgressBar(min=0, max=nrow(nirs_data))
  succes <- 0
  failure <- 0
  empty <- 0
  nirs_data$timestamp <- as.POSIXct(nirs_data$timestamp, format="%Y-%m-%d %H:%M:%S")
  total_data_pt$pat_datetime <- as.POSIXct (total_data_pt$pat_datetime, format="%Y-%m-%d %H:%M:%S")
  
  
  for (x in 1:nrow(nirs_data)) {
    setTxtProgressBar(progress, x)
    if( is.na(nirs_data[x,2]) == FALSE){
      match_row <- unlist(sapply(nirs_data$timestamp[x], function(check_sim_time) grep(check_sim_time, total_data_pt$pat_datetime)))
      #cat("match:", match_row)
      # filter for first match -> time 00:00:00 at each day results in all entries for said day
      match_row <- match_row[1]
      # cat("Match_1: ")
      
      if(is.na(match_row) == FALSE) {
        
        nirs_l <- nirs_data$nirs_l[x]
        nirs_r <- nirs_data$nirs_r[x]
        channel_1 <- nirs_data$channel_1[x]
        channel_2 <- nirs_data$channel_2[x]
        
        succes <- succes+1
        addition <- data.frame("nirs_l"=nirs_l, "nirs_r"=nirs_r, "channel_1"=channel_1, "channel_2"=channel_2)
        new_row <- cbind(total_data_pt[match_row,], addition)
        
        ## --- Optioneel: rbind(total_data, nirs_l, nirs_r) -> hangt vanaf hoe groot de database gaat worden. 
        
        pt_complete <- rbind(pt_complete, new_row)
        #cat("row", x, "of", nrow(nirs_data), "has been matched \n")
        #} else cat("No match was found for row", x, " of ", nrow(nirs_data), "\n")
      } else {
        #cat("No match was found for row", x, "\n") 
        failure <- failure+1
      }
      
      ## No timestamp-match has been found; what to do? Add 1 to 'failed timestamps' -> print with merger-info? Or add NA's?
      
      # do nothing, the datapoint is useless.
    } #cat("This row does not contain a timestamp")
    # new_result <- data.frame("pat_id"=pat_id, "succes" = succes, "failure"=failure, "empty"=empty)
    # result_pt <- rbind(result_pt, new_result)
  }
  cat("\n Merger complete: \n Succes: ", succes, "\n Failure: ", failure)
  return(pt_complete)
  close(progress)
  
}

## --- Upload total pt-data to a specific sql-table
upload_total_data <- function(total_data, sql_server_name) {
  total_data[,2] <- format(as.POSIXct(total_data[,2], format="%Y-%m-%d %H:%M:%S"), usetz=FALSE)
  total_data[,3] <- format(as.POSIXct(total_data[,3], format="%Y-%m-%d %H:%M:%S"), usetz=FALSE)
  
  conn_id <- sql_connect()
  
  if(nrow(total_data) != 0){
    progress = txtProgressBar(min=0, max=nrow(total_data))
    for (x in 1:nrow(total_data)){
      setTxtProgressBar(progress, x)
      for(y in 1:ncol(total_data[x,])){
        if(is.na(total_data[x,y]==TRUE)){
          total_data[x,y] <- 'NULL'
        } else total_data[x,y] <- paste("'",total_data[x,y],"'", sep="")
      } 
      
      query <- paste("INSERT INTO [PICURED].[dbo].", sql_server_name, " VALUES (", paste(total_data[x,] , collapse=",") ,")", sep="")
      sqlQuery(conn_id, query)
      if( (grepl("ERROR", total_data[2]) || is.na(total_data[1,1])) == TRUE){
        print("A query-error has occured or the query returned empty")
        print(total_data)
        stop()
      } 
      
    }
    close(progress)
    cat("Insertion succesfull")
  } else cat ("\n No data to be inserted")
}


read_nirs <- function(file_loc){
  if(is.null(file_loc)==FALSE){
    data <- read.table(file=file_loc, header=FALSE, fill=TRUE, skipNul=TRUE)
  } else {
    print("no file selected") 
    
  }
  data_sorted <- matrix(nrow=0, ncol=5)
  
  n_col_set <- as.numeric(ncol(data))
  colnames(data_sorted) <- c("timestamp", "nirs_l", "nirs_r", "channel_1", "channel_2")
  #laatste 3 columns is statische data (nirs-probe type?)
  if(NCOL(data) == "51") {
    data_sorted <- extract_data_51(data)
  } else if (NCOL(data) == "34") {
    data_sorted <- extract_data_34(data)
  } else if (NCOL(data) == "18"){
    data_sorted <- extract_data_18(data) 
  }  else if (NCOL(data) == "52") {
    data_sorted <- extract_data_51(data)
    #The column-values for NIRS are similar to the '51-edition'
    
  } else {
    cat('error, unknown number of columns in file ', file_loc, '\n')
    return("error, file not read")
  }
  return(data_sorted)
}

extract_data_34 <- function(data){
  data_34 <- matrix(nrow=0, ncol=5)
  colnames(data_34) <- c("timestamp", "nirs_l", "nirs_r", "channel_1", "channel_2")
  for (x in 1:nrow(data)) {
    timestamp <- as.POSIXct(paste(data[x,1], data[x,2]), format="%d.%m.%y %H:%M:%S")
    nirs_l <- as.numeric(data$V3[x])
    nirs_r <- as.numeric(data$V10[x])
    channel_1 <- as.numeric(data$V17[x])
    channel_2 <- as.numeric(data$V24[x])
    
    new_row <- data.frame("timestamp"=timestamp, "nirs_l"=nirs_l, "nirs_r"=nirs_r, "channel_1"=channel_1, "channel_2"=channel_2)
    data_34 <- rbind(data_34, new_row)
  }
  return(data_34)
}

extract_data_51 <- function(data){
  data_51 <- matrix(nrow=0, ncol=5)
  colnames(data_51) <- c("timestamp", "nirs_l", "nirs_r", "channel_1", "channel_2")
  for (x in 1:nrow(data)) {
    timestamp <- as.POSIXct(paste(data[x,2], data[x,3]), format="%d.%m.%y %H:%M:%S")
    nirs_l <- as.numeric(data$V5[x])
    nirs_r <- as.numeric(data$V16[x])
    channel_1 <- as.numeric(data$V27[x])
    channel_2 <- as.numeric(data$V38[x])
    
    new_row <- data.frame("timestamp"=timestamp, "nirs_l"=nirs_l, "nirs_r"=nirs_r, "channel_1"=channel_1, "channel_2"=channel_2)
    data_51 <- rbind(data_51, new_row)
  }
  return(data_51)
}

extract_data_18 <- function(data){
  data_18 <- matrix(nrow=0, ncol=5)
  colnames(data_18) <- c("timestamp", "nirs_l", "nirs_r", "channel_1", "channel_2")
  for (x in 1:nrow(data)) {
    timestamp <- as.POSIXct(paste(data[x,1], data[x,2]), format="%d.%m.%y %H:%M:%S")
    nirs_l <- as.numeric(data[x,3])
    nirs_r <- as.numeric(data[x,7])
    channel_1 <- as.numeric(data[x,11])
    channel_2 <- as.numeric(data[x,15])
    
    new_row <- data.frame("timestamp"=timestamp, "nirs_l"=nirs_l, "nirs_r"=nirs_r, "channel_1"=channel_1, "channel_2"=channel_2)
    data_18 <- rbind(data_18, new_row)
  }
  return(data_18)
}

extract_data_19 <- function(data){
  data_19 <- matrix(nrow=0, ncol=5)
  colnames(data_19) <- c("timestamp", "nirs_l", "nirs_r", "channel_1", "channel_2")
  for (x in 1:nrow(data)) {
    timestamp <- as.POSIXct(paste(data[x,2], data[x,3]), format="%d.%m.%y %H:%M:%S")
    nirs_l <- as.numeric(data[x,4])
    nirs_r <- as.numeric(data[x,8])
    channel_1 <- as.numeric(data[x,12])
    channel_2 <- as.numeric(data[x,16])
    
    new_row <- data.frame("timestamp"=timestamp, "nirs_l"=nirs_l, "nirs_r"=nirs_r, "channel_1"=channel_1, "channel_2"=channel_2)
    data_19 <- rbind(data_19, new_row)
  }
  return(data_19)
}

edit_frame <- function(main_set, data_name) {
  main_set$pat_datetime <- as.numeric( (as.POSIXct(main_set$pat_datetime, format="%Y-%m-%d %T", origin="1970-01-01 00:00:00")) - (as.POSIXct(main_set$pat_datetime[1], format="%Y-%m-%d %T", origin="1970-01-01 00:00:00")))/60
  progress = txtProgressBar(min=0, max=nrow(main_set))
  for (x in 1:nrow(main_set)){
    setTxtProgressBar(progress, x)
    #cat("this is line",x, "\n")
    if (sum(main_set$nirs_l[x], main_set$nirs_r[x], main_set$channel_1[x], main_set$channel_2[x]) == 0){
      main_set <- main_set[-x,]
    }
    
    for (y in 6:ncol(main_set)){
      if (is.null(as.numeric(main_set[x,y])) == TRUE) {
        main_set[x,y] <- NA
        
      } else {
        main_set[x,y] <-     floor(as.numeric(gsub(",", ".", main_set[x,y])))
      }
    } 
    if (is.na(main_set$mon_ibp_mean[x]) == FALSE){
      if (is.na(main_set$mon_ibp_mean[x]) == FALSE && is.na(main_set$mon_ibp_sys[x]) == TRUE ) {
        main_set$mon_ibp_mean[x] <- NA
      } else if (main_set$mon_ibp_mean[x] > main_set$mon_ibp_sys[x]){
        main_set$mon_ibp_mean[x] <- (2*main_set$mon_ibp_dia[x] + main_set$mon_ibp_sys[x])/3
        
      } else if(main_set$mon_ibp_mean[x] < main_set$mon_ibp_dia[x]) {
        main_set$mon_ibp_mean[x] <- (2*main_set$mon_ibp_dia[x] + main_set$mon_ibp_sys[x])/3
      }
    }
    
  }
  close(progress)
  return(main_set)
}
