library(tidyverse)
library(RODBC)

file_import <- function(file_path){
  file_path <- paste("Standard path to the folder where files are included in", file_path, sep="")
  data <- read_tsv(file_path, col_names = TRUE, skip = 5)
  # remove patient-name and period columns
  data <- data[,-2:-3]
  data$Date <- paste(data$Date, data$Time)
  data <- data[,-3]
  if (ncol(data)==2){
    return(matrix(data=NA,nrow=0, ncol=1))
  } else {
    clean_frame <- data %>% filter( ! is.na(data[,3:ncol(data)]) )
    names <- colnames(clean_frame)
    names[1] <- "pat_hosp_id"
    names[2] <- "pat_datetime"
    colnames(clean_frame) <- names
    
    clean_frame[,3:ncol(clean_frame)] <- round(clean_frame[,3:ncol(clean_frame)], digits=2)
    clean_frame <- as.data.frame(lapply(clean_frame, function(x){gsub("NaN", "NULL",x)}))
    
    return(clean_frame)
    
  }
}


export_frame_test <- function(total_data, db_name){
  progress = txtProgressBar(min=0, max=nrow(total_data))
  
  conn_id <- sql_connect()
  
  for (x in 1:nrow(total_data)){
    setTxtProgressBar(progress, x)
    query <- query_construct(total_data[x,], db_name)
    #cat("\n query", x ,"is", query)
    sqlQuery(conn_id, query, errors = TRUE)
  }
  close(progress)
  close(conn_id)
}


sql_connect <- function(){
  result <- odbcConnect("PICURED", uid="**", pwd = "**")
  return(result)
}

query_construct <- function(data_row, db_name){
  query_colname <- colnames(data_row[1])
  for (x in 2:ncol(data_row)){
    query_colname <- paste(query_colname, ",", colnames(data_row[x]))
  }
  row <- paste(data_row, collapse="','", sep="")
  query <- paste("INSERT INTO [PICURED].[dbo].", db_name,
                 "(", query_colname, ") ",
                 "VALUES ('", row ,"')", sep="")
  query <- gsub("'NULL'", "NULL", query)
  return(query)
}

