library(tidyverse)
library(timetk)
library(RODBC)

#Generate looped file-list
file_list <- list.files("H:\\NIRS-R\\Signalbase\\results\\1101-1200\\")

for (x in 1:NROW(file_list)){
    data_frame <- file_import(file_list[x])             # Import the file to a dataframe
  # frame is already cleaned
  
  if (nrow(data_frame) == 0){ 
    cat("No data to upload in file nr:", x)
  } else {                            # No relevant data in this file
    export_frame_test(as.data.frame(data_frame), "[NIRS_Ruben_compl]")    
  }# Export based on column data! Elaborate query-construction
}

# -- Testing -- #


1101-1200
