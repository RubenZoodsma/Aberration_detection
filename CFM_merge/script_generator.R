# Script loader
script <- matrix(nrow=0, ncol=2)
new_row <- c("Section", "Presets")
script <- rbind(script, new_row)
new_row <- c("Loadpreset", "Ruben1")
script <- rbind(script, new_row)
new_row <- c("EndSection", "")
script <- rbind(script, new_row)

script_load <- c("LoadScriptfile", "Insert scriptfile-NAME + extension here")
file_list_brm_files <- list.files("File location of exporteable files")
for (x in 1:NROW(file_list_brm_files)){
  file_name <- gsub('.{4}$',"",file_list_brm_files[x])
  new_row <- c("scriptArgument1", file_name)
  script <- rbind(script, new_row)
  script <- rbind(script, script_load)
}

write.table(script, file="export-location of the desired script file + extension", sep="\t", row.names=FALSE, col.names = FALSE, quote = FALSE)

