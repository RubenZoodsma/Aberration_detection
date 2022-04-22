PICU Aberration detection algorithm
A separate algorithm was created for both the subgroup containing infants bearing an average SpO2 >90- and <90. Respectively, each algorithm may be found in the 'high-algorithm' and 'low-algorithm' files.

Not included are the subsequent Support-Vector-Machines (SVM) which are needed for prediction. The 'functions'-file contains all functions needed for both the prediction- as well as the visualization. Required packages are stated at the top of the file.

Included also are:

- Readme + scripts for automated reading- and uploading of CFM files to a SQL database
- Scripts for training of a one-class Support vector machine




--------------
CFM-Merge

Signalbase script generator
For CFM export to SQL three things are required:
 1) CFM export to a .tsv (tab-separated-value) format
 2) .tsv import in R
 3) R upload to SQL

1) CFM export to .tsv format
Signalbase allows for mass-export functions, through a script-file where each CFM-file can be automatically exported.
The  signalbase "preset" can be used to generate the format at which the .tsv file is exported. 
The names of the containers in both Low- and high-frequency values can be used, where best is to name them according to later SQL column headers.
Any parameters not present in the actual datafile will be ignored upon export; no column with NULL values will therefore be created.

An R-script generator was used to generate the signalbase-script.
The variable 'scriptargument1' can be set to the name of the CFM-file, which will be passed on to the actual exported .tsv-filename.
The tsv-filename can consequently be used to identify the patient-ID and used in the SQl-upload.


2) .tsv import in R
With all .tsv files in a folder, a loop was generated which runs over the present files.
Files are read, adapted where needed (rounding of parameters for example) and prepared for upload.

3) R upload to SQL
For uploading, an RODBC link to the SQL-database was made. 
The at 2 generated dataframe was row-wise uploaded, where a SQL-query was generated using the column headers as derived from the signalbase-export-format.
The query will then be presented to SQL and inserted to the pre-defined table.
