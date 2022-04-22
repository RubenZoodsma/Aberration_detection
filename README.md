# PICU Aberration detection algorithm

A separate algorithm was created for both the subgroup containing infants bearing an average SpO2 >90- and <90. Respectively, each algorithm may be found in the 'high-algorithm' and 'low-algorithm' files.

Not included are the subsequent Support-Vector-Machines (SVM) which are needed for prediction. 
The 'functions'-file contains all functions needed for both the prediction- as well as the visualization. Required packages are stated at the top of the file.


Included also are:
- Readme + scripts for automated reading- and uploading of CFM files to a SQL database
- Scripts for training of a one-class Support vector machine
