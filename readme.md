Repository for scripts and documents related to the AIM Analysis/Reporting Workflow.

Output file names should have a common format:

FO_Project_Step_systemdate.ext 

FO and Project are optional depending on the analysis, however a minimum of one is required. Maintaining some kind of administrative name at the beginning of the file is useful. 

For instance, the weights output file for NorCal could be:
ELFO_wgt_20161221.csv

The data explorer might be: 
ELFO_explorer_20161221.xlsx

RMarkdown:
ELFO_report_20161221.html

Recommend that you prescribe this as an output at the beginning of each script:

out.src<-"filepath"
out.filename<-"FO_Project" #the remainder of the filename can be scripted
