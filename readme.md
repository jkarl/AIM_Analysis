Repository for scripts and documents related to the AIM Analysis/Reporting Workflow.

Output file names should have a common format:
  FO_Project_Step_systemdate.ext 

FO and Project are optional depending on the analysis, however a minimum of one is required. Maintaining some kind of administrative name at the beginning of the file is useful. "Project" is for our own internal tracking at this point and doesn't necessarily refer to the sample design, so it could be something like "2017_GrazAllots" as long as it's consistent between steps in the process.

For instance, the weights output file for NorCal could be:
  ELFO_wgt_20161221.csv

The data explorer might be: 
  ELFO_explorer_20161221.xlsx

RMarkdown:
  ELFO_report_20161221.html

Recommend that you prescribe this as an output at the beginning of each script:
  out.src<-"filepath"
  out.filename<-"FO_Project" #the remainder of the filename can be scripted

The code to create the output filename in this case would be:
  paste(out.filename, "wgt", strftime(Sys.Date(), "%Y%m%d"), sep = "_")

Or to create the full filepath to write out to a .csv (assuming your out.src string doesn't end in "/"):
  write.csv(output.df, paste0(out.src, "/", out.filename, "_wgt_", strftime(Sys.Date(), "%Y%m%d"), ".csv")
