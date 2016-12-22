Repository for scripts and documents related to the AIM Analysis/Reporting Workflow.

Output file names should have a common format:
  State_FO_Project_Step_systemdate.ext 

State, FO, and Project are optional depending on the analysis, however a minimum of one is required. Maintaining some kind of administrative name at the beginning of the file is useful. "Project" is for our own internal tracking at this point and doesn't necessarily refer to the sample design, so it could be something like "2017_GrazAllots" as long as it's consistent between steps in the process.

For instance, the weights output file for NorCal could be:
  CA_ELFO_wgt_20161221.csv

The data explorer might be: 
  CA_ELFO_explorer_20161221.xlsx

RMarkdown:
  CA_ELFO_report_20161221.html

Recommend that you prescribe this as an output at the beginning of each script:
  out.src <- "filepath"
  # Making sure that out.src ends in a "/" so it can be safely assumed later
  if (!grepl(x = out.src, pattern = "/$")) {
  out.src <- paste0(out.src, "/")
  }
  out.filename <- "Sate_FO_Project"
  # The code to create the output filename in this case would be:
  paste(out.filename, "wgt", strftime(Sys.Date(), "%Y%m%d"), sep = "_")
  # Or to create the full filepath to write out to a .csv:
  write.csv(output.df, paste0(out.src, out.filename, "_wgt_", strftime(Sys.Date(), "%Y%m%d"), ".csv")
