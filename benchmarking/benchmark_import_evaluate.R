library(xlsx)
library(dplyr)
library(rgdal)
library(tidyr)

##########################################################
#### GLOBAL VARIABLES ####
##########################################################
## Where is the TerrADat .gdb?
tdat.path <- paste0(getwd(), "/", "data")
if (!grepl(x = tdat.path, pattern = "/$")) {
  tdat.path <- paste0(tdat.path, "/")
}
## What is the filename of the TerrADat .gdb?
tdat.name <- "Terradat_data_8.17.15_complete.gdb"
## More sanitizing because this needs to be a .gdb file with the extension provided
if (!grepl(x = benchmarks.filename, pattern = "\\.[Gg][Dd][Bb]$")) {
  benchmarks.filename <- paste0(benchmarks.filename, ".gdb")
}

## Where are the indicator lookup table and benchmarks .xlsx?
data.path <- paste0(getwd(), "/", "data")
## A bit of sanitization to be safe
if (!grepl(x = data.path, pattern = "/$")) {
  data.path <- paste0(data.path, "/")
}
## What is the filename of the benchmarks .xlsx 
benchmarks.filename <- "TerrestrialAIM_DataAnalysis_Template.xlsx"
## Sanitization because this needs to be an .xlsx file with the extension provided
if (!grepl(x = benchmarks.filename, pattern = "\\.[Xx][Ll][Ss][Xx]$")) {
  benchmarks.filename <- paste0(benchmarks.filename, ".xlsx")
}
## What is the filename of the indicators lut?
tdat.indicators.lut.filename <- "tdat_indicator_lut.csv"
## Santization because this needs to be a .csv file with the extension provided
if (!grepl(x = tdat.indicators.lut.filename, pattern = "\\.[Cc][Ss][Vv]$")) {
  tdat.indicators.lut.filename <- paste0(tdat.indicators.lut.filename, ".csv")
}

## Where will the outputs be written to?
out.path <- paste0(getwd(), "/", "data")
## A bit of sanitization to be safe
if (!grepl(x = out.path, pattern = "/$")) {
  out.path <- paste0(out.path, "/")
}
## What is the project name for this effort. Often [STATE]_FO_[PROJECT], e.g. "CA_ELFO_report"
project.name <- "NM_TAFO_RGDNNM"


##########################################################
#### FUNCTIONS ####
##########################################################
## Reading in the benchmarks from the Data Explorer
read.benchmarks <- function(data.path = "", ## Path to the folder containing the Data Explorer with the benchmarks in it
                            benchmarks.filename = "", ## The filename of the Data Explorer workbook
                            indicator.lut, ## A lookup table with a column called "indicator.name" matching the values in the Data Explorer "Indicator" field and one called "indicator.tdat" with corresponding value for the indicators' names in TerrADat
                            indicator.lut.benchmarkfield = "indicator.name" ## In case you are ignoring the instructions for indicator.lut
){
  ## Sanitizing inputs because users can't be trusted
  if (!grepl(x = data.path, pattern = "/$")) {
    data.path <- paste0(data.path, "/")
  }
  if (!grepl(x = benchmarks.filename, pattern = "\\.[Xx][Ll][Ss][Xx]$")) {
    benchmarks.filename <- paste0(benchmarks.filename, ".xlsx")
  }
  ## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
  benchmarks.raw <- read.xlsx(file = paste0(data.path, benchmarks.filename),
                              sheetName = "Monitoring Objectives",
                              header = T,
                              stringsAsFactors = F)
  
  ## In case there's a "Classification" column where we'd prefer a "Category" column. This lets us maintain backwards compatibility with older iterations of the spreadsheet
  names(benchmarks.raw)[names(benchmarks.raw) %in% c("Classification")] <- "Evaluation.Category"
  
  ## Strip out the extraneous columns and rows, which includes if they left the example in there
  benchmarks <- benchmarks.raw[!grepl(x = benchmarks.raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks.raw$Indicator), 1:12]
  
  ## Create the evaluations for the upper and lower limits of each benchmark.
  ## The way the spreadsheet is configured, there should be no rows without both defined
  benchmarks$eval.string.lower <- paste(benchmarks$Lower.Limit, benchmarks$LL.Relation)
  benchmarks$eval.string.upper <- paste(benchmarks$UL.Relation, benchmarks$Upper.Limit)
  
  ## Create an evaluation string for future use with the required proportion and its relationship
  benchmarks$eval.string.proportion[!is.na(benchmarks$Required.Proportion)] <- paste(benchmarks$Proportion.Relation[!is.na(benchmarks$Required.Proportion)], benchmarks$Required.Proportion[!is.na(benchmarks$Required.Proportion)])
  
  ## For each benchmark add in the name of the field in TerrADat that corresponds
  benchmarks <- merge(x = benchmarks, y = indicator.lut, by.x = "Indicator", by.y = indicator.lut.benchmarkfield)
  
  return(benchmarks)
}

## A function that evaluates a parsed text string like the ones in $eval.string.upper and lower. Used in a lapply() later. Probably replaceable with parse() %>% eval() there though
parser <- function(string){
  return(eval(parse(text = string)))
}

##########################################################
#### IMPORTING DATA ####
##########################################################
## Get TerrADat imported
tdat.terrestrial.spdf <- readOGR(dsn = paste0(tdat.path, tdat.name), layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
tdat.remote.spdf <- readOGR(dsn = paste0(tdat.path, tdat.name), layer = "SV_IND_REMOTESENSING", stringsAsFactors = F)
tdat.prj <- proj4string(tdat.terrestrial.spdf)
tdat.spdf <- merge(tdat.terrestrial.spdf, tdat.remote.spdf)
tdat <- tdat.spdf@data

## Import the lookup table for indicators in TerrADat versus in the benchmark spreadsheet
indicator.lut <- read.csv(paste0(data.path, tdat.indicators.lut.filename), stringsAsFactors = F)

## Import benchmarks using the function for the spreadsheet format
benchmarks <- read.benchmarks(data.path, benchmarks.filename, indicator.lut)


##########################################################
#### ATTRIBUTING AND EVALUATING ####
##########################################################
## Slicing the data frame from terradat.spdf
## This step depends on how you're attributing the points with evaluation strata.
## If they aren't constrained somehow to the project you intend, then something along these lines needs to happen
tdat <- tdat[grepl(x = tdat$ProjectName, pattern = "rgdnnm", ignore.case = T),]

## To properly assign, see eval_stratum_attribution_functions.R
tdat$Evaluation.Stratum <- "Loamy"

## Strip out the ones without evaluation strata
tdat <- tdat[!is.na(tdat$Evaluation.Stratum),]

## Making a tall version of the TerrADat data frame
## Indicators listed in order of appearance in TerrADat, line breaks inserted at thematic breaks
tdat.tall <- gather(tdat, Indicator, Value,
                    ## Terrestrial AIM values first
                    GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,
                    BareSoilCover_FH,TotalFoliarCover_FH,
                    NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,
                    NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,
                    InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,
                    InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,
                    SagebrushCover_AH,
                    WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,
                    NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,
                    InvPlantCover_AH,
                    InvPlant_NumSp,
                    SoilStability_All,SoilStability_Protected,SoilStability_Unprotected,
                    ## Remote sensing values
                    HerbLitterCover_FH,WoodyLitterCover_FH,EmbLitterCover_FH,TotalLitterCover_FH,
                    RockCover_FH,BiologicalCrustCover_FH,VagrLichenCover_FH,LichenMossCover_FH,
                    DepSoilCover_FH,WaterCover_FH,
                    NonInvPerenForbCover_FH,NonInvAnnForbCover_FH,NonInvPerenGrassCover_FH,NonInvAnnGrassCover_FH,
                    NonInvSucculentCover_FH,NonInvShrubCover_FH,NonInvSubShrubCover_FH,NonInvTreeCover_FH,
                    InvPerenForbCover_FH,InvAnnForbCover_FH,InvPerenGrassCover_FH,InvAnnGrassCover_FH,
                    InvSucculentCover_FH,InvShrubCover_FH,InvSubShrubCover_FH,InvTreeCover_FH,
                    SageBrushCover_FH)

## Merge the tall TerrADat with the benchmark information
tdat.tall.benched <- merge(x = tdat.tall, y = benchmarks[, c("Evaluation.Stratum", "indicator.tdat", "Evaluation.Category", "eval.string.lower", "eval.string.upper")], by.x = c("Evaluation.Stratum", "Indicator"), by.y = c("Evaluation.Stratum", "indicator.tdat"))

## Create parseable evaluation strings
tdat.tall.benched$eval.string.lower <- paste0(tdat.tall.benched$eval.string.lower, tdat.tall.benched$Value)
tdat.tall.benched$eval.string.upper <- paste0(tdat.tall.benched$Value, tdat.tall.benched$eval.string.upper)

## Parse the strings to determing if the value falls within the upper and lower bounds for that benchmark evaluation category
tdat.tall.benched$meeting <- lapply(tdat.tall.benched$eval.string.lower, parser) %>% unlist() & lapply(tdat.tall.benched$eval.string.upper, parser) %>% unlist()

## Because all the benchmark evaluation categories should be mutually exclusive, applying the vector from $meeting should result in one row per indicator per plot
## Also restricting this to the relevant columns that are required for the next step
output <- tdat.tall.benched[tdat.tall.benched$meeting, c("PrimaryKey", "PlotID", "Evaluation.Stratum", "Indicator", "Value", "Evaluation.Category")]


##########################################################
#### WRITING OUT DATA ####
##########################################################
## Writing out the benchmarked points in tall/long format 
write.csv(output, paste0(out.path, project.name, "_eval_", strftime(Sys.Date(), "%Y%m%d"), ".csv"))

## Writing out the benchmarks table with relevant columns in R-friendly format for later use
write.csv(benchmarks[, c("Management.Question", "Benchmark.Source", "Evaluation.Stratum", "Indicator", "Evaluation.Category", "eval.string.lower", "eval.string.upper", "eval.string.proportion")],
          paste0(out.path, project.name, "_benchmarks_", strftime(Sys.Date(), "%Y%m%d"), ".csv"))