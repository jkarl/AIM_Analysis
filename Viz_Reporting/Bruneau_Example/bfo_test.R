## BRUNEAU
## NOTE: Run the script functions.r before running this or you will have regrets

## Where is the TerrADat .gdb?
tdat.path <- paste0(getwd(), "/", "data") %>% sanitizer(type = "filepath")
## What is the filename of the TerrADat .gdb?
tdat.name <- "TerrADatExport_19Jan2016.gdb" %>% sanitizer(type = "gdb")

## Where are the indicator lookup table and benchmarks .xlsx?
data.path <- paste0(getwd(), "/", "data/bfo")  %>% sanitizer(type = "filepath")

## What is the filename of the benchmarks .xlsx?
benchmarks.filename <- "DataExplorer_AnalysisSprint_BruneauFO-FAKE.xlsx" %>% sanitizer(type = "xlsx")

## What is the filename of the indicators lut?
tdat.indicators.lut.filename <- "tdat_indicator_lut.csv" %>% sanitizer(type = "csv")

## Where will the outputs be written to?
out.path <- paste0(getwd(), "/", "data/bfo") %>% sanitizer(type = "filepath")

## What is the project name for this effort. Often [STATE]_FO_[PROJECT], e.g. "CA_ELFO_report"
project.name <- "ID_BFO_test"

##########################################################
#### IMPORTING DATA ####
##########################################################
## Get TerrADat imported
tdat.terrestrial.spdf <- readOGR(dsn = paste0(tdat.path, tdat.name), layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
tdat.remote.spdf <- readOGR(dsn = paste0(tdat.path, tdat.name), layer = "SV_IND_REMOTESENSING", stringsAsFactors = F)
tdat.prj <- proj4string(tdat.terrestrial.spdf)
tdat.spdf <- merge(tdat.terrestrial.spdf, tdat.remote.spdf)
# tdat <- tdat.spdf@data

## Import the lookup table for indicators in TerrADat versus in the benchmark spreadsheet
indicator.lut <- read.csv(paste0(data.path, tdat.indicators.lut.filename), stringsAsFactors = F)

## Import benchmarks using the function for the spreadsheet format
benchmarks <- read.benchmarks(data.path, benchmarks.filename, indicator.lut)

## Read in the SDD contents
sdd.raw <- sdd.reader(src = data.path, sdd.src = c("BFO_2015-2019_SDD_122116.gdb"), func = "readogr")

## Get the SPDFs that define the reporting units and evaluation categories
reporting.unit.spdf <- sdd.raw$sf$`BFO_2015-2019_SDD_122116.gdb`
eval.cat.spdf <- sdd.raw$strata$`BFO_2015-2019_SDD_122116.gdb`
## Making sure these match the values in the benchmarks table
## This part is going to be such a pain
eval.cat.spdf@data$DMNNT_STRTM <- str_replace_all(eval.cat.spdf@data$DMNNT_STRTM, "_", " ")
eval.cat.spdf@data$DMNNT_STRTM <- str_replace_all(eval.cat.spdf@data$DMNNT_STRTM, "brush", "")
eval.cat.spdf@data$DMNNT_STRTM <- str_replace_all(eval.cat.spdf@data$DMNNT_STRTM, " & ", "/")

## Attributing with the reporting units
tdat.spdf.attribute <- attribute.shapefile(points = tdat.spdf,
                                           shape = reporting.unit.spdf,
                                           attributefield = "TERRA_SAMPLE_FRAME_ID",
                                           newfield = "Reporting.Unit")

## Attributing with the evaluation categories
tdat.spdf.attribute1 <- attribute.shapefile(points = tdat.spdf.attribute,
                                             shape = eval.cat.spdf,
                                             attributefield = "DMNNT_STRTM",
                                             newfield = "Evaluation.Stratum")
tdat.spdf.attribute.2 <- tdat.spdf.attribute
tdat.spdf.attribute.2$Evaluation.Stratum <- "Study Area"

tdat.spdf.attribute <- rbind(tdat.spdf.attribute.1, tdat.spdf.attribute.2)

## Weighting
weights.first.pass <- weighter(sdd.import = sdd.raw,
                               tdat = tdat.spdf@data)

## Evaluating
benchmarks.first.pass <- benchmarker(benchmarks = benchmarks,
                                     tdat = tdat.spdf.attribute@data)

## Analyzing
analysis.first.pass <- analyzer(evaluated.points = benchmarks.first.pass,
                                weights = weights.first.pass,
                                tdat = tdat.spdf.attribute@data)

write.csv(analysis.first.pass, paste(out.path, "BFO_2015_test_analysis.csv", sep = "/"))
write.csv(weights.first.pass$strata.weights, paste(out.path, "BFO_2015_test_stratum_weights.csv", sep ="/"))
write.csv(weights.first.pass$point.weights, paste(out.path, "BFO_2015_test_point_weights.csv", sep = "/"))
