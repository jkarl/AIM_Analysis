##########################################################
#### FUNCTIONS ####
##########################################################
library(tidyverse)
library(stringr)
library(xlsx)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(arcgisbinding)
library(spsurvey)
arc.check_product()

## A function that evaluates a parsed text string like the ones in $eval.string.upper and lower. Used in a lapply() later. Probably replaceable with parse() %>% eval() there though
safe.parser <- function(string){
  output <- safely(eval(parse(text = string)))
  return(output[[1]])
}

## For those strange occasions when the safe version gives you errors?
parser <- function(string){
  output <- eval(parse(text = string))
  return(output)
}

## A function to make sure that input strings are correctly formatted for filepaths, .gdb filenames, .xlsx filenames, .csv filenames, and .shp filenames
sanitizer <- function(string, type){
  switch(type,
         filepath = {
           if (!grepl(x = string, pattern = "/$") & !grepl(x = string, pattern = "\\\\$")) {
             string <- paste0(string, "/")
           }
         },
         gdb = {
           if (!grepl(x = string, pattern = "\\.[Gg][Dd][Bb]$")) {
             string <- paste0(string, ".gdb")
           }
         },
         xlsx = {
           if (!grepl(x = string, pattern = "\\.[Xx][Ll][Ss][Xx]$")) {
             string <- paste0(string, ".xlsx")
           }
         },
         csv = {
           if (!grepl(x = string, pattern = "\\.[Cc][Ss][Vv]$")) {
             string <- paste0(string, ".csv")
           }
         },
         shp = {
           if (!grepl(x = string, pattern = "\\.[Ss][Hh][Pp]$")) {
             string <- paste0(string, ".shp")
           }
         }
  )
  return(string)
}


#### ATTRIBUTING POINTS WITH VALUES ####
## Functions for adding a column/field called Evaluation.Stratum (by default) to points
## for use in benchmark evaluations

## attribute.shapefile() adds from a specified field in a shapefile, either points or polygons
## attribute.list() adds based on a simple lookup table of PlotID and Evaluation.Stratum
## attribute.field() adds from a lookup table based on values found in the fields of the points fed to it


## Shapefile attribute extraction function where the shapefile attribute table contains the values to assign
attribute.shapefile <- function(points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                                data.path = "", ## If the shape is in a .gdb feature class then this should be the full path, including the file extension .gdb. If the SPDF is already made, do not specify this argument
                                shape = "", ## The name of the shapefile or feature class !!!OR!!! an SPDF
                                attributefield = "", ## Name of the field in the shape that specifies the attribute to assign to the points
                                newfield = "Evaluation.Stratum", ## Name of the new field in the output to assign the values from attributefield to
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")){
  ## Strip the file extension from shape, just in case it was there
  if (is.character(shape)) {
    shape <- str_replace(shape, pattern = "\\.[Ss][Hh][Pp]$", replacement = "") 
  }
  ## If this is coming from a geodatabase, extract the shapefile appropriately. Otherwise read in the .shp
  if (grepl(x = data.path, pattern = "\\.[Gg][Dd][Bb]$")) {
    shape.spdf <- readOGR(dsn = data.path, layer = shape, stringsAsFactors = F) %>% spTransform(projection)
  } else if (data.path != "") {
    shape.spdf <- readOGR(dsn = paste0(data.path, "/", shape, ".shp"), layer = shape, stringsAsFactors = F) %>% spTransform(projection)
  } else if (class(shape)[1] == "SpatialPointsDataFrame" | class(shape)[1] == "SpatialPolygonsDataFrame") {
    shape.spdf <- shape %>% spTransform(projection)
  }
  ## Make sure that the points also adhere to the same projection
  points <- points %>% spTransform(projection)
  
  ## Because there might be overlap between polygons with different evaluation stratum identities, we'll check each eval stratum independently
  for (n in unique(shape.spdf@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.points <- points
    ## Get the data frame from checking the points against the current subset of the polygons
    over.result <- over(current.points, shape.spdf[shape.spdf@data[, attributefield] == n,])
    ## Add the values to the newfield column
    current.points@data[, newfield] <- over.result[, attributefield]
    ## Store the results from this loop using the naming scheme "over__[current value of n]" with spaces replaced with underscores to prevent parsing errors later
    assign(x = str_replace(paste0("over__", n), " ", "_"),
           ## Only keep the ones that actually took on an attribute
           value = current.points[!is.na(current.points@data[, newfield]),])
  }
  ## List all the objects in the working environment that start with "over__" and rbind them into a single SPDF
  attributed.spdfs <- ls()[grepl(x = ls(), pattern = "^over__")]
  output <- eval(parse(text = paste0("rbind(`", paste(attributed.spdfs, collapse = "`,`") ,"`)")))
  
  return(output)
}

## TODO: make the lut and list functions pull from and assign to arbitrary field names

## Function to import .csv that attributes the plots matching the PLOT column with evaluation strata from the EVAL.STRATUM column
attribute.list <- function(points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                           datapath = "", ## Only specify if you need to read in the lookup table from a file
                           lut = "", ## Either the filename !!!OR!!! a data frame. Either way it needs a (PlotID | PrimaryKey) column and an Evaluation.Stratum column
                           dropNA = T ## Strip out points that did not qualify for an attribution stratum
){
  ## Sanitize the input
  datapath <- str_replace(datapath, pattern =  "/$", replacement = "")
  if ((datapath == "" | is.null) & is.data.frame(lut)) {
    ## The format of the lookup table needs to include Evaluation.Stratum and one of either PlotID or PrimaryKey
    if (grepl(x = paste(names(lut), collapse = ""), pattern = "PrimaryKey")) {
      lut <- lut[, c("PrimaryKey", "Evaluation.Stratum")] ## Preferably use the PrimaryKey if both PlotID and PrimaryKey are present
    } else if (grepl(x = paste(names(lut), collapse = ""), pattern = "PlotID")) {
      lut <- lut[, c("PlotID", "Evaluation.Stratum")] 
    }
  } else if (!is.data.frame(lut) & grepl(x = lut, pattern = "\\.[Cc][Ss][Vv]$")) {
    lut <- read.csv(paste0(datapath, "/", lut), stringsAsFactors = F)
    if (grepl(x = paste(names(lut), collapse = ""), pattern = "PrimaryKey")) {
      lut <- lut[, c("PrimaryKey", "Evaluation.Stratum")]
    } else if (grepl(x = paste(names(lut), collapse = ""), pattern = "PlotID")) {
      lut <- lut[, c("PlotID", "Evaluation.Stratum")] 
    }
  }
  output <- merge(points, lut)
  if (dropNA) {
    output <- output[!is.na(output$Evaluation.Stratum),]
  }
  return(output)
}

## TODO: Function to import .csv or .xlsx to function as a lookup table with columns for TerrADat/MS field, field values, and evaluation strata
# attribute.field <- function(points = SpatialPointsDataFrame(coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
#                             data.path = "", ## Only specify if you need to read in the lookup table from a file
#                             lut = "", ## Either the filename !!!OR!!! a data frame. Either way it needs the columns Attribute.Field, Field.Value, Evaluation.Stratum
#                             dropNA = T, ## Strip out points that did not qualify for an attribution stratum
# ){
#   ## Sanitize the input
#   data.path <- str_replace(data.path, pattern =  "/$", replacement = "")
#   if ((data.path == "" | is.null) & is.data.frame(lut)) {
#     ## The format of the lookup table needs to include these three fields
#     lut <- lut[, c("Attribute.Field", "Field.Value", "Evaluation.Stratum")]
#   } else if (!is.data.frame(lut) & grepl(x = lut, pattern = "\\.[Cc][Ss][Vv]$")) {
#     lut <- read.csv(paste0(datapath, "/", lut), stringsAsFactors = F)[, c("Attribute.Field", "Field.Value", "Evaluation.Stratum")]
#   }
#   for (n in 1:nrow(lut)) {
#     points$Evaluation.Stratum[points[, lut$Attribute.Field[n]] == lut$Attribute.Value[n]] <- lut$Evaluation.Stratum[n]
#   }
#   output <- points
#   if (dropNA) {
#     output <- output[!is.na(output$Evaluation.Stratum),]
#   }
#   return(output)
# }



#################################
### SPECIALIZED FUNCTIONS ###
#################################
## Reading in the benchmarks from the Data Explorer
## TODO: Add capitalization sanitization stuff
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

## Applying the benchmarks to a TerrADat data frame.
benchmarker <- function(benchmarks, ## The data frame imported with read.benchmarks()
                        tdat, ## The data frame from TerrADat. It needs to already be attributed with evaluation strata
                        evalstratumfield = "Evaluation.Stratum" ## The field in tdat that contains the evaluation strata
){
  ## Sanitization as always
  names(benchmarks) <- str_to_upper(names(benchmarks))
  ## In case someone didn't read the instructions and fed in an SPDF
  if (class(tdat)[1] == "SpatialPointsDataFrame") {
    tdat <- tdat@data
  }
  tdat.fields.indicators.expected <- c('BareSoilCover_FH', 'TotalFoliarCover_FH',
                                'GapPct_25_50', 'GapPct_51_100', 'GapPct_101_200', 'GapPct_200_plus', 'GapPct_25_plus',
                                'NonInvPerenForbCover_AH', 'NonInvAnnForbCover_AH', 'NonInvPerenGrassCover_AH', 'NonInvAnnGrassCover_AH', 'NonInvAnnForbGrassCover_AH', 'NonInvPerenForbGrassCover_AH', 'NonInvSucculentCover_AH', 'NonInvShrubCover_AH', 'NonInvSubShrubCover_AH', 'NonInvTreeCover_AH',
                                'InvPerenForbCover_AH', 'InvAnnForbCover_AH', 'InvPerenGrassCover_AH', 'InvAnnGrassCover_AH', 'InvAnnForbGrassCover_AH', 'InvPerenForbGrassCover_AH', 'InvSucculentCover_AH', 'InvShrubCover_AH', 'InvSubShrubCover_AH', 'InvTreeCover_AH',
                                'SagebrushCover_AH',
                                'WoodyHgt_Avg', 'HerbaceousHgt_Avg', 'SagebrushHgt_Avg', 'OtherShrubHgt_Avg',
                                'NonInvPerenGrassHgt_Avg', 'InvPerenGrassHgt_Avg',
                                'InvPlantCover_AH', 'InvPlant_NumSp',
                                'SoilStability_All', 'SoilStability_Protected', 'SoilStability_Unprotected',
                                ## Remote sensing values
                                'HerbLitterCover_FH', 'WoodyLitterCover_FH', 'EmbLitterCover_FH', 'TotalLitterCover_FH', 'RockCover_FH', 'BiologicalCrustCover_FH', 'VagrLichenCover_FH', 'LichenMossCover_FH', 'DepSoilCover_FH', 'WaterCover_FH',
                                'NonInvPerenForbCover_FH', 'NonInvAnnForbCover_FH', 'NonInvPerenGrassCover_FH', 'NonInvAnnGrassCover_FH', 'NonInvSucculentCover_FH', 'NonInvShrubCover_FH', 'NonInvSubShrubCover_FH', 'NonInvTreeCover_FH',
                                'InvPerenForbCover_FH', 'InvAnnForbCover_FH', 'InvPerenGrassCover_FH', 'InvAnnGrassCover_FH', 'InvSucculentCover_FH', 'InvShrubCover_FH', 'InvSubShrubCover_FH', 'InvTreeCover_FH',
                                'SageBrushCover_FH')

  if (length(tdat.fields.indicators.expected[tdat.fields.indicators.expected %in% names(tdat)]) != length(tdat.fields.indicators.expected)) {
    print("These expected indicators weren't found in the tdat data frame")
    print(paste(tdat.fields.indicators.expected[!(tdat.fields.indicators.expected %in% names(tdat))], collapse = ", "))
    print("All of these are being dropped from consideration and the remaining indicators are being used")
  }
  ## Making a tall version of the TerrADat data frame
  ## Indicators listed in order of appearance in TerrADat, line breaks inserted at thematic breaks
  tdat.tall <- eval(parse(text = paste0("gather(tdat, Indicator, Value, ",
                paste(tdat.fields.indicators.expected[tdat.fields.indicators.expected %in% names(tdat)],collapse = ", ") %>% str_replace_all("'", ""),
                ")"
                )
         ))
  
  ## Strip down benchmarks to just the distinct ones that matter because sometimes the same benchmark appears for multiple reasons?
  benchmarks.distinct <- distinct(benchmarks[, c("MANAGEMENT.QUESTION", "EVALUATION.STRATUM", "INDICATOR.TDAT", "EVALUATION.CATEGORY", "EVAL.STRING.LOWER", "EVAL.STRING.UPPER")])
  
  ## Merge the tall TerrADat with the benchmark information
  tdat.tall.benched <- merge(x = tdat.tall,
                             y = benchmarks.distinct,
                             by.x = c(names(tdat.tall)[grepl(x = names(tdat.tall), pattern = evalstratumfield, ignore.case = T)], "Indicator"),
                             by.y = c("EVALUATION.STRATUM", "INDICATOR.TDAT"))
  
  ## Create parseable evaluation strings
  tdat.tall.benched$EVAL.STRING.LOWER <- paste0(tdat.tall.benched$EVAL.STRING.LOWER, tdat.tall.benched$Value)
  tdat.tall.benched$EVAL.STRING.UPPER <- paste0(tdat.tall.benched$Value, tdat.tall.benched$EVAL.STRING.UPPER)
  
  ## Parse the strings to determing if the value falls within the upper and lower bounds for that benchmark evaluation category
  tdat.tall.benched$meeting <- lapply(tdat.tall.benched$EVAL.STRING.LOWER, parser) %>% unlist() & lapply(tdat.tall.benched$EVAL.STRING.UPPER, parser) %>% unlist()
  
  names(tdat.tall.benched) <- str_to_upper(names(tdat.tall.benched))
  
  ## Because all the benchmark evaluation categories should be mutually exclusive, applying the vector from $meeting should result in one row per indicator per plot
  ## Also restricting this to the relevant columns that are required for the next step
  output <- tdat.tall.benched[tdat.tall.benched$MEETING, c("PRIMARYKEY", "PLOTID", "MANAGEMENT.QUESTION", "EVALUATION.STRATUM", "INDICATOR", "VALUE", "EVALUATION.CATEGORY")]
  names(output) <- str_to_upper(names(output))
  return(output)
}

## TODO: Should try to handle raster location/import either within sdd.reader() or as an independent function
## Reads in SDDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## sf is a list of sample frame SPDFs, pts is a list of point SPDFs, strata is a list of stratfication SPDFs
## The SPDFs are all named using the SDD filename provided in sdd.src so that output$sf$generic_design.gdb has the sample frame that corresponds to output$pts$generic_design.gdb
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to the same SDD source
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that SDD in the list
sdd.reader <- function(src = "", ## A filepath as a string
                       sdd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                       func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                       projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83 projection
){
  
  ## readOGR() wrapped in safely() so that it will return NULL instead of an error. I need this for the function
  safe.readOGR <- safely(readOGR, otherwise = NULL)
  
  ## Sanitization
  func <- str_to_upper(func)
  
  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    print("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }
  
  ## Only keeping the SDD filenames that actually exist in the src filepath provided
  sdd.src.exist <- sdd.src[sdd.src %in% list.files(path = src)]
  
  ## Reporting the filenames it couldn't find in the folder
  if (length(sdd.src) != length(sdd.src.exist)) {
    print(paste0("Couldn't find the following .gdb[s]: ", paste(sdd.src[!(sdd.src %in% list.files(path = src))], collapse = ", "))) 
  }
  
  switch(func,
         READOGR = {
           ## Looped so that it can execute across all the SDDs in the vector (if there are more than one)
           for (s in sdd.src.exist) {
             ## Read in the sample frame feature class inside the current SDD.
             sf <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                layer = "Terra_Sample_Frame",
                                stringsAsFactors = F)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe, but probably isn't necessary
             if (!is.null(sf)) {
               sf <- spTransform(sf, projection)
             }
             ## Stores the current sf SPDF with the name sf.[SDD name]
             assign(x = paste("sf", s, sep = "."), value = sf)
             
             #Read in the Strata
             strata <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Strtfctn",
                                    stringsAsFactors = F)[[1]]
             if (!is.null(strata)) {
               strata <- spTransform(strata, projection)
             }
             assign(x = paste("strata", s, sep = "."), value = strata)
             
             #Read in the Points
             points <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                    layer = "Terra_Sample_Points",
                                    stringsAsFactors = F)[[1]]
             if (!is.null(points)) {
               points <- spTransform(points, projection)
             }
             assign(x = paste("pts", s, sep = "."), value = points)
           }
         },
         ARCGISBINDING = {
           for (s in sdd.src.exist) {
             ## Identify/create the filepath to the sample frame feature class inside the current SDD
             sf <- paste(src, s, "Terra_Sample_Frame", sep = "/")
             ## Creates an SPDF with the name sf.[SDD name] using the filepath to that feature class
             assign(x = paste("sf", s, sep = "."),
                    value = sf %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)
             )
             
             #Read in the Strata
             #first check for strata
             ## Identify/create the filepath to the design stratification feature class inside the current SDD
             strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
             #this loads enough of the feature class to tell if there are strata
             strata <- strata %>% arc.open() %>% arc.select
             #check for strata, if there are, then we will finish loading the file. 
             if (nrow(strata) > 0) {
               ## Identify/create the filepath to the design stratification feature class inside the current SDD
               strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
               ## Creates an SPDF with the name strat.[SDD name] using the filepath to that feature class
               assign(x = paste("strata", s, sep = "."),
                      value = strata %>% arc.open() %>% arc.select %>%
                        SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()},
                                                 data = .))
               
             } else {
               ## If the stratification feature class is empty, we'll just save ourselves some pain and store NULL
               assign(x = paste("strata", s, sep = "."),
                      value = NULL)
             }
             
             #Read in the Points
             ## Identify/create the filepath to the design points feature class inside the current SDD
             pts <- paste(src, s, "Terra_Sample_Points", sep = "/")
             ## Creates an SPDF with the name pts.[SDD name] using the filepath to that feature class
             assign(x = paste("pts", s, sep = "."),
                    value = pts %>% arc.open() %>% arc.select %>%
                      #read in the feature class, notice the difference between Polygons and points (different function with different arguments needs)
                      SpatialPointsDataFrame(coords = {arc.shape(.) %>% arc.shape2sp()}))
           }
         }
  )
  
  ## Create a list of the sample frame SPDFs.
  ## This programmatically create a string of the existing object names that start with "sf." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it, creating a list from those SPDFs
  sf.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")], collapse = "`, `"), "`)")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")] %>% str_replace(pattern = "^sf\\.", replacement = "")
  
  ## Creating the named list of all the pts SPDFs created by the loop
  pts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = "`, `"), "`)")))
  names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")] %>% str_replace(pattern = "^pts\\.", replacement = "")
  
  ## Creating the named list of all the strata SPDFs created by the loop
  strata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")], collapse = "`, `"), "`)")))
  names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")] %>% str_replace(pattern = "^strata\\.", replacement = "")
  
  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)
  
  return(output)
}

## TODO: Needs to use PRIMARYKEYs instead of PLOTIDs because those are unique between sampling events when there is more than one
## TODO: Add in using the stratum value table if possible, because that should have the stratum area. Will only work with arcgisbinding :/
## This function produces point weights by design stratum (when the SDD contains them) or by sample frame (when it doesn't)
weighter <- function(sdd.import, ## The output from sdd.reader()
                     # tdat, ## The TerrADat data frame to use. This lets you throw the whole thing in or slice it down first, if you like
                     reporting.units.spdf = NULL, ## An optional reporting unit SPDF that will be used to clip the SDD import before calculating weights
                     reportingunitfield = "REPORTING.UNIT", ## If passing a reporting unit SPDF, what field in it defines the reporting unit[s]?
                     ## Keywords for point fate—the values in the vectors unknown and nontarget are considered nonresponses.
                     ## Assumes the following keywords are sufficient and consistent.
                     ## "UNK" and "NT" show up in certain SDDs even though the shapefle attributes spell out the keywords and they're invalid??? 
                     target.values = c("Target Sampled"),
                     unknown.values = c("Unknown",
                                        "UNK"),
                     nontarget.values = c("Non-Target",
                                          "NT",
                                          "Inaccessible",
                                          "Not Needed",
                                          NA),
                     ## These shouldn't need to be changed from these defaults, but better to add that functionality now than regret not having it later
                     fatefieldname = "final_desig", ## The field name in the points SPDF to pull the point fate from
                     pointstratumfieldname = "dsgn_strtm_nm", ## The field name in the points SPDF to pull the design stratum
                     designstratumfield = "dmnnt_strtm" ## The field name in the strata SPDF to pull the stratum identity from 
){
  ## Sanitization
  names(tdat) <- str_to_upper(names(tdat))
  if (!is.null(reporting.units.spdf)) {
    names(reporting.units.spdf@data) <- str_to_upper(names(reporting.units.spdf@data))
  }
  fatefieldname <- str_to_upper(fatefieldname)
  pointstratumfieldname <- str_to_upper(pointstratumfieldname)
  designstratumfield <- str_to_upper(designstratumfield)
  reportingunitfield <- str_to_upper(reportingunitfield)
  
  ## Initialize data frame for stratum info. The results from each loop end up bound to this
  master.df <- NULL
  ## Initialize list for point weight info. The results from each loop end up added to this
  ## In the end, these will all be joined to TerrADat and stripped down to the bare essentials to report out
  pointweights.df <- NULL
  
  ## The fate values that we know about are hardcoded here.
  ## Whatever values are provided in the function arguments get concatenated and then we keep only the unique values from that result
  target.values <- c(target.values,
                     "Target Sampled") %>% unique() %>% str_to_upper()
  unknown.values <- c(unknown.values,
                      "Unknown",
                      "UNK") %>% unique() %>% str_to_upper()
  nontarget.values <- c(nontarget.values,
                        "Non-Target",
                        "NT",
                        "Inaccessible",
                        "Not Needed",
                        NA) %>% unique() %>% str_to_upper()
  
  ## for each sample frame...
  for (s in names(sdd.import$sf)) {
    
    ## get the pts file in sdd.src that corresponds to s and call it pts.spdf, then create and init the WGT attribute
    pts.spdf <- sdd.import$pts[[s]]
    pts.spdf@data[, fatefieldname] <- str_to_upper(pts.spdf@data[, fatefieldname])
    pts.spdf@data$WGT <- 0
    
    ## If there's a reporting.units.spdf provided, then we'll assign that identity and restrict the points to the reporting.units.spdf
    if (!is.null(reporting.units.spdf)) {
      pts.spdf <- attribute.shapefile(points = pts.spdf,
                                      shape = reporting.units.spdf,
                                      newfield = reportingunitfield,
                                      attributefield = reportingunitfield)
    }
    
    ## based on FINAL_DESIG, get the no. of pts sampled and the nonresponses.  
    ## For now, we don't use unk and nontarget outside of this sum.  Has been some chatter about gen. WGT for nonresponse categories,
    ## so having nontarget and unk N's may be useful down the road.
    target.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% target.values,])
    unknown.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% unknown.values,])
    nontarget.count <- nrow(pts.spdf@data[pts.spdf@data[, fatefieldname] %in% nontarget.values,])
    
    ## How many points had fate values that were found in fate vectors?
    sum <- target.count + unknown.count + nontarget.count
    
    ## Let the user know what the 'bad fates' are that need to be added
    if (sum != nrow(pts.spdf)) {
      print("The following fate[s] need to be added to the appropriate fate argument[s] in your function call:")
      ## Take the vector of all the unique values in pts.spdf$final_desig (or another fate field) that aren't found in the fate vectors and collapse it into a single string, separated by ", "
      print(paste(unique(pts.spdf@data[, fatefieldname])[!(unique(pts.spdf@data[, fatefieldname]) %in% c(target.values, unknown.values, nontarget.values))], collapse = ", "))
    }
    
    ##TODO - The grep(..strata) evaluation will need to be modified to evaluate for a polygon OR a raster df
    ## If the value for the current SDD in the list strata is not NULL, then we have a strata SPDF
    if (!is.null(sdd.import$strata[[s]])) {
      ## since we have stratification, use Design Stratum attribute to determine the number of stratum, tally the extent of each stratum,
      ## then tally the no. of pts by stratum
      designstrata <- unique(pts.spdf@data[, pointstratumfieldname])
      
      ## Get the stratum SPDF for this SDD (i.e., s), and call it strata.spdf
      strata.spdf <- sdd.import$strata[[s]]
      
      ## Intialize a vector called area to store the area values in hectares, named by the stratum
      area <- NULL
      
      ## Use recorded area of each stratum if present; else derive areas
      if (length(strata.spdf$STRTM_AREA_SQKM) > 0) {
        ## use names to pick up area (sqkm) because designstrata and strtm_area_sqkm accession orders differ!
        for (j in designstrata) {
          area[j] = (strata.spdf$STRTM_AREA_SQKM[strata.spdf@data[, designstratumfield] == j]) * 100 ## *100 to convert from sqkm to ha
        }
      } else {
        ## the following gArea is efficient when polygons are listed separately in the shapefile; otherwise, this can take
        ## an inordinate amount of time (at least on BLM's toy computers).  Also, need to verify the ha conversion (this worked on an example, but
        ## not sure this is a global solution for the SDD files!)
        strata.spdf@data$hectares <- (gArea(strata.spdf, byid = T) * 0.0001)  ## derive ha of each polygon - 0.0001 converts from m2 to ha
        for (j in designstrata) {
          area[j] <- sum(strata.spdf$hectares[strata.spdf@data[, designstratumfield] == j])
        }
      }
      
      ## no. pts by stratum
      ## This creates two named-by-fate-value vectors: Tpts (contains the number of points in each fate value) and Opts (contains the number of points for each TARGET fate value)
      for (j in designstrata) {
        Tpts <- NULL # total pts
        Opts <- NULL ## observed pts - i.e., sampled pts
        working.pts <- pts.spdf@data[pts.spdf@data[, pointstratumfieldname] == j,]
        Tpts <- nrow(working.pts[working.pts[, fatefieldname] %in% c(target.values, nontarget.values, unknown.values),])
        Opts <- nrow(working.pts[working.pts[, fatefieldname] %in% c(target.values),])
        # for (k in c(target.values, nontarget.values, unknown.values)) {
        #   print(k)
        #   ## Get the number of points in the stratum that have the current fate
        #   Tpts[k] <- nrow(working.pts[working.pts[, fatefieldname] == k,])
        #   if (k %in% target.values) {
        #     Opts <- Tpts[k] ## Storing any target point counts in their own vector
        #   }
        # }
        
        
        ## derive adjusted wgt for stratum j
        sigma <- sum(Tpts) # total number of pts within the spatial extent of stratum j
        Pprop <- 1 # initialize - proportion of 1.0 means there were no nonresponses
        wgt <- 0 ## initialize wgt
        Sarea <- 0 ## initialize actual sampled area
        
        if (sigma > 0) {
          Pprop <- Opts/sigma	## realized proportion of the stratum that was sampled (observed/total no. of points) 
        }
        if (Opts > 0) {
          wgt   <- (Pprop*area[j])/Opts  ## (The proportion of the total area that was sampled * total area [ha]) divided by the no. of observed points
          Sarea <-  Pprop*area[j] ## Record the actual area(ha) sampled - (proportional reduction * stratum area)
        }
        
        ##Tabulate key information for this SDD, by stratum (j)  
        temp.df <- NULL
        temp.df <- data.frame(SDD = s,
                              Stratum = j,
                              Total.pts = sigma,
                              Observed.pts = Opts,
                              Area.HA = area[j],
                              Prop.dsgn.pts.obsrvd = Pprop,
                              Sampled.area.HA = Sarea,
                              Weight = wgt,
                              stringsAsFactors = F)
        ## Bind this stratum's information to the master.df initialized outside and before the loop started
        master.df <- rbind(master.df, temp.df)  ## pile it on.....
        
        ## store weights for the stratum j observed pts
        ## At the end of the j loop, pts.spdf is used to output the key attributes listed in Step 7 below
        ## If a point had a target fate, assign the calculates weight
        working.pts$WGT[working.pts[, pointstratumfieldname] == j & working.pts[, fatefieldname] %in% target.values] <- wgt
        ## If a point had a non-target or unknown designation, assign 0 as the weight
        ##WGT init to zero, but these 2 lines are to make sure we record 0
        working.pts$WGT[working.pts[, pointstratumfieldname] == j & working.pts[, fatefieldname] %in% c(nontarget.values, unknown.values)] <- 0
        ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
        pointweights.df <- rbind(pointweights.df, working.pts)
      }## endof for (j in designstrata)
      
      ## init re-used SPDFs
      pts.spdf <- NULL
      strata.spdf <- NULL
    } else { ## If there aren't strata available to us in a useful format in the SDD, we'll just weight by the sample frame
      ## since we lack stratification, use the sample frame to derive spatial extent (ha)
      sf.spdf <- sdd.import$sf[[s]]
      area <- (sf.spdf$SAMPLE_FRAME_AREA_SQKM) * 100		## *100 to convert from km to ha. The "SAMPLE_FRAME_AREA.." is specific to sample frames, stratification
      ##  files have a different name for pre-calculated area (see above)
      
      
      ## derive weights
      Pprop <- 1			## initialize - proportion of 1.0 means there were no nonresponses
      wgt <- 0			## initialize wgt
      Sarea <- 0			## initialize actual sampled area
      if (sum > 0) {
        Pprop <- target.count/sum ## realized proportion of the stratum that was sampled (observed/total no. of points)
      }
      if (target.count > 0) {
        wgt   <- (Pprop*area)/target.count  ## (The proportion of the total area that was sampled * total area [ha]) divided by the no. of observed points
        Sarea <-  Pprop*area	      ## Record the actual area(ha) sampled - (proportional reduction * stratum area)
      }
      
      ##Tabulate key information for this SDD, by stratum (j)  
      temp.df <- NULL
      temp.df <- data.frame(SDD = s,
                            Stratum = "Sample Frame",
                            Total.pts = sum,
                            Observed.pts = target.count,
                            Area.HA = area,
                            Prop.dsgn.pts.obsrvd = Pprop,
                            Sampled.area.HA = Sarea,
                            Weight = wgt,
                            stringsAsFactors = F)
      ## Bind this stratum's information to the master.df initialized outside and before the loop started
      master.df <- rbind(master.df, temp.df)  ## pile it on.....
      
      
      ## store weights for the stratum j observed pts
      ## At the end of the j loop, pts.spdf is used to output the key attributes listed in Step 7 below
      ## If a point had a target fate, assign the calculates weight
      pts.spdf$WGT[pts.spdf@data[, fatefieldname] %in% target.values] <- wgt
      ## If a point had a non-target or unknown designation, assign 0 as the weight
      ##WGT init to zero, but these 2 lines are to make sure we record 0
      pts.spdf$WGT[pts.spdf@data[, fatefieldname] %in% c(nontarget.values, unknown.values)] <- 0
      
      ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
      pointweights.df <- rbind(pointweights.df, pts.spdf@data)
      
      ## init re-used SPDFs
      pts.spdf <- NULL		
      sf.spdf <- NULL
      
    }## endof if no stratification
  }  ## endof for(s in sdd.src )
  
  ## Adding in the TerrADat attributes because we need those primary keys
  ## TODO: Figure out where PrimaryKey actually lives, because it's not my copy of TerrADat
  # pointweights.df.merged <- merge(y = pointweights.df[, c("TERRA_TERRADAT_ID", "FINAL_DESIG", "WGT")],
  #                                 x = tdat[, c("PLOTID", "PRIMARYKEY")],
  #                                 by.y = c("TERRA_TERRADAT_ID"),
  #                                 by.x = "PLOTID", all = F)
  
  ## Diagnostics in case something goes pear-shaped
  # if (length(pointweights.df.merged$PLOTID[!(unique(pointweights.df.merged$PLOTID) %in% unique(pointweights.df.merged$PLOTID))]) > 0) {
  #   print("Somehow the following points were in the SDD and weighted, but had no counterpart in the provided TerrADAT")
  #   print(paste(pointweights.df.merged$PLOTID[!(unique(pointweights.df.merged$PLOTID) %in% unique(pointweights.df.merged$PLOTID))], collapse = ", "))
  # }
  names(pointweights.df)[names(pointweights.df) == "TERRA_TERRADAT_ID"] <- "PRIMARYKEY"
  ## Output is a named list with two data frames: information about the strata and information about the points
  # return(list(strata.weights = master.df, point.weights = pointweights.df.merged[, c("PRIMARYKEY", "PLOTID", "FINAL_DESIG", "WGT")]))
  return(list(strata.weights = master.df, point.weights = pointweights.df[, c("PRIMARYKEY", "PLOTID", "FINAL_DESIG", "WGT")]))
}

analyzer <- function(evaluated.points, ## Data frame output from benchmarker()
                     weights, ## The list output from weighter()
                     tdat ## The attributed TerrADat that has the reporting units
                     ) {
  ## Splitting out the weighter() output
  stratum.weights <- weights[[1]]
  point.weights <- weights[[2]]
  ## Sanitization
  names(evaluated.points) <- str_to_upper(names(evaluated.points))
  names(stratum.weights) <- str_to_upper(names(stratum.weights))
  names(point.weights) <- str_to_upper(names(point.weights))
  if (class(tdat)[1] == "SpatialPointsDataFrame") {
    tdat <- tdat@data
  }
  names(tdat) <- str_to_upper(names(tdat))
  
  ## Then we combine them!
  data <- merge(x = evaluated.points, y = point.weights)
  
  ## We're going to add ".ind" to the end of each indicator name so we can find them easily later with a select() after we've spread() this data frame
  data$INDICATOR <- paste0(data$INDICATOR, ".ind") %>% as.factor()
  
  ## Initialize the output data frame
  output <- data.frame()
  
  ## The actual analysis will be done on a per-objective level, so we're just going to loop through those because apply() is kind of a pain and this is computationally cheap enough (I think)
  for (o in unique(data$MANAGEMENT.QUESTION)) {
    data.current <- data[data$MANAGEMENT.QUESTION == o,]
    
    ## Make the data set wide because that's the format that makes our lives easier for cat.analysis()
    ## Need to remove the columns Value and so that each plot ends up existing on just one row per evaluation stratum it has membership in
    data.wide.current <- spread(data = data.current %>% select(-VALUE, -EVALUATION.STRATUM), ## Data frame to make wide
                                key = INDICATOR, ## Column that contains the column names
                                value = EVALUATION.CATEGORY, ## Column that contains the values
                                fill = NA ## Where there's an NA, fill it with 0
    )
    
    ## Add in the reporting unit information from the supplied TerrADat
    data.wide.current <- merge(x = data.wide.current,
                               y = tdat[, c("PRIMARYKEY", "REPORTING.UNIT", "LONGITUDE", "LATITUDE")],
                               by.x = c("PRIMARYKEY"),
                               by.y = c("PRIMARYKEY")) %>% distinct()
    
    ## Because it's easier to do this now while the data frame is still just one object and not four or five
    names(data.wide.current)[names(data.wide.current) %in% c("PLOTID", "WGT", "REPORTING.UNIT","LONGITUDE", "LATITUDE")] <- c("siteID", "wgt", "Reporting.Unit", "xcoord", "ycoord")
    ## All the sites are active? Sure! Why not?
    data.wide.current$Active <- T
    
    ## First, the sites. This is a data frame with the siteIDs and whether they're active or not
    aim.sites <- data.wide.current[, c("siteID", "Active")] %>% distinct()
    
    ## The subpopulations. This is a data frame of the siteIDs and reporting units. I think each siteID can only appear once, so we need to programmatically create this from the tall data frame
    aim.subpop <- data.wide.current[, c("siteID", "Reporting.Unit")] %>% distinct()
    
    ## The design information
    aim.design <- data.wide.current[, c("siteID", "wgt", "xcoord", "ycoord")] %>% distinct()
    
    ## The data. A data frame with siteID and columns for each indicator (with the evaluation category strings as factors)
    aim.datacat <- data.wide.current %>% dplyr::select(siteID, matches("\\.ind$")) %>% distinct()
    ## Fix the names of the indicators so that the output doesn't include the ".ind" suffix which interferes with the automated report knitting
    names(aim.datacat) <- names(aim.datacat) %>% str_replace_all("\\.ind$", "")
    
    ## TODO: Think abot how to get sum of wgt by stratum and set up a stratified aim.popsize list
    ## The areas should be the sum of the weights, right?
    areas.df <- data.wide.current %>% group_by(Reporting.Unit) %>% summarize(area = sum(wgt))
    ## So we're converting them to a list
    area.list <- areas.df$area %>% as.list()
    ## And naming them with the reporting unit they belong to
    names(area.list) <- areas.df$Reporting.Unit
    
    ## This example is for unstratified sampling (also for simplicity) for stratified, need to add the stratum field to the design data frame
    ## and add the stratum areas to the popsize list
    aim.popsize = list("Reporting.Unit" = area.list)
    
    ### Now run cat.analysis
    aim.analysis <- cat.analysis(sites = aim.sites, subpop = aim.subpop, design = aim.design, data.cat = aim.datacat, popsize = aim.popsize)
    
    ## Add the objective/management question info to those results
    aim.analysis$MANAGEMENT.QUESTION <- o
    
    ## rbind() these results to the output data frame
    # output < rbind(output, aim.analysis)
    
    ### Assign that to an object we can call later to combine all the loop results
    assign(paste0(str_replace_all(o, " ", "."), ".analysis.output"), aim.analysis)
  }
  
  ## Make a list of all the names of data frames that came out of that loop
  extant.results <- ls()[grepl(x = ls(), pattern = "\\.analysis.output$")]
  
  ## Bind together all the data frames listed in extant.results
  output <- parser(string = paste0("rbind(`", paste(extant.results, collapse = "`, `"), "`)"))
  
  return(output)
}