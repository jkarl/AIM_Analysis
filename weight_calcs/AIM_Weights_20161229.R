#####################################################################
#### CALCULATING WEIGHTS USING SAMPLE DESIGN DATABASES AND TERRADAT ####
## Reads in SDDs and reporting unit shapefiles

## attribute.shapefile() adds from a specified field in a shapefile, either points or polygons
## attribute.list() adds based on a simple lookup table of PlotID and Evaluation.Stratum
## attribute.field() adds from a lookup table based on values found in the fields of the points fed to it
#####################################################################
#12/21/2016
#


#####load packages####
library(tidyverse)
library(arcgisbinding) #we use this package to read from file geodatabases--it is significantly faster than rOGDB
arc.check_product()
library(maptools) #this is for reading in shapefiles
library(rgdal) # for read in rasters
library(rgeos)
library(purrr) ## To wrap functions in to create default values to return when there are errors instead of just failing

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

##########################################################
#### GLOBAL VARIABLES ####
##########################################################
###Set Data Sources####
## Filepath containing the Sample Design Database[s]. Use the first line if it's not a subdirectory of the working directory
# src <- "C:\\Users\\samccord\\Documents\\AIM\\Projects\\NorCal\\" %>% sanitizer(type = "filepath")
src <- paste(getwd(),
             "data", ## This is the subdirectory path. It can be multiple layers deep, e.g. "data/norcal/test_set"
             sep = "/") %>% sanitizer(type = "filepath")

## Vector of SDD filenames. There can be more than one SDD in the vector.
sdd.src <- c("SDD_NorCal_2013extensive_SDD.gdb",
             "SDD_NorCal_2013intensive_SDD.gdb",
             "SDD_NorCal_2013ESR_SDD.gdb", 
             "SDD_NorCal_2014-2018_SDD_122116.gdb",
             "SDD_NorCal_2014intensive_SDD.gdb") %>% sapply(sanitizer, type = "gdb", USE.NAMES = F)

##Set the source of the reporting unit.
## The vector can contain any number of filenames, but these are expected/need to be .shp files in the directory src
reporting.unit.src <- c("Eagle_Lake_FO.shp",
                        "Twin_Peaks.shp")  %>% sapply(sanitizer, type = "shp", USE.NAMES = F)


###Set Output Files####
## Set the filepath for the output file. Use the first line if it's not a subdirectory of the working directory
# out.src <- "C:\\Users\\samccord\\Documents\\AIM\\Projects\\NorCal\\" 
out.src <- paste0(getwd(), "/",
                  "data" ## This is the subdirectory path. It can be multiple layers deep, e.g. "data/norcal/test_set"
                  ) %>% sanitizer(type = "filepath")

out.filename <- "ELFO_TwinPeaks" #set the file name, of the structure: FO_Project



##########################################################
#### IMPORTING DATA ####
##########################################################
#### Step 1: Read in Files#####
###First, the SDDs#### 

## Reads in SDDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## sf is a list of sample frame SPDFs, pts is a list of point SPDFs, strata is a list of stratfication SPDFs
## The SPDFs are all named using the SDD filename provided in sdd.src so that output$sf$generic_design.gdb has the sample frame that corresponds to output$pts$generic_design.gdb
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to each other.
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that SDD in the list
sdd.reader <- function(src = "", ## A filepath as a string
                       sdd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                       projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83 projection
                       ){
  
  ## readOGR() wrapped in safely() so that it will return NULL instead of an error. I need this for the function
  safe.readOGR <- safely(readOGR, otherwise = NULL)
  
  
  ## Looped so that it can execute across all the SDDs in the vector (if there are more than one)
  for (s in sdd.src) {
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
  ## Create a list of the sample frame SPDFs.
  ## This programmatically create a string of the existing object names that start with "sf." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it
  sf.list <- eval(parse(text = paste0("list(", paste(ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")], collapse = ", "), ")")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")]
  
  ## Creating the named list of all the pts SPDFs created by the loop
  pts.list <- eval(parse(text = paste0("list(", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = ", "), ")")))
  names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")]
  
  ## Creating the named list of all the strata SPDFs created by the loop
  strata.list <- eval(parse(text = paste0("list(", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = ", "), ")")))
  names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")]
  
  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)
  
  return(output)
}

###Then the Reporting Units###
for (r in reporting.unit.src) {
  assign(x = paste0("rep.", r),
         value = readShapePoly(fn = paste(src, r, sep = "/"),
                               ## The standard NAD83 projection. Used in the SDD
                               proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")))
  #remove intermediate objects
  rm(r)
}

##########################################################
#### CREATE WEIGHT CATEGORIES ####
##########################################################
###Step 2: Create Weight Categories

#Step 2a: If no.strata has values in it, examine those values to determine if the strata are a) rasters or b) do not exist. 
#If the strata are rasters, then use the following code to read in the raster strata, with the prefix "strat."
    ###-->Jason

#Step 2b: Intersect each sample frame polygon with each reporting unit to determine a) if they overlap and b) if one is contained within the other
#I haven't figured out how to determine the overlapping order automatically, yet. So at this point it is a manual step to figure out overlapping designs




#Once you know the overlap order, you can determine weight categories
#These categories should be of the form Reporting.Unit_SampleFrame

#With the general sample frame weight categories in place, you can then add strata as appropriate
#In the case of NorCal, strata are relevate for the 2013ESR design and the 2014-2018 design category
#Weight category should be


##########################################################
#### CALCULATE WEIGHTS ####
##########################################################
###Step 3: Calculate the area of each weight category

###Step 4 Calculate # points in each weight category, use the SDD pts but verify that all TerrADat points are accounted for

### Step 5: Produce weights (area/#weights in each weigth category)

##########################################################
#### CREATE AND WRITE OUTPUT ####
##########################################################
### Step 6: Join weights to the points df, called wgt.df


#Step 6a: Check that the points in TerrADat are all accounted for

### Step 7: Write output
###write out results, field names should be: 
  #PrimaryKey(from SDD), 
  #PlotID.SDD (from SDD), 
  #FINAL_DESIG (from SDD, no not a typo), this is the point fate
  #Reporting Unit (from intersection)
  #Wgt.Category (from intersection), should be of the form ReportingUnit_Stratum/MDCaty
  #Wgt
  #PlotID.TDAT (PlotID from TerrADat)


write.csv(wgt.df, file = paste0(out.src, out.filename, "_wgt_", Sys.Date()))



###Build Weights Table###




