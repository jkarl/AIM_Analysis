#####################################################################
#### ATTRIBUTING POINTS WITH EVALUATION/BENCHMARK STRATA ####
## Functions for adding a column/field called Evaluation.Stratum to points
## for use in benchmark evaluations

## attribute.shapefile() adds from a specified field in a shapefile, either points or polygons
## attribute.list() adds based on a simple lookup table of PlotID and Evaluation.Stratum
## attribute.field() adds from a lookup table based on values found in the fields of the points fed to it
#####################################################################

library(stringr)
library(sp)


## Shapefile attribute extraction function where the shapefile attribute table contains the evaluation stratum (possibly strata)
attribute.shapefile <- function(points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                                data.path = "", ## If the shape is in a .gdb feature class then this should be the full path, including the file extension .gdb. If the SPDF is already made, do not specify this argument
                                shape = "", ## The name of the shapefile or feature class !!!OR!!! an SPDF
                                attributefield = "", ## Name of the field in the shape that specifies the evaluation stratum
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")){
  ## Strip the file extension from shape, just in case it was there
  shape <- str_replace(shape, pattern = "\\.[Ss][Hh][Pp]$", replacement = "")
  ## If this is coming from a geodatabase, extract the shapefile appropriately. Otherwise read in the .shp
  if (grepl(x = data.path, pattern = "\\.[Gg][Dd][Bb]$")) {
    shape.spdf <- readOGR(dsn = data.path, layer = shape, stringsAsFactors = F) %>% spTransform(projection)
  } else if (data.path != "") {
    shape.spdf <- readOGR(dsn = paste0(data.path, "/", shape, ".shp"), layer = shape, stringsAsFactors = F) %>% spTransform(projection)
  } else if (class(shape)[1] == "SpatialPointsDataFrame" | class(shape)[1] == "SpatialPolygonsDataFrame") {
    shape.spdf <- shape
  }
  if (dropNA) {
  ## Make sure that the points also adhere to the same projection
  points <- points %>% spTransform(projection)
  
  ## Because there might be overlap between polygons with different evaluation stratum identities, we'll check each eval stratum independently
  for (n in unique(shape.spdf@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.points <- points
    ## Get the data frame from checking the points against the current subset of the polygons
    over.result <- over(current.points, shape.spdf[shape.spdf@data[, attributefield] == n,])
    ## Add the values to the Evaluation.Stratum column
    current.points$Evaluation.Stratum <- over.result[, attributefield]
    ## Store the results from this loop using the naming scheme "over__[current value of n]" with spaces replaced with underscores to prevent parsing errors later
    assign(x = str_replace(paste0("over__", n), " ", "_"),
           ## Only keep the ones that actually took on an attribute
           value = points[!is.na(points$Evaluation.Stratum),])
  }
  ## List all the objects in the working environment that start with "over__" and rbind them
  attributed.spdfs <- ls()[grepl(x = ls(), pattern = "^over__")]
  output <- eval(parse(text = paste0("rbind(", paste(attributed.spdfs, collapse = ",") ,")")))
  
  return(output)
}

## Function to import .csv that attributes the plots matching the PLOT column with evaluation strata from the EVAL.STRATUM column
attribute.list <- function(points = points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
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
attribute.field <- function(points = points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                            datapath = "", ## Only specify if you need to read in the lookup table from a file
                            lut = "", ## Either the filename !!!OR!!! a data frame. Either way it needs the columns Attribute.Field, Field.Value, Evaluation.Stratum
                            dropNA = T, ## Strip out points that did not qualify for an attribution stratum
                            ){
  ## Sanitize the input
  datapath <- str_replace(datapath, pattern =  "/$", replacement = "")
  if ((datapath == "" | is.null) & is.data.frame(lut)) {
    ## The format of the lookup table needs to include these three fields
    lut <- lut[, c("Attribute.Field", "Field.Value", "Evaluation.Stratum")]
  } else if (!is.data.frame(lut) & grepl(x = lut, pattern = "\\.[Cc][Ss][Vv]$")) {
    lut <- read.csv(paste0(datapath, "/", lut), stringsAsFactors = F)[, c("Attribute.Field", "Field.Value", "Evaluation.Stratum")]
  }
  for (n in 1:nrow(lut)) {
    points$Evaluation.Stratum[points[, lut$Attribute.Field[n]] == lut$Attribute.Value[n]] <- lut$Evaluation.Stratum[n]
  }
  output <- points
  if (dropNA) {
    output <- output[!is.na(output$Evaluation.Stratum),]
  }
  return(output)
}