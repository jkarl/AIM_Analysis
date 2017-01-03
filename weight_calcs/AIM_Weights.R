#####################################################################
#### CALCULATING WEIGHTS USING SAMPLE DESIGN DATABASES AND TERRADAT ####
#####################################################################
#####load packages####
library(tidyverse)
library(arcgisbinding) #we use this package to read from file geodatabases--it is significantly faster than rOGDB
arc.check_product()
library(maptools) #this is for reading in shapefiles
library(rgdal) # for read in rasters
library(rgeos)
library(stringr)

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

## A function that evaluates a parsed text string like the ones in $eval.string.upper and lower. Used in a lapply() later. Probably replaceable with parse() %>% eval() there though
parser <- function(string){
  return(safely(eval(parse(text = string)))[[1]])
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
  sf.list <- eval(parse(text = paste0("list(", paste(ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")], collapse = ", "), ")")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")] %>% str_replace(pattern = "^sf\\.", replacement = "")
  
  ## Creating the named list of all the pts SPDFs created by the loop
  pts.list <- eval(parse(text = paste0("list(", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = ", "), ")")))
  names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")] %>% str_replace(pattern = "^pts\\.", replacement = "")
  
  ## Creating the named list of all the strata SPDFs created by the loop
  strata.list <- eval(parse(text = paste0("list(", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = ", "), ")")))
  names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")] %>% str_replace(pattern = "^strata\\.", replacement = "")
  
  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)
  
  return(output)
}

## Currently uncalled
# ## A function for clipping polygons
# gClip <- function(frame, ## SpatialPolygonsDataFrame to be clipped
#                   clip ## SpatialPolygonsDataFrame defining the extent to clip to
#                   ) {
#   clipped <- gIntersection(frame, clip, byid = T)
#   row.names(clipped) <- as.character(gsub(" 0", "", row.names(clipped)))
#   return(SpatialPolygonsDataFrame(clipped, frame@data[row.names(clipped), ]))
# }
# 
# ## A function for removing polygons. NB: This is quite slow right now
# gErase <- function(frame,erase) {
#   gDifference(frame, erase)
# }

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
out.src <- "data" %>% sanitizer(type = "filepath")
## If out.src is a subdirectory path within the working directory, follow the previous line with this.
## If out.src is a full filepath in its own right, comment it out
out.src <- paste0(getwd(), "/", out.src)

out.filename <- "ELFO_TwinPeaks" #set the file name, of the structure: FO_Project



##########################################################
#### IMPORTING DATA ####
##########################################################
#### Step 1: Read in Files#####

###First, the SDDs#### 
sdd.raw <- sdd.reader(src = src, sdd.src = c("SDD_NorCal_2014intensive_SDD.gdb"))

#Step 1a: If no.strata has values in it, examine those values to determine if the strata are a) rasters or b) do not exist. 
#If the strata are rasters, then use the following code to read in the raster strata, with the prefix "strata."
###-->Jason

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
## and so begins Garman's folly
## Step 3.  Cycle thru all the SDDs and generate pt weight estimates for each separately.  We'll deal with other weightings 
##          (e.g., a FO, overlapping designs) in subsequent steps.


## For each SDD sample frame (specified in sdd.src), adjust the sampled area (account for non-responses), generate weights,
## update the pts file with weights, tabulate area and pt information (this table is a quick summary for internal purposes),
## & generate the .csv file with attributes listed in Step 7 below. 


## This function produces point weights by design stratum (when the SDD contains them) or by sample frame (when it doesn't)
weighter <- function(sdd.import, ## The output from sdd.reader()
                     tdat, ## The TerrADat data frame to use. This lets you throw the whole thing in or slice it down first, if you like
                     ## Keywords for point fate—the values in the vectors unknown and nontarget are considered nonresponses.
                     ## Assumes the following keywords are sufficient and consistent.
                     ## "UNK" and "NT" show up in certain SDDs even though the shapefle attributes spell out the keywords and they're invalid??? 
                     target.values = c("Target Sampled"),
                     unknown.values = c("Unknown",
                                        "UNK",
                                        NA),
                     nontarget.values = c("Non-Target",
                                          "NT"),
                     ## These shouldn't need to be changed from these defaults, but better to add that functionality now than regret not having it later
                     fatefieldname = "final_desig", ## The field name in the points SPDF to pull the point fate from
                     pointstratumfieldname = "dsgn_strtm_nm", ## The field name in the points SPDF to pull the design stratum
                     designstratumfield = "dmnnt_strtm" ## The field name in the strata SPDF to pull the stratum identity from 
){
  
  ## Initialize data frame for stratum info. The results from each loop end up bound to this
  master.df <- NULL
  ## Initialize list for point weight info. The results from each loop end up added to this
  ## In the end, these will all be joined to TerrADat and stripped down to the bare essentials to report out
  pointweights.df <- NULL
  
  ## The fate values that we know about are hardcoded here.
  ## Whatever values are provided in the function arguments get concatenated and then we keep only the unique values from that result
  target.values <- c(target.values,
                     "Target Sampled") %>% unique()
  unknown.values <- c(unknown.values,
                      "Unknown",
                      "UNK",
                      NA) %>% unique()
  nontarget.values <- c(nontarget.values,
                        "Non-Target",
                        "NT") %>% unique()
  
  ## for each sample frame...
  for (s in names(sdd.import$sf)) {
    
    ## get the pts file in sdd.src that corresponds to s and call it pts.spdf, then create and init the wgts attribute
    pts.spdf <- sdd.import$pts[[s]]
    pts.spdf@data$wgts <- 0
    
    ## based on FINAL_DESIG, get the no. of pts sampled and the nonresponses.  
    ## For now, we don't use unk and nontarget outside of this sum.  Has been some chatter about gen. wgts for nonresponse categories,
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
      if (length(strata.spdf$strtm_area_sqkm) > 0) {
        ## use names to pick up area (sqkm) because designstrata and strtm_area_sqkm accession orders differ!
        for (j in designstrata) {
          area[j] = (strata.spdf$strtm_area_sqkm[strata.spdf@data[, designstratumfield] == j]) * 100 ## *100 to convert from sqkm to ha
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
        for (k in c(target.values, nontarget.values, unknown.values)) {
          ## Get the number of points in the stratum that have the current fate
          Tpts[k] <- nrow(pts.spdf@data[pts.spdf@data[, pointstratumfieldname] == j & pts.spdf@data[, fatefieldname] == k ,])
          if (k %in% target.values) {
            Opts <- Tpts[k] ## Storing any target point counts in their own vector
          }
        }
        
        
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
        pts.spdf$wgts[pts.spdf@data[, pointstratumfieldname] == j & pts.spdf@data[, fatefieldname] %in% target.values] <- wgt
        ## If a point had a non-target or unknown designation, assign 0 as the weight
        ##wgts init to zero, but these 2 lines are to make sure we record 0
        pts.spdf$wgts[pts.spdf@data[, pointstratumfieldname] == j & pts.spdf@data[, fatefieldname] %in% c(nontarget.values, unknown.values)] <- 0
        ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
        pointweights.df <- rbind(pointweights.df, pts.spdf@data)
      }## endof for (j in designstrata)
      
      ## init re-used SPDFs
      pts.spdf <- NULL
      strata.spdf < -NULL
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
      pts.spdf$wgts[pts.spdf@data[, fatefieldname] %in% target.values] <- wgt
      ## If a point had a non-target or unknown designation, assign 0 as the weight
      ##wgts init to zero, but these 2 lines are to make sure we record 0
      pts.spdf$wgts[pts.spdf@data[, fatefieldname] %in% c(nontarget.values, unknown.values)] <- 0
      
      ## Add the point SPDF now that it's gotten the extra fields to the list of point SPDFs so we can use it after the loop
      pointweights.df <- rbind(pointweights.df, pts.spdf@data)
      
      ## init re-used SPDFs
      pts.spdf <- NULL		
      sf.spdf <- NULL
      
    }## endof if no stratification
  }  ## endof for(s in sdd.src )
  
  ## Adding in the TerrADat attributes because we need those primary keys
  pointweights.df.merged <- merge(x = pts.combined, y = tdat, by.x = c("plot_key"), by.y = "PlotKey", all = F)
  
  ## Diagnostics in case something goes pear-shaped
  print("Somehow the following points were in the SDD and weighted, but had no counterpart in the provided TerrADAT")
  print(paste(pts.combined$PlotID[!(unique(pts.combined$PlotID) %in% unique(pts.combined.merged$PlotID))], collapse = ", "))
  
  
  ## Output is a named list with two data frames: information about the strata and information about the points
  return(list(strata.weights = master.df, point.weights[, c("PrimaryKey", "PlotID", "final_desig", "wgts")]))
}




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




