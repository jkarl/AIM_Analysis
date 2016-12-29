####load packages####
library(xlsx)
library(tidyverse)
library(stringr)
library(rgdal)
library(arcgisbinding)
arc.check_product()
library(sp)
library(dplyr)
library(maptools)
library(rgeos)
library(latticeExtra)
library(spsurvey)
library(ggplot2)
library(spatialEco)


##### aimwgt.r ->  Derivation of weights for a basic AIM data set for an individual project area.  No STRATA in addition to ESDs/BpSs.  All shapefiles are assumed to be in the same projection.  
##1) Requires a polygonal map of ESDs or BpSs used to stratify samples (= frame)
##2) Requires TerrADAT file clipped to the project area (observed data)
##3) If available, uses a point layer recording the fate of the original sample points.  This information is used
##   to adjust the weights of points by strata



##### trying to make this section of code as flexible as possible.  
## 1) use setwd(directory) in the console to set the working directory (wd)
## 2) wd is specified in filenames.txt; setwd() is executed here in case this script is initiated as R CMD BATCH aimwgt.r output within the actual wd
## 3) filenames.txt contains: workingdirectory (wd), source (src), output shapefile (output), point_fate (fate), terra_data (terra),frame (frame),
##    summary output (table)
##    If there is no point_fate file, use "none" - case sensitive!



##### read working directory, paths, and filenames
setwd("u:/aim/geo/lander/")
src <- "u:/aim/geo/lander/"
output <- "u:/aim/geo/lander/crap"
fate <- NULL
terra <- "aimpts_2015_proj"
frame <- "landerpolystrata"
table <- "summary.out"


##### store the frame.  The frame should contain STRATA (e.g., ESDs, BpSs)
frame.spdf <- readOGR(dsn = ".",
                      layer = fpe$frame,
                      stringsAsFactors = F)


##### determine total no. of points by stratum
if (!is.null(fate)) {

   #####Ingest fate of points
   all.spdf <- readOGR(dsn = ".",
                       layer = fpe$fate,
                       stringsAsFactors = F)



  #### determine intersection of pts and ESD or BpS types
   ### need to determine how to score (which attribute?) the fate; in particular, what constitues observed, a nonresponse, and oversample that was never actually used. 

  pts.poly <- point.in.poly(all.spdf,frame.spdf)  ##(points,polygon)
  n.types <- length(unique(pts.poly$STRATA))
  a <- c(unique(pts.poly$STRATA))  ##now each pt has a STRATA (ESD OR BpS) type - store the unique types in a[]. need to check/eliminate NA

  ### derive no. of pts in each strata
  Tpts = NULL
  for (i in 1:length(unique(pts.poly$STRATA))) {
    Tpts[i] <- nrow(pts.poly[pts.poly$STRATA == a[i], ]) ##Tpts[i] stores the total no. of pts in stratum=a[i] that were used in this sample event
  }

}




#####Ingest data from TerrADAT, and repeat the above count of points by stratum. NOTE: this only works if the number of strata in above analysis is the same as in analysis below?  
terra.spdf <- readOGR(dsn = ".",
                      layer = fpe$terra,
                      stringsAsFactors = F)


  #### determine intersection of observed pts, and ESD or BpS types
  pts.poly <- point.in.poly(terra.spdf,frame.spdf)  ##(points,polygon)
  n.types <- length(unique(pts.poly$STRATA))
  a <- c(unique(pts.poly$STRATA))  ##now each pt has a STRATA (ESD.BpS) type - store the unique types in a[]. need to check/eliminate NA

  ### derive no. of pts by strata
  Opts = NULL
  for (i in 1:length(unique(pts.poly$STRATA))) {
    Opts[i] <- nrow(pts.poly[pts.poly$STRATA == a[i],])  ##Opts[i] stores the observed no. of pts in stratum=a[i] 
  }


##### We may not have the fate of points in all cases.  if fate==none, then set b[] to c[] for correct calulcation of adjusted weights in the next step
  if (is.null(fate)) {
	Tpts = NULL
        for (i in 1:length(unique(pts.poly$STRATA))) {
    		Tpts[i] <- Opts[i] 
  	}
  }




#####Derive adjustment of stratum area -i.e., the proportion of stratum area that was actually sampled.   
  Pprop <- NULL
  for (i in 1:length(unique(pts.poly$STRATA))) {
    Pprop[i] <- Opts[i]/ Tpts[i]  ##Tpts[i] is the total no. of points by stratum; Opts[i] stores the observed no. of pts in stratum=a[i]. 
  } 
## Pprop[i] stores the proportion of stratum area that was actually sampled (1-nonresponses effect)
## Pprop[i] = 1 where there were no nonresponses.  Prop[i] < 1 where there were nonresponses.  
## (the no. of pts/ (Prop[i]* spatial extent of stratum i) ) is the actual inclusion probability.  The inverse of this is the actual weight. 



##### Derive area (ha) of each polygon, then total area by stratum
  frame.spdf@data$hectares <- (gArea(frame.spdf,byid=TRUE)*0.0001)  ## derive ha of each polygon - 0.0001 converts from m2 to ha

  area <-NULL
  for(i in 1:length(unique(pts.poly$STRATA))) {
     temp <- frame.spdf$hectares[frame.spdf$STRATA==a[i]] 
     area[i] <- sum(temp)
  }


##### Derive the final weights by stratum = wgt[i].  The weights are in hectares; may want to change to acres?  Also, record actual sampled area by stratum
  wgt <- NULL
  Sarea <- NULL
  for(i in 1:length(unique(pts.poly$STRATA))) {
     wgt[i] <- (Pprop[i]*area[i])/Opts[i]           ##By STRATA, (The proportion of the total area that was sampled * total area) divided by the no. of observed points
     Sarea[i]=  Pprop[i]*area[i]
  }

##### At this point, pts.poly is terra + strata.  Now add wgts to pts.poly and output the shapefile.
    pts.poly@data$weight<-NULL


    for (i in 1:length(unique(pts.poly$STRATA))) {
      	 pts.poly$weight[pts.poly$STRATA == a[i]] = wgt[i]
    }


    ### save original terra shapefile with strata and weights
    pts.poly %>% arc.write (output, data = .)



##### summarize and output results
      summary <- NULL
      ## STRATA label, total pts, observed pts, sampled area as a proportion, total area, actual sampled area, weight
      summary <- data.frame(c(a[1:n.types]),Tpts[1:n.types],Opts[1:n.types],Pprop[1:n.types],area[1:n.types],Sarea[1:n.types],wgt[1:n.types])
      write.table(summary,fpe$table)



### tdt.spdf@data$INT <- gIntersects(lay.spdf,tdt.spdf,byid=F) - for future reference
