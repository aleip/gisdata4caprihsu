#Used Packages
library(data.table)
library(plyr)
library(dplyr)    #installed the dev version from https://github.com/hadley/dplyr
#library(reshape)
library(reshape2)
library(stats)
library(gdxrrw)
#library(gdxtools)
?gdxrrw
?rgdx
#library(foreach)
#library(doParallel)
#library(foreign)


#Set working directory and load of general (used to update all) datasets.
# These databases have been updated by Renate Koeble and delivered in the folder capri/hsu2_database_update_2016_02
if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
  path<-"s:/Actions/AGRIENV/base_data/ELISA/Datasets/uscie/"
  gamspath<-"C:/GAMS/win64/24.4"
}else if(Sys.info()[4]=="D01RI1600881"){ #checks machine name
  path<-"s:/Actions/AGRIENV/base_data/ELISA/Datasets/uscie/"
  gamspath<-"C:/GAMS/win64/24.4"
}else{
  path<-"X:/MARS_disaggregation/"
  gamspath<-"X:/GAMS/win64/24.7"
}

setwd(paste0(path,"hsu2_database_togdx_201605_nocita"))


#al20160627: path not needed any more?? (sufficient load("hsu2_xavi.RData))?
#al20160802 - should not start with ... xavi-data to test out from scratch?
#load("hsu2_xavi.RData")

#link with gams directory
igdx(gamspath)



## Function 1: Computing statistics per HSU, NUTS 3-2-0
# NOTES FOR THIS FUNCTION: 
# The first argument is the data table
# Pass the functions (statistics) to be computed as arguments (e.g. functs = c("weighted.mean", "max"))
# Pass the variables to compute (e.g. vbles = c("f_2000", "f_2006"))
# If desired, provide na.rm = TRUE
# If weighted mean wants to be computed, in the data table must be one column called "area". 
#    If it is not provided, an error message is issued and the function stops

func1 <- function(x, functs, vbles, ...){
  
  start <- Sys.time()
  
  print("Preparing to compute statistics...")
  
  # A) Check if a weighted mean is calculated - set the HSMU area as weighting parameter
  for (i in 1:length(functs)){   # this loop is to add the argument "area", nedded to compute weighted mean
    if (functs[i] == "weighted.mean"){      # this is to tell the function how to compute weighted mean
      functs[i] <- "weighted.mean(., area)"   
      if(!"area" %in% colnames(x)) stop("please, provide a column called 'area' with areas", call. = FALSE)   # If a column called "area" with areas is not provided, an error message is issued and the function stops
    } else {
      functs[i] <- functs[i]
    }
  }
  
  ## computing statistics per HSU ##
  print("Computing statistics per HSU...")
  
  # B) 
  by_hsu2 <- x %>% group_by(hsu2)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
  x.hsu <- by_hsu2 %>% summarise_at(vbles, functs, ...)    # summarise_at() is a new function included in the new Dev version of dplyr. It will replace the old summarise_each()
  
  colnames(x.hsu)[1] <- "spatunit"  #changing 1st column name
  x.hsu$spatunit <- paste0("U", x.hsu$spatunit) #adding "U" in front of spatial units
  
  
  ## Computing statistics per NUTS 3 ##
  print("Computing statistics per NUTS 3...")
  
  by_nuts3 <- x %>% group_by(nuts3)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
  x.nuts3 <- by_nuts3 %>% summarise_at(vbles, functs, ...)
  
  colnames(x.nuts3)[1] <- "spatunit"  #changing 1st column name
  
  ## Computing statistics per CAPRI NUTS II ##
  
  print("Computing statistics per CAPRI NUTS II...")
  
  
  by_CAPRI_NUTSII <- x %>% group_by(CAPRI_NUTSII)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
  x.caprinuts <- by_CAPRI_NUTSII %>% summarise_at(vbles, functs, ...)
  
  colnames(x.caprinuts)[1] <- "spatunit"  #changing 1st column name
  
  
  
  
  ## Computing statistics per country (CAPRI NUTS 0) ##
  
  print("Computing statistics per country (CAPRI NUTS 0...)")
  
  
  by_CAPRI_NUTS0 <- x %>% group_by(CAPRI_NUTS0)           #grouping the DataTable before to be passed to the summarise_at(), to be faster
  x.country <- by_CAPRI_NUTS0 %>% summarise_at(vbles, functs, ...)
  
  colnames(x.country)[1] <- "spatunit"  #changing 1st column name
  
  
  
  ## Combining by rows ##
  
  print("Creating a table with the results...")
  
  x.hsu.nuts2 <- rbind(x.hsu, x.nuts3, x.caprinuts, x.country)
  
  
  
  ## Exporting to a gdx file with ##
  
  print("Exporting to a gdx file...")
  
  
  x.hsu.nuts2_noNA <- x.hsu.nuts2[complete.cases(x.hsu.nuts2$Spatial_unit), ]  # to remove NA's
  x.hsu.nuts2_noNA <- as.data.frame(x.hsu.nuts2_noNA)
  
  #functs <- c("max", "min", "weighted.mean", "sd", "median")
  x.hsu.nuts2_longDF <- data.frame()
  for (fct in 1:length(functs)){                        #This is to stack the DataFrame in order to have a "long" format table, appropriate to be transformed to a gsx
    
    wideDF <- x.hsu.nuts2_noNA[ , grepl(functs[fct], names(x.hsu.nuts2_noNA))]
    longDF <- melt(x.hsu.nuts2_noNA,
                   id.vars=c("Spatial_unit"),
                   measure.vars = names(wideDF),
                   variable.name = "data",
                   value.name = functs[fct])
    
    if (fct==1){
      x.hsu.nuts2_longDF <- longDF
      x.hsu.nuts2_longDF$data <- as.character(x.hsu.nuts2_longDF$data)
      pattern <- paste0("_", functs[fct])
      x.hsu.nuts2_longDF$data <- gsub(pattern, "", x.hsu.nuts2_longDF$data, fixed = TRUE)
      
    }else{
      x.hsu.nuts2_longDF <- cbind(x.hsu.nuts2_longDF, longDF[, -c(1, 2)])
      ncls  <- ncol(x.hsu.nuts2_longDF)
      names(x.hsu.nuts2_longDF)[ncls] <- functs[fct]
    }
  }
  
  x.hsu.nuts2_longDF[is.na(x.hsu.nuts2_longDF)] <- 0    #to convert all NA's to 0
  
  #symDim <- 3
  symDim <- length(vbles) + 1 
  nm_table <- deparse(substitute(x))
  attr(x.hsu.nuts2_longDF,"symName") <- nm_table
  #attr(dem.hsu.nuts2_longDF, "ts") <- "DEM per spatial unit"   #explanatory text for the symName
  myText <- c("spatial entity", "statistics")     # explanatory text for the extracted index sets
  
  lst <- wgdx.reshape(x.hsu.nuts2_longDF, symDim, tName = "stats", setNames = myText)   #to reshape the DT before to write the gdx. tName is the index set name for the new index position created by reshaping 
  
  #wgdx.lst("forest.hsu.nuts2_kk2.gdx", lst)
  wgdx.lst(paste0(nm_table, ".hsu.nuts2_kk3.gdx"), lst)
  
  
  print("End of Function 1")
  
  end <- Sys.time() - start
  print(end)
  
  return(x.hsu.nuts2)
  
  
}   

## Function 2.1: Multifunction used in func2 to compute several HSU - USCIE statistics 

multi.fun.2.1 <- function(y) {
  c(min = min(y), max = max(y), mean = mean(y), sd = sd(y), median = median(y))
  
} #End of Function 2.1

## Function 2: Computing HSU - USCIE statistics

func2 <- function(x){
  
  ## computing HSU - USCIE statistics ##
  
  start <- Sys.time()
  print("Computing HSU - USCIE statistics...")
  
  count.hsu.uscie <-count_(x, vars="hsu2")
  stats.hsu.uscie <- round(multi.fun.2.1(count.hsu.uscie$n), 2)
  
  #save(stats.hsu.uscie, file ="stats.hsu.uscie.rdata")  # If Func2 is included to Func1, this line should be activated in order to save the result in disk
  #load(file = "stats.hsu.uscie.rdata")   # If Func2 is included to Func1, this line should be activated in order to load the result in the environment
  
  
  print("End of Function 2")
  end <- Sys.time() - start
  print(end)
  
  return(stats.hsu.uscie)
  
  
}  # End of Function 2

## Input 1: USCIE-HSU2 table
# It relates USCIE codes with HSU2 codes
#coming from USCIE_HSU2.CSV. Reading the file directly from the CSV with fread(). 
hsu_uscie1 <- fread("USCIE_HSU2.csv", header=TRUE) # fread() function is much faster than load()
# You get a DataTable
names(hsu_uscie1) <- c("s_uscierc","s_hsu2")
setkey(hsu_uscie1, "s_uscierc") # to set a key column of the DataTable
#export as gdx

#str(hsu_uscie1)
#head(hsu_uscie1)
#tables()


## Input 2: HSU2-NUTS-CAPRI
# It relates HSU2 codes with NUTS2, CAPRI_NUTSII, CAPRI_NUTS0 (countries). It also include the area of each HSU

#hsu2 <- read.csv(file="HSU2_DEFPARAM.csv")
#nuts_capri <- read.table(file="HSU2_NUTS_TO_CAPRI_NUTS_CODES.txt",sep="\t",header = T)
#hsu2.nuts3 <- hsu2[,c(2,3,11,22)]
#names(hsu2.nuts3) <- c("hsu2","area","nuts3","SMU")
#hsu2.nuts <- join(hsu2.nuts3,nuts_capri)
#save(hsu2.nuts,file="hsu2.nuts.rdata")
load(file = "hsu2.nuts.rdata")
#head(hsu2.nuts)
hsu2.nutsDT <- as.data.table(hsu2.nuts) # to transform hsu2.nuts to a DataTable
hsu2.nutsDT$hsu2 <- factor(hsu2.nutsDT$hsu2) # to transform the column to factor
setkey(hsu2.nutsDT, "hsu2") # to set a key column
#export as gdx
rm(hsu2.nuts)
#str(hsu2.nutsDT)
#head(hsu2.nutsDT)
#tables()
#
