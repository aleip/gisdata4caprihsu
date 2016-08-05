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



## Function 1: Computing statistics per HSU, NUTS 3-2-0.
# 
# NOTES FOR THIS FUNCTION: 
# The first argument (x) is the data table
# data2ag is to define the original data to be aggregated (i.e. uscie or hsu)
#    xavi: the data which I've been working with is based on USCIE,
#          so it makes no sense to compute weighted mean.
#          However, I've split the function in two parts: 
#          the first (if data2ag == "uscie") to compute statistics 
#          aggregating USCIE data (as done so far), and the second 
#          (if data2ag == "uscie") to aggragate data by HSU.
#          Do we have data (forest, dem, etc.) at HSU level to check
#          if the second part of the function is working properly? 
#          I think that there is still a problem with weighted mean.
# Pass the functions (statistics) to be computed as arguments (e.g. functs = c("weighted.mean", "max"))
# Pass the variables to compute (e.g. vbles = c("f_2000", "f_2006"))
# If desired, provide na.rm = TRUE
# If weighted mean wants to be computed, in the data table must be one column called "area". 
# If it is not provided, an error message is issued and the function stops

func1 <- function(x, data2ag, functs, vbles, ...){
  
  start <- Sys.time()
  
  print("Preparing to compute statistics...")
  
  if(tolower(data2ag) == "uscie"){
    
    # A) Check if a weighted mean is passed, it wouldn't make sense for USCIE data
    
    if(any(functs == "weighted.mean")) stop("It makes no sense to compute weighted mean for USCIE data", call. = FALSE)
    
    
    
    ## computing statistics per HSU ##
    print("Computing statistics per HSU...")
    
    # B) 
    by_hsu2 <- x %>% group_by(hsu2)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
    x.hsu <- by_hsu2 %>% summarise_at(vbles, functs, ...)    # summarise_at() is a new function included in the new Dev version of dplyr. It will replace the old summarise_each()
    
    colnames(x.hsu)[1] <- "s_spatunit"  #changing 1st column name
    x.hsu$s_spatunit <- paste0("U", x.hsu$s_spatunit) #adding "U" in front of spatial units
    
    
    ## Computing statistics per NUTS 3 ##
    print("Computing statistics per NUTS III...")
    
    by_nuts3 <- x %>% group_by(nuts3)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
    x.nuts3 <- by_nuts3 %>% summarise_at(vbles, functs, ...)
    
    colnames(x.nuts3)[1] <- "s_spatunit"  #changing 1st column name
    
    
    ## Computing statistics per CAPRI NUTS II ##
    
    print("Computing statistics per CAPRI NUTS II...")
    
    
    by_CAPRI_NUTSII <- x %>% group_by(CAPRI_NUTSII)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
    x.caprinuts <- by_CAPRI_NUTSII %>% summarise_at(vbles, functs, ...)
    
    colnames(x.caprinuts)[1] <- "s_spatunit"  #changing 1st column name
    
    
    
    ## Computing statistics per country (CAPRI NUTS 0) ##
    
    print("Computing statistics per country (CAPRI NUTS 0...)")
    
    
    by_CAPRI_NUTS0 <- x %>% group_by(CAPRI_NUTS0)           #grouping the DataTable before to be passed to the summarise_at(), to be faster
    x.country <- by_CAPRI_NUTS0 %>% summarise_at(vbles, functs, ...)
    
    colnames(x.country)[1] <- "s_spatunit"  #changing 1st column name
    
    
    
    ## Combining by rows ##
    
    print("Creating a table with the results...")
    
    x.hsu.nuts2 <- rbind(x.hsu, x.nuts3, x.caprinuts, x.country)
    
    
    
  

  }else if(tolower(data2ag) == "hsu"){
    
    
    # A) Check if a weighted mean is calculated - set the HSMU area as weighting parameter
    for (i in 1:length(functs)){   # this loop is to add the argument "area", nedded to compute weighted mean
      if (functs[i] == "weighted.mean"){      # this is to tell the function how to compute weighted mean
        functs[i] <- "weighted.mean(., area)"   
        if(!"area" %in% colnames(x)) stop("please, provide a column called 'area' with areas", call. = FALSE)   # If a column called "area" with areas is not provided, an error message is issued and the function stops
      } else {
        functs[i] <- functs[i]
      }
    }
    
    
    ## Computing statistics per NUTS III ##
    print("Computing statistics per NUTS III...")
    
    by_nuts3 <- x %>% group_by(nuts3)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
    x.nuts3 <- by_nuts3 %>% summarise_at(vbles, functs, ...)
    
    colnames(x.nuts3)[1] <- "s_spatunit"  #changing 1st column name
    
    
    ## Computing statistics per CAPRI NUTS II ##
    
    print("Computing statistics per CAPRI NUTS II...")
    
    
    by_CAPRI_NUTSII <- x %>% group_by(CAPRI_NUTSII)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
    x.caprinuts <- by_CAPRI_NUTSII %>% summarise_at(vbles, functs, ...)
    
    colnames(x.caprinuts)[1] <- "s_spatunit"  #changing 1st column name
    
    
    
    
    ## Computing statistics per country (CAPRI NUTS 0) ##
    
    print("Computing statistics per country (CAPRI NUTS 0...)")
    
    
    by_CAPRI_NUTS0 <- x %>% group_by(CAPRI_NUTS0)           #grouping the DataTable before to be passed to the summarise_at(), to be faster
    x.country <- by_CAPRI_NUTS0 %>% summarise_at(vbles, functs, ...)
    
    colnames(x.country)[1] <- "s_spatunit"  #changing 1st column name
    
    
    
    ## Combining by rows ##
    
    print("Creating a table with the results...")
    
    x.hsu.nuts2 <- rbind(x.hsu, x.nuts3, x.caprinuts, x.country)
    
    
    

    
  }else{
    stop("Please, provide a correct value for the argument data2ag (i.e. uscie or hsu)", call. = FALSE)
  }
  

  
  ## Exporting to a gdx file with ##
  
  print("Exporting to a gdx file...")
  
  
  x.hsu.nuts2_noNA <- x.hsu.nuts2[complete.cases(x.hsu.nuts2$s_spatunit), ]  # to remove NA's
  x.hsu.nuts2_noNA <- as.data.frame(x.hsu.nuts2_noNA)
  nm_table <- deparse(substitute(x))   # to extract the name of the table

  x.hsu.nuts2_longDF <- data.frame()
  for (fct in 1:length(functs)){                        #This is to stack the DataFrame in order to have a "long" format table, appropriate to be transformed to a gsx
    
    print(functs[fct])
    wideDF <- x.hsu.nuts2_noNA[ , grepl(functs[fct], names(x.hsu.nuts2_noNA))]
    longDF <- melt(x.hsu.nuts2_noNA,
                   id.vars=c("s_spatunit"),
                   measure.vars = names(wideDF),
                   variable.name = "s_variable",
                   value.name = functs[fct])

    if (fct==1){
      x.hsu.nuts2_longDF <- longDF
      x.hsu.nuts2_longDF$s_variable <- as.character(x.hsu.nuts2_longDF$s_variable)
      pattern <- paste0("_", functs[fct])
      x.hsu.nuts2_longDF$s_variable <- gsub(pattern, "", x.hsu.nuts2_longDF$s_variable, fixed = TRUE)
      
    }else{
      x.hsu.nuts2_longDF <- cbind(x.hsu.nuts2_longDF, longDF[, -c(1, 2)])
      ncls  <- ncol(x.hsu.nuts2_longDF)
      names(x.hsu.nuts2_longDF)[ncls] <- functs[fct]
    }
    print(head(x.hsu.nuts2_longDF))
  }
  
  x.hsu.nuts2_longDF[is.na(x.hsu.nuts2_longDF)] <- 0    #to convert all NA's to 0
  
  symDim <- length(vbles) + 1 
  attr(x.hsu.nuts2_longDF,"symName") <- paste0("p_", nm_table)
  attr(x.hsu.nuts2_longDF, "ts") <- paste0(nm_table, " per spatial unit")   #explanatory text for the symName
  myText <- c("spatial entity", "variables", "statistics")     # explanatory text for the extracted index sets

  lst <- wgdx.reshape(x.hsu.nuts2_longDF, symDim, tName = "s_stats", setNames = myText)   #to reshape the DT before to write the gdx. tName is the index set name for the new index position created by reshaping 
  
  wgdx.lst(paste0(nm_table, ".hsu.nuts2.gdx"), lst)
  
  
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





## Function 3: Preparing data set to run function 2

# NOTES FOR THIS FUNCTION: 
# The first argument (x) is the data set (DataFrame)
# spunit in the spatial unit (e.g. s_uscierc, s_hsu). By default spunit = s_uscierc


func3 <- function(x, spunit = c("s_uscierc")){

  print("Running function 3...")
  x[[spunit]] <- as.numeric(as.character(x[[spunit]])) # to transform the column to numeric
  x2 <- as.data.table(x) # to transform x to a DataTable
  setkeyv(x2, spunit) # to set a key column

  usciehsu_x2 <- merge(x2, hsu_uscie1, by=spunit, all.x=TRUE) #to join of old gdx file with new hsu. Type of joining "left"
  usciehsu_x2[[spunit]] <- factor(usciehsu_x2[[spunit]]) # to transform the column to factor
  usciehsu_x2$s_hsu2 <- factor(usciehsu_x2$s_hsu2) # to transform the column to factor
  setkey(usciehsu_x2, "s_hsu2") # to set a key column
  
  x3 <- merge(usciehsu_x2, hsu2.nutsDT, by.y="hsu2", by.x="s_hsu2", all.x=TRUE)
  setnames(x3, old="s_hsu2", new="hsu2")
  
  print("End of function 3")
  return(x3)

} #End of func3






## Input 1: USCIE-HSU2 table
# It relates USCIE codes with HSU2 codes
#coming from USCIE_HSU2.CSV. Reading the file directly from the CSV with fread(). 
hsu_uscie1 <- fread("USCIE_HSU2.csv", header=TRUE) # fread() function is much faster than load()
# You get a DataTable
names(hsu_uscie1) <- c("s_uscierc","s_hsu2")
setkey(hsu_uscie1, "s_uscierc") # to set a key column of the DataTable
#export as gdx       xavi: How can I export as gdx without data? THer are only two columns (uscie and hsu codes) 
#str(hsu_uscie1)
#head(hsu_uscie1)

#Export to a gdx file

hsu_uscie2 <- as.data.frame(hsu_uscie1)
#hsu_uscie1[is.na(hsu_uscie1)] <- 0    #to convert all NA's to 0
symDim <- 2
attr(hsu_uscie2,"symName") <- "hsu_uscie"
attr(hsu_uscie2, "ts") <- "relates HSU2 codes USCIE codes"   #explanatory text for the symName
#myText <- c("hsu2 code", "uscie code")     # explanatory text for the extracted index sets
#lst <- wgdx.reshape(hsu2.nuts, symDim, tName = "area", order=c(2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
lst <- wgdx.reshape(hsu_uscie2, symDim, order=c(2,1,0))   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
wgdx.lst("hsu2_uscie1", lst)

remove(hsu_uscie2)





## Input 2: HSU2-NUTS-CAPRI
# It relates HSU2 codes with NUTS2(Eurostat codes), CAPRI_NUTSII, CAPRI_NUTS0 (countries). It also include the area of each HSU

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

#Export to a gdx file

hsu2.nuts <- hsu2.nuts[complete.cases(hsu2.nuts), ]  # to remove NA's
symDim <- 7
attr(hsu2.nuts,"symName") <- "hsu_nuts_capri"
attr(hsu2.nuts, "ts") <- "relates HSU2 codes (and areas) with NUTS2, CAPRI_NUTSII, CAPRI_NUTS0"   #explanatory text for the symName
myText <- c("country code","nuts2 code of capri","nuts2 code","soil mapping unit","nuts3 code","hsu2 code","area of hsu2")     # explanatory text for the extracted index sets
lst <- wgdx.reshape(hsu2.nuts, symDim, tName = "area", order=c(7,6,5,4,3,1,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
wgdx.lst("hsu2_nuts1", lst)

rm(hsu2.nuts)
#str(hsu2.nutsDT)
#head(hsu2.nutsDT)
