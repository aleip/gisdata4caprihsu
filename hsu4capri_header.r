#Used Packages
library(data.table)
library(plyr)
library(dplyr)    #installed the dev version from https://github.com/hadley/dplyr
#library(reshape)
library(reshape2)
library(stats)
library(gdxrrw)

usciedatapath<-"s:/Actions/AGRIENV/base_data/ELISA/Datasets/uscie/"
usciedatapath<-paste0(usciedatapath,"hsu2_database_update_2016_02orig/")

#Set working directory and load of general (used to update all) datasets.
# These databases have been updated by Renate Koeble and delivered in the folder capri/hsu2_database_update_2016_02
if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
    gamspath<-"C:/GAMS/win64/24.4"
    workpath<-"x:/adrian/tools/rprojects/gisdata4caprihsu/"
}else if(Sys.info()[4]=="D01RI1600881"){ #checks machine name
    gamspath<-"C:/GAMS/win64/24.4"
    workpath<-"x:/adrian/tools/rprojects/gisdata4caprihsu/"
    
}else{
    workpath<-"X:/MARS_disaggregation/hsu2_statistics_xavi"
    usciedatapath<-workpath
    gamspath<-"X:/GAMS/win64/24.7"
}

setwd(workpath)


#link with gams directory
igdx(gamspath)

## Function 1: Computing statistics per HSU, NUTS 3-2-0.
# 
# NOTES FOR THIS FUNCTION: 
# The first argument (x) is the data table
# data2ag is to define if you want to aggregate data by uscie or hsu
# Pass the functions (statistics) to be computed as arguments (e.g. functs = c("weighted.mean", "max"))
# Pass the variables to compute (e.g. vbles = c("f_2000", "f_2006"))
# If desired, provide na.rm = TRUE
# If weighted mean wants to be computed, in the data table must be one column called "area". If it is not provided, an error message is issued and the function stops
# filenm4gdx is to define the filename for the exported gdx


# Comments al20160822 - func1 is not correct. the aggregation should always start with uscie - unless no 'uscies' are the start 

func1 <- function(x, data2ag, functs, vbles, filenm4gdx, ...){
    
    start <- Sys.time()
    
    print("Preparing to compute statistics...")
    
    if(tolower(data2ag) == "uscie"){
        
        # Make sure that there is a USCIE column
        if(!any(grepl("uscie", names(x))==TRUE)) stop("There isn't any USCIE column. Are you sure about data2ag=uscie???", call. = FALSE)
        
        
        # Check if a weighted mean is passed, it wouldn't make sense for USCIE data
        if(any(functs == "weighted.mean")) stop("It makes no sense to compute weighted mean for USCIE data", call. = FALSE)
        
        
        ## computing statistics per HSU ##
        print("Computing statistics per HSU...")
        
        #  
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
        
        if(length(functs)==1 && length(vbles)>1){
            colnames(x.hsu.nuts2)[-1] <- paste(colnames(x.hsu.nuts2)[-1], functs, sep = "_")
        }else if(length(functs)>1 && length(vbles)==1){
            colnames(x.hsu.nuts2)[-1] <- paste(vbles, colnames(x.hsu.nuts2)[-1], sep = "_")
        }else if(length(functs)==1 && length(vbles)==1){
            colnames(x.hsu.nuts2)[-1] <- paste(colnames(x.hsu.nuts2)[-1], functs, sep = "_")
        }
        
        
    }else if(tolower(data2ag) == "hsu"){
        
        ## Check if a weighted mean is calculated - set the HSMU area as weighting parameter
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
        
        x.hsu.nuts2 <- rbind(x.nuts3, x.caprinuts, x.country)
        
        if(length(functs)==1 && length(vbles)>1){
            colnames(x.hsu.nuts2)[-1] <- paste(colnames(x.hsu.nuts2)[-1], functs, sep = "_")
        }else if(length(functs)>1 && length(vbles)==1){
            colnames(x.hsu.nuts2)[-1] <- paste(vbles, colnames(x.hsu.nuts2)[-1], sep = "_")
        }else if(length(functs)==1 && length(vbles)==1){
            colnames(x.hsu.nuts2)[-1] <- paste(colnames(x.hsu.nuts2)[-1], functs, sep = "_")
        }
        
        names(x.hsu.nuts2) <- gsub("weighted.mean\\(., area\\)", "weighted.mean", names(x.hsu.nuts2))
        
        
    }else{
        stop("Please, provide a correct value for the argument data2ag (i.e. uscie or hsu)", call. = FALSE)
    }
    
    
    
    ## Exporting to a gdx file with ##
    
    print("Exporting to a gdx file...")
    
    x.hsu.nuts2_noNA <- x.hsu.nuts2[complete.cases(x.hsu.nuts2$s_spatunit), ]  # to remove NA's
    x.hsu.nuts2_noNA <- as.data.frame(x.hsu.nuts2_noNA)
    nm_table <- deparse(substitute(x))   # to extract the name of the table
    x.hsu.nuts2_longDF <- data.frame()
    
    for (fct in 1:length(functs)){            #This is to stack the DataFrame in order to have a "long" format table, appropriate to be transformed to a gsx
        
        if(functs[fct]=="weighted.mean(., area)"){
            newfctnm <- "weighted.mean"
        }else{
            newfctnm <- functs[fct]
        }
        
        wideDFnms <- grep(newfctnm, names(x.hsu.nuts2_noNA), value = TRUE)
        longDF <- melt(x.hsu.nuts2_noNA,
                       id.vars=c("s_spatunit"),
                       measure.vars = wideDFnms,
                       variable.name = "s_variable",
                       value.name = newfctnm)
        
        if (fct==1){
            x.hsu.nuts2_longDF <- longDF
            x.hsu.nuts2_longDF$s_variable <- as.character(x.hsu.nuts2_longDF$s_variable)
            pattern <- paste0("_", newfctnm)
            x.hsu.nuts2_longDF$s_variable <- gsub(pattern, "", x.hsu.nuts2_longDF$s_variable, fixed = TRUE)
            
            
        }else{
            x.hsu.nuts2_longDF <- cbind(x.hsu.nuts2_longDF, longDF[, -c(1, 2)])
            ncls  <- ncol(x.hsu.nuts2_longDF)
            names(x.hsu.nuts2_longDF)[ncls] <- newfctnm
        }
    }
    
    x.hsu.nuts2_longDF[is.na(x.hsu.nuts2_longDF)] <- 0    #to convert all NA's to 0
    
    
    symDim <- 3 
    attr(x.hsu.nuts2_longDF,"symName") <- paste0("p_", nm_table)
    attr(x.hsu.nuts2_longDF, "ts") <- paste0(nm_table, " statistics per spatial unit")   #explanatory text for the symName
    myText <- c("spatial entity", "variables", "statistics")     # explanatory text for the extracted index sets
    
    lst <- wgdx.reshape(x.hsu.nuts2_longDF, symDim, tName = "s_stats", setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping 
    
    #wgdx.lst(paste0(filenm4gdx, ".gdx"), lst)
    wgdx.lst(paste0(filenm4gdx, ".gdx"), lst)
    
    
    print(paste0("Exported gdx as ", paste0(filenm4gdx, ".gdx")))
    
    print("End of Function 1")
    
    end <- Sys.time() - start
    print(end)
    gc()
    
    return(x.hsu.nuts2)
    
    
}    #End of Funct1  




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





## Function 3: Preparing data set to run function 1

# NOTES FOR THIS FUNCTION: 
# It relates the data set with HSU2 (if it is based on USCIE) and NUTS. Also adds HSU areas
# The first argument (x) is the data set (DataFrame)
# Colname for USCIE codes has to be "s_uscierc". For HSU2 codes, "s_hsu2". For MARS GRID, "grid"


func3 <- function(x){
    
    print("Running function 3...")
    
    nmt <- deparse(substitute(x))
    
    if(any(grepl("s_uscierc", names(x))==TRUE)){  #to add HSU2, their areas, and NUTS data
        
        print(paste0("Relating ", nmt, " with HSU2 and NUTS codes"))  
        
        x$s_uscierc <- as.numeric(as.character(x$s_uscierc)) # to transform the column to numeric
        x2 <- as.data.table(x) # to transform x to a DataTable
        setkeyv(x2, "s_uscierc") # to set a key column
        
        usciehsu_x2 <- merge(x2, hsu_uscie1, by="s_uscierc", all.x=TRUE) #to join of old gdx file with new hsu. Type of joining "left"
        gc()
        usciehsu_x2$s_uscierc <- factor(usciehsu_x2$s_uscierc) # to transform the column to factor
        usciehsu_x2$s_hsu2 <- factor(usciehsu_x2$s_hsu2) # to transform the column to factor
        setkey(usciehsu_x2, "s_hsu2") # to set a key column
        
        x3 <- merge(usciehsu_x2, hsu2_nutsDT, by.y="hsu2", by.x="s_hsu2", all.x=TRUE)
        setnames(x3, old="s_hsu2", new="hsu2")
        
        
    }else if(any(grepl("s_hsu2", names(x))==TRUE)){  #to add hsu2 areas and NUTS data
        
        print(paste0("Relating ", nmt, " with NUTS codes"))  
        x$s_hsu2 <- gsub("U", "", x$s_hsu2)
        #x$s_hsu2 <- as.numeric(as.character(x$s_hsu2)) # to transform the column to numeric
        x2 <- as.data.table(x) # to transform x to a DataTable
        setkeyv(x2, "s_hsu2") # to set a key column
        gc()
        
        x3 <- merge(x2, hsu2_nutsDT, by.x="s_hsu2", by.y="hsu2", all.x=TRUE)
        setnames(x3, old="s_hsu2", new="hsu2")
        
        
    }else if(any(grepl("grid", names(x))==TRUE)){  #to add HSU2 and NUTS codes, and hsu2 areas
        
        print(paste0("Relating ", nmt, " with HSU and NUTS codes"))
        
        
        
    }else{
        
        print("Unknown spatial units of the data")
        print("You can choose 's_uscierc', 's_hsu2' or 'grid' as column name")
    }
    
    print("End of function 3")
    return(x3)
    
} #End of func3


dataprep<-paste0(usciedatapath,"Uscie_hsu2_nuts.rdata")
if(file.exists(dataprep)){
    load(file = dataprep)
}else{
    ### Input 1: USCIE-HSU2 table  ####
    # It relates USCIE codes with HSU2 codes
    #coming from USCIE_HSU2.CSV. Reading the file directly from the CSV with fread(). 
    hsu_uscie1 <- fread(paste0(usciedatapath,"USCIE_HSU2.csv"), header=TRUE) # fread() much faster than read.csv - output is a data table
    names(hsu_uscie1) <- c("s_uscierc","s_hsu2")
    setkey(hsu_uscie1, "s_uscierc") # to set a key column of the DataTable
    
    
    #Export to a gdx file   #xavi: I'm not sure that it is correctly done
    #al20160823 - i don't think the gdx generated is correct - but if this script works well (and fast)
    #             we do not need the uscie's themselves in CAPRI data base any more!!
    
    #hsu_uscie1[is.na(hsu_uscie1)] <- 0    #to convert all NA's to 0
    # symDim <- 2
    # attr(hsu_uscie1,"symName") <- "hsu_uscie"
    # attr(hsu_uscie1, "ts") <- "relates HSU2 codes and USCIE codes"   #explanatory text for the symName
    # myText <- c("hsu2 code", "uscie code")     # explanatory text for the extracted index sets
    # lst <- wgdx.reshape(hsu_uscie1, tName = "s_uscie", symDim, order=c(2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    # wgdx.lst("hsu2_uscie1", lst)
    
    
    
    #It relates HSU2 with old HSMU codes
    #hsuhsmu <- rgdx.param("hsu_hsmu.gdx","p_hsu_hsmu")#load of gdx linking old and new hsu,dataset coming from capri/dat/capdis/hsu2
    #hsuhsmu <- hsuhsmu[,1:2]
    #names(hsuhsmu) <- c("hsu2","hsmu")
    #hsuhsmu$hsu2 <- gsub("U","",hsuhsmu$hsu2)
    #hsuhsmu$hsmu <- gsub("H|U|HE","",hsuhsmu$hsmu)
    #head(hsuhsmu)
    
    
    ### Input 2: HSU2-NUTS-CAPRI  ####
    # It relates HSU2 codes with NUTS2(Eurostat codes), CAPRI_NUTSII, CAPRI_NUTS0 (countries). It also include the area of each HSU
    hsu2_nuts3 <- fread(paste0(usciedatapath,"HSU2_DEFPARAM.csv"), header=TRUE,select=c("HSU2_IDRUN","HSU2_TOTAR","ADMIN_EEZ","SMU")) 
    #hsu2_nuts3 <- as.data.table(hsu2[ , c(2,3,11,22), with=FALSE])  #to select the columns of the DT
    setnames(hsu2_nuts3,old=c("HSU2_IDRUN","HSU2_TOTAR","ADMIN_EEZ","SMU"),new=c("s_hsu2","area","nuts3","SMU")) #Same header as for the uscie table
    setkey(hsu2_nuts3, "s_hsu2") # to set a key column of the DataTable
    
    
    ### Input 3: CAPRI NUTS ####
    #al20160823 - original file is *csv
    #nuts_capri <- fread(paste0(usciedatapath,"HSU2_NUTS_TO_CAPRI_NUTS_CODES.txt"), header=TRUE) 
    #setkey(nuts_capri, "nuts3") # to set a key column of the DataTable
    nuts_capri <- fread(paste0(usciedatapath,"HSU2_NUTS_TO_CAPRI_NUTS_CODES.csv"), header=TRUE,drop="CAPRI_MS") 
    setnames(nuts_capri,old = c("ADMIN_EEZ","NURGCDL2"),new = c("nuts3","nuts2")) #keep CAPRI_NUTSII,CAPRI_NUTS0
    setkey(nuts_capri, "nuts3") # to set a key column of the DataTable
    hsu2_nuts <- merge(hsu2_nuts3, nuts_capri, by.x = "nuts3", by.y = "nuts3", all.x = TRUE)
    rm(hsu2_nuts3,nuts_capri) #Not needed any more
    
    hsu2_nuts$s_hsu2 <- factor(hsu2_nuts$s_hsu2) # to transform the column to factor
    setkey(hsu2_nuts, "s_hsu2") # to set a key column
    
    ### Input 4: Fraction of MARS-GRID per HSU2 ####
    #load csv file linking uscie and grid numbers, dataset coming from capri/dat/capdis/uscie
    uscie_marsgrid <- fread(paste0(usciedatapath,"../hsu2_database_togdx_201605_nocita/","USCIE_PARAM.csv"), header=TRUE,select=c(1, 3)) 
    setnames(uscie_marsgrid,old=c("USCIE_RC","GRIDNO"),new=c("s_uscierc","marsgrid"))
    
    # Merge uscie - marsgrid - with HSU2; eliminate empty HSU2 and MARS-grids
    setkey(hsu_uscie1,"s_uscierc")
    setkey(uscie_marsgrid,"s_uscierc")
    hsu_uscie_marsgrid <- hsu_uscie1[uscie_marsgrid][!is.na(s_hsu2)&marsgrid!=0]
    hsu_uscie_marsgrid<-hsu_uscie_marsgrid[,.(area=.N),by=.(s_hsu2,marsgrid)]
    hsu_uscie_marsgrid<-hsu_uscie_marsgrid[,`:=`(gridarea=sum(area)),by=marsgrid]
    hsu_uscie_marsgrid<-hsu_uscie_marsgrid[,`:=`(fraction=area/gridarea)]
    
    ### Save data in rdata format ####
    save(hsu2_nuts,hsu_uscie1,hsu_uscie_marsgrid,file=dataprep)
}
#al20160823 it is already a data table
#hsu2_nutsDT <- as.data.table(hsu2_nuts) # to transform hsu2_nuts to a DataTable


#Export to a gdx file

hsu2_nuts <- hsu2_nuts[complete.cases(hsu2_nuts), ]  # to remove NA's
symDim <- 7
attr(hsu2_nuts,"symName") <- "hsu_nuts_capri"
attr(hsu2_nuts, "ts") <- "relates HSU2 codes (and areas) with NUTS2, CAPRI_NUTSII, CAPRI_NUTS0"   #explanatory text for the symName
myText <- c("country code","nuts2 code of capri","nuts2 code","soil mapping unit","nuts3 code","hsu2 code","area of hsu2")     # explanatory text for the extracted index sets
lst <- wgdx.reshape(hsu2_nuts, symDim,tName = "area", setsToo=FALSE,order=c(7,6,5,4,3,1,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
wgdx.lst("hsu2_nuts1", lst)

remove(hsu2_nuts)
remove(hsu2_nuts3)


## Input 4: HSU-METEOGRID

# activate <- 0  # if activate=0, this part of code is ommited
# # if activate=1, this part of code runs, but it takes more than two days to calculate the results
# 
# if (activate==1){
#     
#     
#     hsu_grid <- hsu_uscie_marsgrid[,c(2, 3), with=FALSE] #removing USCIE col
#     str(hsu_grid)
#     
#     uniq_hsu_grid <- unique(hsu_grid)
#     head(uniq_hsu_grid)
#     
#     hsu_frac <- data.frame(s_hsu2=character(), GRIDNO=character(), frac=numeric(), stringsAsFactors=FALSE)
#     
#     for (i in 1:nrow(uniq_hsu_grid)){   # To compute What fraction of each HSU is occuped by each GRID
#         #for (i in 10000:10010){
#         #for (i in 9700:10100){
#         
#         #print(uniq_hsu_grid[i])
#         hsu <- uniq_hsu_grid[i]$s_hsu2
#         grid <- uniq_hsu_grid[i]$GRIDNO
#         
#         #sel <- subset(hsu_grid, s_hsu2==hsu & GRIDNO==grid)
#         sel <- hsu_grid[(s_hsu2==hsu & GRIDNO==grid)]
#         nrw <- nrow(sel)
#         
#         #sel2 <- subset(hsu_grid, s_hsu2==hsu)
#         sel2 <- hsu_grid[s_hsu2==hsu]
#         nrw2 <- nrow(sel2)
#         
#         frac <- nrw/nrw2
#         
#         hsu_frac[i,1] <- hsu
#         hsu_frac[i,2] <- grid
#         hsu_frac[i,3] <- frac
#         
#     }    
#     
#     
#     
#     View(hsu_frac)
#     uniq_hsu_grid[uniq_hsu_grid$s_hsu2==501792,]
#     hsu_grid[s_hsu2==419435]
#     hsu_frac[hsu_frac$s_hsu2==419435,]
#     subset(hsu_frac, s_hsu2==419238)
#     
#     
#     
#     
# } #End if activate==1



