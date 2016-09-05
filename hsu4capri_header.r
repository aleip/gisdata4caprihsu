#Used Packages
rm(list=objects())
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
    workpath<-"C:/adrian/tools/rprojects/gisdata4caprihsu/"
    capridat<-"C:/adrian/models/capri/trunk20160810/dat/capdis/hsu2/"
}else if(Sys.info()[4]=="D01RI1600881"){ #checks machine name
    gamspath<-"C:/GAMS/win64/24.4"
    workpath<-"x:/adrian/tools/rprojects/gisdata4caprihsu/"
    capridat<-"x:/adrian/models/capri/trunk20160810/dat/capdis/hsu2/"
}else{
    workpath<-"X:/MARS_disaggregation/hsu2_statistics_xavi2/"
    usciedatapath<-workpath
    capridat<-workpath
    gamspath<-"X:/GAMS/win64/24.7"
}

setwd(workpath)


#link with gams directory
igdx(gamspath)



agguscie2hsu<-function(x2agg,xfrom,xto,xvbles,dim,functs, varbles){
  
  
  #For weighted mean return the weighting column (area) otherwise leave functs unchanged
  for (i in 1:length(functs)){   # this loop is to add the argument "area", nedded to compute weighted mean
    if (functs[i] == "weighted.mean(., area)"){      # this is to tell the function how to compute weighted mean
      if(!"area" %in% colnames(x2agg)) stop("please, provide a column called 'area' with areas", call. = FALSE)   # If a column called "area" with areas is not provided, an error message is issued and the function stops
    }
  }
  
  x2agg$i<-as.numeric(as.character(x2agg$i))
  setkey(x2agg,i)
  setnames(x2agg,old="i",new=xfrom)
  
  if(xfrom=="s_uscierc"){
    print(paste0("Linking ", xfrom, " data with ", xto))
    #setkey(uscie_hsu,s_uscierc) # xavi: this is already done
    xhsu<-x2agg[uscie_hsu]   # merge HSU (left join)
    x2agg<-xhsu[complete.cases(xhsu)] 
    setnames(x2agg,old="hsu",new="xto")
    
    
  }else if(xfrom=="marsgrid"){
    print(paste0("Linking ", xfrom, " data with ", xto))
    setkey(marsgrid_hsu, marsgrid)
    xhsu<-x2agg[marsgrid_hsu, allow.cartesian=TRUE]
    if(dim==3){
      x2agg <- xhsu[,.(value =  weighted.mean(mean, fraction)), by=.(hsu, j, k)]  #xavi: this step takes a bit long
    }else{  #xavi: for now, always dim is 2 or 3
      x2agg <- xhsu[,.(value =  weighted.mean(mean, fraction)), by=.(hsu, j)]
    }
    setnames(x2agg, old=c(xto, "value"), new=c("xto", "mean"))
    
    
  }else if(xfrom=="emepgrid"){
    print(paste0("Linking ", xfrom, " data with ", xto))  
    
    if(any(grepl("WD", varbles)==TRUE)){   #This is for Wet Deporition
      
      dep_unique <- unique(x2agg, by="i50_j50")
      longDT <- melt(dep_unique,
                     id.vars = "i50_j50",
                     measure.vars = varbles,
                     variable.name = "j",
                     value.name = "value")
      setkey(longDT, i50_j50) # to set a key column of the DataTable
      setkey(hsu2_emepgrid_area, i50_j50)
      emep_hsu<-longDT[hsu2_emepgrid_area, allow.cartesian=TRUE]   # merge HSU (left join)
      x2agg <- emep_hsu[,.(value =  weighted.mean(value, frac_emep_hsu)), by=.(hsu, j)]
      
    }else if(any(grepl("DD", varbles)==TRUE)){  #This is for Dry deposition
      dep_unique <- unique(x2agg, by=c("i50_j50", "EMEP_LC_CLASS"))
      longDT <- melt(dep_unique,
                     id.vars = c("i50_j50", "EMEP_LC_CLASS"),
                     measure.vars = varbles,
                     variable.name = "j",
                     value.name = "value")
      setkey(longDT, i50_j50) # to set a key column of the DataTable
      setkey(hsu2_emepgrid_lc_area, i50_j50)
      emep_hsu <- merge(longDT, hsu2_emepgrid_lc_area, by = c("i50_j50", "EMEP_LC_CLASS"), allow.cartesian=TRUE)
      x2agg <- emep_hsu[,.(value =  weighted.mean(value, frac_emep_lu_hsu)), by=.(hsu, j)]
    }
    setnames(x2agg, old=c("hsu", "value"), new=c("xto", "mean"))
    setkey(x2agg, xto)
    
    
  }else{   #(if xfrom==hsu)
    #setnames(x2agg,old="hsu",new="xto")
    #print(paste0("Computing statistics from ", xfrom, " to ", xto, "!"))
  }
  
  
  
  if(xfrom=="s_uscierc" | xfrom=="hsu"){
    if(xfrom=="hsu") setnames(x2agg,old=xto,new="xto")
    if(xfrom=="s_uscierc"){
      setnames(x2agg,old="mean", new="value")
      xvbles <- "value"  #xavi: if it's called the same that the function to compute ("mean"), summarise_at tries to compute SD and median from the just computed new column for the means
    } 
    
    print(paste0("Computing statistics from ", xfrom, " to ", xto, "!"))
    print(paste0("Computing ", paste0(functs, collapse = ", ")))
    
    
    if(dim==1){
      xres<-as.data.table(summarise_at(group_by(x2agg,xto),xvbles, functs, na.rm=TRUE))
      
    }else if(dim==2){
      xres<-as.data.table(summarise_at(group_by(x2agg,j,xto),xvbles, functs, na.rm=TRUE))
      #xres<-as.data.table(summarise_at(group_by(xhsu2,j,hsu),xvbles, functs, na.rm=TRUE))
      
    }else if(dim==3){
      xres<-as.data.table(summarise_at(group_by(x2agg,j,k,xto),xvbles, functs, na.rm=TRUE))
      
    }else{
      stop("Function aggs_uscierc2hsu not yet set-up for more than 3 dimensions")
    }
    
  }else{
    
    xres <- x2agg
    
  }
  
  
  for (jjj in (dim+1):ncol(xres)) set(xres,which(is.na(xres[[jjj]])),jjj,0)
  setkey(xres,xto)
  setnames(xres,new="i",old="xto")
  xresn<-c(letters[9:(9+dim-1)])
  xresn<-c(xresn,colnames(xres)[!colnames(xres)%in%xresn])
  xres<-xres[,xresn,with=FALSE]
  colnames(xres)<-gsub("weighted.mean","mean",colnames(xres))
  #hsu-colname required for preparedata
  if(xfrom=="s_uscierc" | xfrom=="emepgrid" | xfrom=="marsgrid") setnames(xres,old="i",new="hsu")
  return(xres)  
  
} #end of agguscie2hsu




preparedata <- function(xstart){
  
  print("Adding NUTS info...")
  if(any(grepl("hsu", names(xstart))==TRUE)){  #to add hsu areas and NUTS data
    
    #setnames(xstart,old="i",new="hsu")
    xstart$hsu <- as.numeric(as.character(xstart$hsu))
    setkey(xstart, hsu) # to set a key column
    hsu2_nuts$hsu <- as.numeric(as.character(hsu2_nuts$hsu))
    setkey(hsu2_nuts, hsu) # to set a key column
    x3<-xstart[hsu2_nuts]
    x3<-x3[!is.na(hsu)]
    setkey(x3, hsu) # to set a key column
    setnames(x3,new="i",old="hsu")
    
    #}else if(any(grepl("grid", names(xstart))==TRUE)){  #to add HSU2 and NUTS codes, and hsu2 areas
    
    #   print(paste0("Relating ", nmt, " with HSU and NUTS codes"))
    
  }else{
    stop("Unknown spatial units of the data, It should be 'hsu'")
  }
  
  return(x3)
} #End of preparedata



loadgdxfile<-function(xfulln, parname=NULL, sets=NULL){
  pars<-as.data.table(gdxInfo(xfulln,dump=FALSE,returnList=FALSE,returnDF=TRUE)$parameters$name)
  dims<-as.data.table(gdxInfo(xfulln,dump=FALSE,returnList=FALSE,returnDF=TRUE)$parameters$dim)
  
  if(is.null(parname)){
    if(nrow(pars)>1){  #xavi: because it's a dataTable
      stop(paste("Data file contains several parameter, please indicate which: ", paste(pars$V1, collapse=" - ")))
    }else if (nrow(pars)==0){
      stop(paste("Data file contains no parameter"))
    }else{
      parname<-as.character(pars)
      dim<-as.numeric(dims)
    }
  }else{
    dim<-as.numeric(dims[pars$V1==parname])
    
  }
  #
  
  if(dim<4){
    #rgdx returns i,j,... for each dim and writes the values into the column 'value'
    gdxl<-c("j","k","l","m") #xavi: for what is this?
    x <- as.data.table(rgdx.param(xfulln,parname))
    
  }else if(dim>3 && is.null(sets)){
    
    x <- as.data.table(rgdx.param(xfulln,parname))
    print(head(x))
    print(paste0("num of dims = ", dim))
    print(paste0("parameter name = ", parname))
    stop("More than 3 dimensions, please choose which columns to be included (e.g. sets=c('i1'...))")
    
  }else if(dim>3 && !is.null(sets)){
    
    x <- as.data.table(rgdx.param(xfulln,parname))
    #x <- x[, .(sets)]
    x <- subset(x, select=sets)
    dim <- length(sets) - 1
  }
  
  return(list(x,dim,parname))
  
} #End of loadgdxfile



#loadcsv<-function(xfulln, spatunit="s_uscierc", varbls){
#  
#  csvfile<-fread(xfulln, header=TRUE)
#  #setkey(csvfile, s_uscierc) # to set a key column of the DataTable
#  # xavi: if Doing Melt here, not possible to join later with hsu-emep
#  longDT <- melt(csvfile,
#                 id.vars = spatunit,
#                 measure.vars = varbls,
#                 variable.name = "j",
#                 value.name = "mean")
#  return(longDT)
#  
#}# End of loadcsv




loadcsv<-function(xfulln, varbles){
  
  if(any(grepl("WD", varbles)==TRUE)){   #This is for Wet Deporition
    
    dep<-fread(xfulln, header=TRUE, select=c("i50_j50", varbles))
    setkey(dep,i50_j50) # to set a key column of the DataTable
    
  }else if(any(grepl("DD", varbles)==TRUE)){  #This is for Dry deposition
    dep<-fread(xfulln, header=TRUE, select=c("i50_j50","EMEP_LC_CLASS", varbles))
    setkey(dep,i50_j50) # to set a key column of the DataTable
  }
  
  dim <- 2   # for now, this is hardcoded
  
  nm <- strsplit(xfulln,"\\.")[[1]]
  nm <- strsplit(nm[1],"_")[[1]]
  nm <- tail(nm, 2)[1:2]
  parname <- paste(nm, collapse = "_")
  
  return(list(dep, dim, parname))
  
}# End of loadcsv


processdata<-function(xfulln,oldn=NULL,newn=NULL,spatunit="s_uscierc",parn=NULL,functs=c("max", "min", "mean", "sd", "median"), varbles=NULL, sets=NULL){
    #x: data table that contains one row with the original unit and further rows with variables of the name 'newn'
    #xn: part of the file names to be generated
    #oldn: variable names as in original data
    #newn: variable names as required in result files
    
    fileext<-strsplit(xfulln,"\\.")[[1]]
    fileext<-fileext[length(fileext)]
    
    if(fileext=="gdx"){
      # Load gdx-file (no other load-function has been develoed yet...)
      print("Loading gdx file...")
      x<-loadgdxfile(xfulln, parname=parn, sets=sets)
      dim<-x[[2]]
      parn<-x[[3]]
      xloaded<-x[[1]]
      names(xloaded)[dim+1]<-"mean"
      if(dim==3){
        names(xloaded)[1] <- "i"
        names(xloaded)[3] <- "j"
        names(xloaded)[2] <- "k" 
      }
      
    }else if(fileext=="csv"){
      print("Loading csv file...")
      x <- loadcsv(xfulln, varbles)
      dim<-x[[2]]
      parn<-x[[3]]
      xloaded<-x[[1]]
      
        
    }else{
        stop(paste0("No loading procedure for files with extension ",fileext," has been developed yet..."))
    }
    
    
    #functs=c("max", "min", "mean", "sd", "median")
    
    if(spatunit=="hsu"){
      xhsu<-xloaded
      names(xhsu)[1] <- "hsu"
      xhsu$hsu <- gsub("U", "", xhsu$hsu)

    }else{  
      xhsu<-agguscie2hsu(xloaded,xfrom=spatunit,xto="hsu",xvbles = "mean",dim=dim,functs=functs, varbles = varbles)
      
    }

    
    #xavi: don't understand next lines (from 193 to 203)
    unittoagg<-"hsu"   #xavi: if you define it here, always will be hsu (if...else doesn't make sense)
    #unittoagg <- spatunit  #xavi: maybe?
    if(unittoagg=="hsu"){
        xstart <- preparedata(xhsu)
        xstart<-xstart[!is.na(xstart$mean)]
        #xstatistics <- computestatistics(xstart)
    }else if(unittoagg=="uscie"){
        xstart <- preparedata(xloaded)
        xstart<-xstart[!is.na(xstart$mean)]
        #xstatistics <- computestatistics(xstart)
    }
    

    
    
    #functs=c("max", "min", "weighted.mean", "sd", "median")
    
    #If mean is included in arguments (functs), change it for weighted.mean
    functs <- replace(functs, functs=="mean", "weighted.mean(., area)")
    
    xnuts3<-agguscie2hsu(xstart,xfrom="hsu",xto="nuts3","mean",dim=dim,functs=functs)
    xnuts2<-agguscie2hsu(xstart,xfrom="hsu",xto="CAPRI_NUTSII","mean",dim=dim,functs=functs)
    xnuts0<-agguscie2hsu(xstart,xfrom="hsu",xto="CAPRI_NUTS0","mean",dim=dim,functs=functs)
    setnames(xhsu,old="hsu",new="i")

    if(spatunit=="s_uscierc"){
      xall<-rbind(xnuts0,xnuts2,xnuts3,xhsu)
      
    }else{  
      xall<-rbind(xnuts0,xnuts2,xnuts3)
    }
    
    setnames(xall,old="i",new="spatial_unit")
    if(dim==2) setnames(xall,old="j",new="variables") 
    if(dim==3) setnames(xall,old=c("j", "k"), new=c("spatial_unit2", "variables"))
    
    export2gdx(xall, dim=dim, parn=parn)  # to export to gdx
    
    save(xnuts0,xnuts2,xnuts3,xhsu, file=paste0(parn,".rdata"))
    return(list(xall,xnuts0,xnuts2,xnuts3,xhsu,xstart))
} #end of processdata


#agg2admins <- function(xprepared, 
#                       data2ag="hsu", 
#                       functs=c("max", "min", "weighted.mean", "sd", "median"), 
#                       #functs=c("max", "min", "mean", "sd", "median"), 
#                       vbles, filenm4gdx, ...){
#    
#    start <- Sys.time()
#    
#    # agg<-function(x2agg,aggto,...){
#    #     #print(x2agg[,eval(q)])
#    #     by_hsu <- x2agg %>% group_by(eval(q))
#    #     print(by_hsu)
#    #     xaggd <- by_hsu %>% summarise_at(vbles, functs, ...)
#    #     return(xaggd)
#    # }
#    print("Preparing to compute statistics...")
#    # If original unit is uscie then first aggregate to hsu
#    if(any(grepl("uscie", names(xprepared))==TRUE)){
#        print("Computing statistics per HSU...")
#        by_hsu <- xprepared %>% group_by(hsu)
#        xprepared.hsu <- by_hsu %>% summarise_at(vbles, functs, ...)
#        colnames(xagg)[1] <- "s_spatunit"  #changing 1st column name
#        
#        #HSU values will be the start the overall result
#        xhsu<-as.data.table(xprepared.hsu)
#        
#        
#        if(tolower(data2ag) == "uscie"){
#            xagg<-xprepared
#        }else if(tolower(data2ag) == "hsu"){
#            xagg<-xprepared[,,drop=s_uscierc]
#        }
#    }else if(any(grepl("hsu", names(xprepared))==TRUE)){
#        xprepared.hsu<-xprepared
#        xagg<-xprepared
#    }else{
#        stop("Aggregation should start from either uscie or hsu!")
#    }
#    
#    for (i in 1:length(functs)){   # this loop is to add the argument "area", nedded to compute weighted mean
#        if (functs[i] == "weighted.mean"){      # this is to tell the function how to compute weighted mean
#            functs[i] <- "weighted.mean(., area)"   
#            if(!"area" %in% colnames(xprepared)) stop("please, provide a column called 'area' with areas", call. = FALSE)   # If a column called "area" with areas is not provided, an error message is issued and the function stops
#        } else {
#            functs[i] <- functs[i]
#        }
#    }
#    
#    print("Computing statistics per NUTS III...")
#    by_nuts3 <- xagg %>% group_by(nuts3)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
#    xprepared.nuts3 <- by_nuts3 %>% summarise_at(vbles, functs, ...)
#    colnames(xprepared.nuts3)[1] <- "s_spatunit"  #changing 1st column name
#    
#    print("Computing statistics per CAPRI NUTS II...")
#    by_CAPRI_NUTSII <- xagg %>% group_by(CAPRI_NUTSII)           #grouping the DataTable before to be passed to the summarise_each(), to be faster
#    xprepared.caprinuts <- by_CAPRI_NUTSII %>% summarise_at(vbles, functs, ...)
#    colnames(xprepared.caprinuts)[1] <- "s_spatunit"  #changing 1st column name
#    
#    print("Computing statistics per CAPRI NUTS 0...")
#    by_CAPRI_NUTS0 <- xagg %>% group_by(CAPRI_NUTS0)           #grouping the DataTable before to be passed to the summarise_at(), to be faster
#    xprepared.country <- by_CAPRI_NUTS0 %>% summarise_at(vbles, functs, ...)
#    colnames(xprepared.country)[1] <- "s_spatunit"  #changing 1st column name
#    
#    ## Combining by rows ##
#    print("Creating a table with the results...")
#    xprepared.hsu.nuts2 <- rbind(xprepared.hsu, xprepared.nuts3, xprepared.caprinuts, xprepared.country)
#    if(length(functs)==1 && length(vbles)>1){
#        colnames(xprepared.hsu.nuts2)[-1] <- paste(colnames(xprepared.hsu.nuts2)[-1], functs, sep = "_")
#    }else if(length(functs)>1 && length(vbles)==1){
#        colnames(xprepared.hsu.nuts2)[-1] <- paste(vbles, colnames(xprepared.hsu.nuts2)[-1], sep = "_")
#    }else if(length(functs)==1 && length(vbles)==1){
#        colnames(xprepared.hsu.nuts2)[-1] <- paste(colnames(xprepared.hsu.nuts2)[-1], functs, sep = "_")
#    }
#    return(xprepared.hsu.nuts2)
#}




## Exporting to a gdx file with ##

#export2gdx<-function(x2gdx, dim=dim, xfulln=xfulln, parn=parn){
export2gdx<-function(x2gdx, dim=dim, parn=parn){
  
  print("Exporting to a gdx file...")
  x2gdx_noNA <- x2gdx[complete.cases(x2gdx)]  # to remove NA's
  x2gdx_noNA <- as.data.frame(x2gdx_noNA)
  x2gdx_noNA <- droplevels(x2gdx_noNA)

  #nm <- strsplit(xfulln,"*\\/|\\.")[[1]]   # to extract the name 
  #nm <- tail(nm, 2)[1]
  nm <- tolower(parn)

  if(dim==1){
    
    symDim <- 2
    attr(x2gdx_noNA,"symName") <- nm
    attr(x2gdx_noNA, "ts") <- paste0("statistics calculated for ", nm)    #explanatory text for the symName
    myText <- c("spatial unit", "statistics")     # explanatory text for the extracted index sets
    lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    wgdx.lst(paste0(nm, "_stats.gdx"), lst)
    
  }else if(dim==2){
    
    symDim <- 3
    attr(x2gdx_noNA,"symName") <- nm
    attr(x2gdx_noNA, "ts") <- paste0("statistics calculated for ", nm)    #explanatory text for the symName
    myText <- c("spatial unit", "variables", "statistics")     # explanatory text for the extracted index sets
    lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    wgdx.lst(paste0(nm, "_stats.gdx"), lst)

  }else if(dim==3){
    
    symDim <- 4
    attr(x2gdx_noNA,"symName") <- nm
    attr(x2gdx_noNA, "ts") <- paste0("statistics calculated for ", nm)    #explanatory text for the symName
    myText <- c("spatial unit", "variables", "variables 2", "statistics")     # explanatory text for the extracted index sets
    lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,3,2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    wgdx.lst(paste0(nm, "_stats.gdx"), lst)

  }else{
    stop("Function export2gdx not yet set-up for more than 3 dimensions")
  }
} #end of export2gdx




# xavi20160826: these two functions are not yet included 

## Function 2.1: Multifunction used in func2 to compute several HSU - USCIE statistics 

#multi.fun.2.1 <- function(y) {
    #c(min = min(y), max = max(y), mean = mean(y), sd = sd(y), median = median(y))
    
#} #End of Function 2.1

## Function 2: Computing HSU - USCIE statistics 

#computestatistics <- function(x){
    
    ## computing HSU - USCIE statistics ##
    
#    start <- Sys.time()
#    print("Computing HSU - USCIE statistics...")
#    
#    count.hsu.uscie <-count_(x, vars="hsu")
#    stats.hsu.uscie <- round(multi.fun.2.1(count.hsu.uscie$n), 2)
#    
    #save(stats.hsu.uscie, file ="stats.hsu.uscie.rdata")  # If Func2 is included to Func1, this line should be activated in order to save the result in disk
    #load(file = "stats.hsu.uscie.rdata")   # If Func2 is included to Func1, this line should be activated in order to load the result in the environment
    
    
#    print("End of Function 2")
#    end <- Sys.time() - start
#    print(end)
    
#    return(stats.hsu.uscie)
    
#}  # End of Function 2







dataprep<-paste0(usciedatapath,"uscie_hsu2_nuts_marsgrid.rdata")

if(! file.exists(dataprep)){
    ### Input 1: USCIE-HSU2 table  ####
    # It relates USCIE codes with HSU2 codes
    #coming from USCIE_HSU2.CSV. Reading the file directly from the CSV with fread(). 
    uscie_hsu <- fread(paste0(usciedatapath,"USCIE_HSU2.csv"), header=TRUE) # fread() much faster than read.csv - output is a data table
    names(uscie_hsu) <- c("s_uscierc","hsu")
    setkey(uscie_hsu, "s_uscierc") # to set a key column of the DataTable
    
    ### Input 2: HSU2-NUTS-CAPRI  ####
    # It relates HSU2 codes with NUTS2(Eurostat codes), CAPRI_NUTSII, CAPRI_NUTS0 (countries). It also include the area of each HSU
    hsu2_nuts3 <- fread(paste0(usciedatapath,"HSU2_DEFPARAM.csv"), header=TRUE,select=c("HSU2_IDRUN","HSU2_TOTAR","ADMIN_EEZ","SMU")) 
    #hsu2_nuts3 <- as.data.table(hsu2[ , c(2,3,11,22), with=FALSE])  #to select the columns of the DT
    setnames(hsu2_nuts3,old=c("HSU2_IDRUN","HSU2_TOTAR","ADMIN_EEZ","SMU"),new=c("hsu","area","nuts3","SMU")) #Same header as for the uscie table
    setkey(hsu2_nuts3, "hsu") # to set a key column of the DataTable
    
    ### Input 3: CAPRI NUTS ####
    #al20160823 - original file is *csv
    #nuts_capri <- fread(paste0(usciedatapath,"HSU2_NUTS_TO_CAPRI_NUTS_CODES.txt"), header=TRUE) 
    #setkey(nuts_capri, "nuts3") # to set a key column of the DataTable
    nuts_capri <- fread(paste0(usciedatapath,"HSU2_NUTS_TO_CAPRI_NUTS_CODES.csv"), header=TRUE,drop="CAPRI_MS") 
    setnames(nuts_capri,old = c("ADMIN_EEZ","NURGCDL2"),new = c("nuts3","nuts2")) #keep CAPRI_NUTSII,CAPRI_NUTS0
    setkey(nuts_capri, "nuts3") # to set a key column of the DataTable
    hsu2_nuts <- merge(hsu2_nuts3, nuts_capri, by.x = "nuts3", by.y = "nuts3", all.x = TRUE)
    rm(hsu2_nuts3,nuts_capri) #Not needed any more
    
    hsu2_nuts$hsu <- factor(hsu2_nuts$hsu) # to transform the column to factor
    setkey(hsu2_nuts, "hsu") # to set a key column
    
    ### Input 4: Fraction of MARS-GRID per HSU2 ####
    #load csv file linking uscie and grid numbers, dataset coming from capri/dat/capdis/uscie
    uscie_marsgrid <- fread(paste0(usciedatapath,"../hsu2_database_togdx_201605_nocita/","USCIE_PARAM.csv"), header=TRUE,select=c(1, 3)) 
    setnames(uscie_marsgrid,old=c("USCIE_RC","GRIDNO"),new=c("s_uscierc","marsgrid"))
    # Merge uscie - marsgrid - with HSU2; eliminate empty HSU2 and MARS-grids
    setkey(uscie_hsu,"s_uscierc")
    setkey(uscie_marsgrid,"s_uscierc")
    marsgrid_hsu <- uscie_hsu[uscie_marsgrid][!is.na(hsu)&marsgrid!=0]
    marsgrid_hsu<-marsgrid_hsu[,.(area=.N),by=.(hsu,marsgrid)]
    marsgrid_hsu<-marsgrid_hsu[,`:=`(gridarea=sum(area)),by=hsu]
    # Fraction of HSU2 that belongs to a certain MARSGRID
    marsgrid_hsu<-marsgrid_hsu[,`:=`(fraction=area/gridarea)]
    
    ### Input 5: Fraction of EMEPGRID / EMEPGRID - EMEP_LC_CLASS per HSU2 ####
    USCIE_EMEP_HSU2_LC <- fread(paste0(usciedatapath, "USCIE_EMEP_HSU2_LC.csv"), header=TRUE)
    #setnames(USCIE_EMEP_HSU2_LC, old=c("i50_j50", "HSU2_IDRUN"), new=c("emepgrid","hsu"))
    setnames(USCIE_EMEP_HSU2_LC, old=c("HSU2_IDRUN"), new=c("hsu"))
    setkey(USCIE_EMEP_HSU2_LC, "hsu")
    # Fraction of EMEPGRID per HSU2
    hsu2_emepgrid_area <- USCIE_EMEP_HSU2_LC[,.(hsu_emep_area = .N), by=.(hsu, i50_j50)]
    hsu2_emepgrid_area <- hsu2_emepgrid_area[,`:=`(emep_area = sum(hsu_emep_area)), by=hsu]
    hsu2_emepgrid_area <- hsu2_emepgrid_area[,`:=`(frac_emep_hsu = hsu_emep_area/emep_area)]
    # Fraction of EMEPGRID - EMEP_LC_CLASS per HSU2
    hsu2_emepgrid_lc_area <- USCIE_EMEP_HSU2_LC[,.(hsu_emep_lc_area = .N), by=.(hsu, i50_j50, EMEP_LC_CLASS)]
    hsu2_emepgrid_lc_area <- hsu2_emepgrid_lc_area[,`:=`(emep_lc_area = sum(hsu_emep_lc_area)), by=hsu]
    hsu2_emepgrid_lc_area <- hsu2_emepgrid_lc_area[,`:=`(frac_emep_lu_hsu = hsu_emep_lc_area/emep_lc_area)]
    
    
    
    #### 6. Export to a gdx file ####
    
    hsu2export <- hsu2_nuts[complete.cases(hsu2_nuts), ]  # to remove NA's
    hsu2export$hsu<-paste0("U",hsu2export$hsu)
    
    symDim <- 7
    attr(hsu2export,"symName") <- "hsu_nuts_capri"
    attr(hsu2export, "ts") <- "relates HSU2 codes (and areas) with NUTS2, CAPRI_NUTSII, CAPRI_NUTS0"   #explanatory text for the symName
    myText <- c("country code","nuts2 code of capri","nuts2 code","soil mapping unit","nuts3 code","hsu2 code","area of hsu2")     # explanatory text for the extracted index sets
    lst <- wgdx.reshape(hsu2export, symDim,tName = "area", setsToo=FALSE,order=c(7,6,5,4,2,1,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    
    # Add sets to include
    shsu<-hsu2export$hsu
    hsu2set<-list(name='s_hsu',ts='List of HSU codes',uels=list(shsu),type='set',dim=1,form='full')
    ssmu<-unique(hsu2export$SMU)
    smuset<-list(name='s_smu',ts='List of soil mapping units',uels=list(ssmu),type='set',dim=1,form='full')
    
    nutsexport<-unique(hsu2export[,.(nuts2,CAPRI_NUTSII,CAPRI_NUTS0,nHSU=.N,areanuts3_km2=sum(area/1000000)),by=nuts3])
    snuts3<-nutsexport$nuts3
    nuts3set<-list(name='s_nuts3',ts='List of ADMIN_EEZ codes, level NUTS3',uels=list(snuts3),type='set',dim=1,form='full')
    snuts2<-nutsexport$nuts2
    nuts2set<-list(name='s_nuts2',ts='List of ADMIN_EEZ codes, level nuts2',uels=list(snuts2),type='set',dim=1,form='full')
    
    scaprinuts2<-unique(nutsexport$CAPRI_NUTSII)
    scaprinuts2set<-list(name='s_srnuts2',ts='List of CAPRI NUTS2 available for HSU',uels=list(scaprinuts2),type='set',dim=1,form='full')
    scapricountries<-unique(nutsexport$CAPRI_NUTS0)
    scapricountriesset<-list(name='s_countries',ts='List of CAPRI countries available for HSU',uels=list(scapricountries),type='set',dim=1,form='full')
    
    mnuts3nuts2<-unique(nutsexport[,.(nuts3,nHSU,areanuts3_km2),by=nuts2])
    mnuts3nuts2$nHSU<-as.numeric(mnuts3nuts2$nHSU)
    attr(mnuts3nuts2,"symName") <- "p_nuts3_nuts2"
    attr(mnuts3nuts2, "ts") <- "Map between ADMIN_EEA codes between level NUTS3 and level NUTS2"
    mn3 <- wgdx.reshape(mnuts3nuts2, symDim=3,tName = "pnuts3", setsToo=FALSE,order=NULL)
    
    mnuts3srnuts2<-unique(nutsexport[CAPRI_NUTSII!="",.(nuts3,CAPRI_NUTS0,nHSU,areanuts3_km2),by=CAPRI_NUTSII])
    mnuts3srnuts2$nHSU<-as.numeric(mnuts3srnuts2$nHSU)
    attr(mnuts3srnuts2,"symName") <- "p_nuts3_srnuts2"
    attr(mnuts3srnuts2, "ts") <- "Map between ADMIN_EEA codes between level NUTS3 and CAPRI NUTS2"
    mn2 <- wgdx.reshape(mnuts3srnuts2, symDim=4,tName = "pnuts3", setsToo=FALSE,order=NULL)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    
    msrnuts2nuts0<-unique(mnuts3srnuts2[CAPRI_NUTSII!="",.(CAPRI_NUTS0,nHSU=sum(nHSU),nnuts3=.N,areanuts2_km2=sum(areanuts3_km2)),by=CAPRI_NUTSII])
    msrnuts2nuts0$nnuts3<-as.numeric(msrnuts2nuts0$nnuts3)
    attr(msrnuts2nuts0,"symName") <- "p_srnuts2_nuts0"
    attr(msrnuts2nuts0, "ts") <- "Map between ADMIN_EEA codes between level NUTS3 and CAPRI NUTS2"
    mn0 <- wgdx.reshape(msrnuts2nuts0, symDim=3,tName = "pnuts3", setsToo=FALSE,order=NULL)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    
    mnuts0<-unique(msrnuts2nuts0[CAPRI_NUTS0!="",.(nHSU=sum(nHSU),nnuts3=sum(nnuts3),nnuts2=.N,areanuts0_km2=sum(areanuts2_km2)),by=CAPRI_NUTS0])
    mnuts0$nnuts2<-as.numeric(mnuts0$nnuts2)
    attr(mnuts0,"symName") <- "p_nuts0"
    attr(mnuts0, "ts") <- "Map between ADMIN_EEA codes between level NUTS3 and CAPRI NUTS2"
    mn <- wgdx.reshape(mnuts0, symDim=2,tName = "pnuts3", setsToo=FALSE,order=NULL)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    
    wgdx.lst("hsu2_nuts1", lst,hsu2set,nuts3set,nuts2set,smuset,scaprinuts2set,scapricountriesset,mn3,mn2,mn0,mn)
    
    ### 6., Save data in rdata format ####
    save(hsu2_nuts,uscie_hsu,marsgrid_hsu,mnuts3nuts2,mnuts3srnuts2,msrnuts2nuts0,mnuts0,file=dataprep)
    rm(hsu2export,uscie_marsgrid)
    
}else{
    load(file = dataprep)
}

