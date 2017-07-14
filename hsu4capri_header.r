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
usciedatapath<-"\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\uscie\\"
usciedatapath<-paste0(usciedatapath,"hsu2_database_update_2016_02orig/")

#Set working directory and load of general (used to update all) datasets.
# These databases have been updated by Renate Koeble and delivered in the folder capri/hsu2_database_update_2016_02
if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
    gamspath<-"C:/GAMS/win64/24.4"
    workpath<-"C:/adrian/tools/rprojects/gisdata4caprihsu/"
    capridat<-"C:/adrian/models/capri/trunk20160810/dat/capdis/hsu2/"
}else if(Sys.info()[4]=="D01RI1600881"){ #checks machine name
    gamspath<-"x:/GAMS/win64/24.4"
    workpath<-"x:/adrian/tools/rprojects/gisdata4caprihsu/"
    capridat<-"x:/adrian/models/capri/trunk20160810/dat/capdis/hsu2/"
    capridat<-"\\\\ies-ud01.jrc.it\\D5_agrienv\\Data\\uscie\\"
    capridat<-paste0(capridat,"gisdata4caprihsu_inputdata/")
}else if(Sys.info()[4]=="MacBook-Pro-de-Xavier.local"){ #checks machine name
    workpath<-"/Users/xavi/Documents/JRC_MARS/hsu2_statistics_xavi2/"
    gamspath<-"/Applications/GAMS24.6/sysdir"
    usciedatapath<-workpath
    capridat<-workpath
}else{
    workpath<-"X:/MARS_disaggregation/hsu2_statistics_xavi2/"
    usciedatapath<-workpath
    capridat<-workpath
    gamspath<-"C:/apps/GAMS/win64/24.7"
}

setwd(workpath)


#link with gams directory
igdx(gamspath)


linkxto2xfrom<-function(x2agg,xfrom,xto){

    if(xfrom=="s_uscierc"){
        print(paste0("Linking ", xfrom, " data with ", xto))
        x2agg$s_uscierc<-as.integer(as.character(x2agg$s_uscierc))
        setkey(x2agg,s_uscierc)
        xhsu<-xhsu<-x2agg[uscie_hsu]   # merge HSU (left join)
        x2agg<-xhsu[complete.cases(xhsu)]
    }else if(xfrom=="marsgrid"){
        print(paste0("Linking ", xfrom, " data with ", xto))
        setkey(marsgrid_hsu, marsgrid)
        xhsu<-x2agg[marsgrid_hsu, allow.cartesian=TRUE]
        if(ndim==3){
            x2agg <- xhsu[,.(value =  weighted.mean(mean, fraction)), by=.(hsu, j, k)]  #xavi: this step takes a bit long
        }else{  #xavi: for now, always ndim is 2 or 3
            x2agg <- xhsu[,.(value =  weighted.mean(mean, fraction)), by=.(hsu, j)]
        }
        setnames(x2agg, old=c("value"), new=c("mean"))
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
        setnames(x2agg, old=c("value"), new=c("mean"))
        setkey(x2agg, xto)
    }else if (xfrom=="hsu"){
        #Nothing to do
    }else{
        stop(paste0("Stop function linkxto2xfrom Unit to aggregate from: ",xfrom," is not implemented."))
    }
    return(x2agg)
}

agguscie2hsu<-function(x2agg,xfrom,xto,xvbles,ndim,functs){
    
    ## Function agguscie2hasu: Computing statistics per HSU, NUTS 3-2-0.
    
    # This function aggregates data from a unit at higher resolutions (e.g. uscie, hsu, marsgrid,...) to a unit of coarser resolution (e.g. hsu, nutsX)
    # It is called various times from the function processdata
    
    # If weighted mean wants to be computed, in the data table must be one column called "area". If it is not provided, an error message is issued and the function stops
    # The 
    
    # x2agg: paramter containing the data to be aggregated (in column xvbles)
    # xfrom: spatial unit at higher spatial resolution (i.e. uscie, hsu)
    # xto: spatial unit at coarser spatial resolution(i.e. hsu, nuts3, nuts2, nuts0)
    # xvbles: variable-name (column name) of parameter x2agg for variable to be aggregated
    #         xvbles may also be a vector, e.g. xvbles = c("f_2000", "f_2006")
    # ndim: number of dimensions of parameter x2agg. So far, only aggregation of parameters with 2 or 3 dimensions has been implemented
    # functs: statistical parameter that should be calculated. Is set in function processdata, currently as functs=c("max", "min", "mean", "sd", "median")
    
    #For weighted mean return the weighting column (area) otherwise leave functs unchanged
    for (i in 1:length(functs)){   # this loop is to add the argument "area", nedded to compute weighted mean
        if (functs[i] == "weighted.mean(., area)"){      # this is to tell the function how to compute weighted mean
            if(!"area" %in% colnames(x2agg)) stop("please, provide a column called 'area' with areas", call. = FALSE)   # If a column called "area" with areas is not provided, an error message is issued and the function stops
        }
    }
    xtemp<-copy(x2agg)
    #print(colnames(xtemp))
    setnames(xtemp,old<-xto,new<-"xto")
    #print(colnames(xtemp))
    if(xfrom=="s_uscierc" | xfrom=="hsu"){
        print(paste0("Computing statistics from ", xfrom, " to ", xto, "!"))
        print(paste0("Computing ", paste0(functs, collapse = ", ")))
        
        # Weighted mean cannot be calculated as the other statistical moments with summarise_at
        functs2<-functs[!functs %in% c("weighted.mean(., area)")]
        
        if(ndim==1){
            if(length(functs2)>0)xres<-as.data.table(summarise_at(group_by(xtemp,xto),xvbles, functs2, na.rm=TRUE))
            if (any(functs=="weighted.mean(., area)")){
                for(i in 1:length(xvbles)){
                    #Rename column to 'value' so that the function works but name it back later so that the next can continue
                    colnames(xtemp)[which(colnames(xtemp)==xvbles[i])]<-"value"
                    xres2 <- ddply(xtemp, c("xto"), function(X) data.frame(value = weighted.mean(X$value, X$area,na.rm=TRUE)))
                    xres <- cbind(xres, xres2$value)
                    setnames(xres, "V2",xvbles[i])
                    colnames(xres)[which(colnames(xtemp)=="value")]<-xvbles[i]
                }
            }
        }else if(ndim==2){
            if(length(functs2)>0)xres<-as.data.table(summarise_at(group_by(xtemp,j,xto),xvbles, functs2, na.rm=TRUE))
            if (any(functs=="weighted.mean(., area)")){
                for(i in 1:length(xvbles)){
                    colnames(xtemp)[which(colnames(xtemp)==xvbles[i])]<-"value"
                    xres2 <- ddply(xtemp, c("j", "xto"), function(X) data.frame(value = weighted.mean(X$value, X$area)))
                    xres <- cbind(xres,xres2$value)
                    setnames(xres, "V2","value")
                    colnames(xres)[which(colnames(xtemp)=="value")]<-xvbles[i]
                }
            }
        }else if(ndim==3){
            if(length(functs2)>0)xres<-as.data.table(summarise_at(group_by(xtemp,j,k,xto),xvbles, functs2, na.rm=TRUE))
            if (any(functs=="weighted.mean(., area)")){
                xres2 <- ddply(xtemp, c("j", "k", "xto"), function(X) data.frame(value = weighted.mean(X$value, X$area)))
                xres <- cbind(xres, xres2$value)
                setnames(xres, "V2","value")
            }
        }else{
            stop("Function aggs_uscierc2hsu not yet set-up for more than 3 dimensions")
        }
        
    }else{
        xres <- xtemp
    }

    for (jjj in (ndim+1):ncol(xres)) set(xres,which(is.na(xres[[jjj]])),jjj,0)
    xres<-xres[xto!=""]
    setkey(xres,xto)
    setnames(xres,new=xto,old="xto")
    colnames(xres)<-gsub("mean","value",colnames(xres))
    colnames(xres)<-gsub("_value","",colnames(xres))
    if(ndim>1){
        xresn<-c(xto,letters[10:(10+ndim-2)],xvbles)
    }else{
        xresn<-c(xto,xvbles)
    }
    View(xres)
    print(colnames(xres))
    print(xresn)
    xresn<-c(xresn,colnames(xres)[!colnames(xres)%in%xresn])
    xres<-xres[,xresn,with=FALSE]
    return(xres)  
    
} #end of agguscie2hsu

preparedata <- function(xstart){
    
    print("Adding NUTS info...")
    if(any(grepl("hsu", names(xstart))==TRUE)){  #to add hsu areas and NUTS data
        
        xstart$hsu <- as.numeric(as.character(xstart$hsu))
        setkey(xstart, hsu) # to set a key column
        hsu2_nuts$hsu <- as.numeric(as.character(hsu2_nuts$hsu))
        setkey(hsu2_nuts, hsu) # to set a key column
        x3<-xstart[hsu2_nuts]
        x3<-x3[!is.na(hsu)]
        setkey(x3, hsu) # to set a key column
        
    }else if(any(grepl("mu_global", names(xstart))==TRUE)){
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
            ndim<-as.numeric(dims)
        }
    }else{
        ndim<-as.numeric(dims[pars$V1==parname])
        
    }
    #
    
    if(ndim<4){
        #rgdx returns i,j,... for each ndim and writes the values into the column 'value'
        gdxl<-c("j","k","l","m") #xavi: for what is this?
        x <- as.data.table(rgdx.param(xfulln,parname))
        
    }else if(ndim>3 && is.null(sets)){
        
        x <- as.data.table(rgdx.param(xfulln,parname))
        print(head(x))
        print(paste0("num of dims = ", ndim))
        print(paste0("parameter name = ", parname))
        stop("More than 3 dimensions, please choose which columns to be included (e.g. sets=c('i1'...))")
        
    }else if(ndim>3 && !is.null(sets)){
        
        x <- as.data.table(rgdx.param(xfulln,parname))
        #x <- x[, .(sets)]
        x <- subset(x, select=sets)
        ndim <- length(sets) - 1
    }
    
    return(list(x,ndim,parname))
    
} #End of loadgdxfile

loadcsv<-function(xfulln, varbles){
    
    if(any(grepl("WD", varbles)==TRUE)){   #This is for Wet Deporition
        
        dep<-fread(xfulln, header=TRUE, select=c("i50_j50", varbles))
        setkey(dep,i50_j50) # to set a key column of the DataTable
        
    }else if(any(grepl("DD", varbles)==TRUE)){  #This is for Dry deposition
        dep<-fread(xfulln, header=TRUE, select=c("i50_j50","EMEP_LC_CLASS", varbles))
        setkey(dep,i50_j50) # to set a key column of the DataTable
    }
    
    ndim <- 2   # for now, this is hardcoded
    
    nm <- strsplit(xfulln,"\\.")[[1]]
    nm <- strsplit(nm[1],"_")[[1]]
    nm <- tail(nm, 2)[1:2]
    parname <- paste(nm, collapse = "_")
    
    return(list(dep, ndim, parname))
    
}# End of loadcsv

getlucas<-function(xfulln,xvbles=NULL,newn="",spatunit="s_uscierc",parn=NULL,functs=c("max", "min", "mean", "sd", "median"), sets=NULL){
    
    getonelucas<-function(yr){
        if(yr==2001) fl<-"LUCAS01EU15_USCIERC.csv"
        if(yr==2003) fl<-"LUCAS03EU15_USCIERC.csv"
        if(yr==2006) fl<-"LUCAS06EU11_USCIERC.csv"
        if(yr==2009) fl<-"LUCAS09EU23_USCIERC.csv"
        if(yr==2012) fl<-"LUCAS12EU27_USCIERC.csv"
        xfulln = paste0(usciedatapath,"../lucas/",fl)
        lucas <- fread(xfulln)
        lucas[,Year:=yr]
        lucas<-lucas[,xvbles,with=FALSE]
        return(lucas)
    }
    fileext<-strsplit(xfulln,"\\.")[[1]]
    fileext<-fileext[length(fileext)]
    origvar<-"USCIE_RC"

    #lucas01<-getonelucas(2001)
    #lucas03<-getonelucas(2003)
    lucas06<-getonelucas(2006)
    lucas09<-getonelucas(2009)
    lucas12<-getonelucas(2012)

    lucas<-rbind(lucas06,lucas09,lucas12)
 
    
    lucas_capri <- fread(paste0(usciedatapath,"../lucas/","lucas_capri.csv"))
    lucas_capri <- unique(lucas_capri)
    lucas <- merge(lucas, lucas_capri, by.x = "LC1", by.y = "LUCAS_LC1", all = TRUE)
    lucas$CAPRI_names[grepl("^C|D",lucas$LC1)]<-"FORE"
    lucas$CAPRI_names[grepl("^E",lucas$LC1)]<-"GRAS"
    lucas$CAPRI_names[grepl("^G",lucas$LC1)]<-"OTHE"
    setnames(lucas, "CAPRI_names", "CAPRI")

    
    xloaded<-lucas
    xdimnam<-c("spatial_unit","Year","POINT_ID")
    setnames(xloaded,old=origvar,new=spatunit)
    xvbles<-setdiff(colnames(xloaded),xdimnam)
    #do this now only for lucas
    xlinked<-linkxto2xfrom(x2agg=xloaded,xfrom=spatunit,xto="hsu")
    xlinked$hsu<-paste0("U",xlinked$hsu)
    setnames(xlinked,"hsu","spatial_unit")
    glucas<-melt(xlinked,xdimnam,xvbles,"variables","value")
    save(glucas,file=paste0(parn,".rdata"))
    export2gdx(glucas, ndim=4, parn=parn,statistics = 0)  # to export to gdx
    
    return(lucas)    
}

processdata<-function(xfulln,xvbles=NULL,newn="",spatunit="s_uscierc",parn=NULL,functs=c("max", "min", "mean", "sd", "median"), sets=NULL){
    #x: data table that contains one row with the original unit and further rows with variables of the name 'newn'
    #xn: part of the file names to be generated
    #oldn: variable names as in original data
    #newn: variable names as required in result files
    
    fileext<-strsplit(xfulln,"\\.")[[1]]
    fileext<-fileext[length(fileext)]
    xfields<-NULL
    if(fileext=="gdx"){
        # Load gdx-file (no other load-function has been develoed yet...)
        print("Loading gdx file...")
        x<-loadgdxfile(xfulln, parname=parn, sets=sets)
        ndim<-x[[2]]
        parn<-x[[3]]
        xloaded<-x[[1]]
        names(xloaded)[ndim+1]<-"value"
        xvbles<-"value"
        if(ndim==3){
            names(xloaded)[1] <- "i"
            names(xloaded)[3] <- "j"
            names(xloaded)[2] <- "k" 
        }
        setnames(xloaded,old<-c("i"),new<-c(spatunit))
        
        if(spatunit=="hsu"){
            xhsu<-xloaded
            names(xhsu)[1] <- "hsu"
            xhsu$hsu <- gsub("U", "", xhsu$hsu)
            
        }
    }else if(fileext=="csv"){
        print("Loading csv file...")
        if(spatunit=="s_uscierc") origvar<-"USCIE_RC"
        if(spatunit=="hsu") origvar<-"HSU2_IDRUN"
        xloaded<-fread(xfulln, header=TRUE)
        if(file.exists(gsub(".csv","_METADATA.csv",xfulln)))
            xmetadata<-fread(gsub(".csv","_METADATA.csv",xfulln), header=TRUE)
        setnames(xloaded,old=origvar,new=spatunit)
        if("MU_GLOBAL"%in%colnames(xloaded)) xloaded<-xloaded[,-"MU_GLOBAL",with=FALSE]
        if(!is.null(xvbles)){
            xloaded<-xloaded[,which(colnames(xloaded)%in%c("s_uscierc",xvbles)),with=FALSE]
        }else{
            xvbles<-setdiff(colnames(xloaded),spatunit)
        }
        
        if(!newn=="") {
                colnames(xloaded)[which(colnames(xloaded)==xvbles)]<-newn
                xvbles<-newn
        }
        ndim=1
        if(spatunit=="s_uscierc"){
            #xhsu<-melt(xloaded,spatunit,xvbles,"j","value")
            ndim<-2
            setkey(xloaded,"s_uscierc")
            xlinked<-xloaded[uscie_hsu]
        }
        if(xvbles%in%c("CLC")){
            # Required shares not average
            xlinked2<-xlinked[,.(count = .N),by=.(hsu,CLC)]
            tmp<-xlinked2[,.(tot=sum(count)),by=.(hsu)]
            #setkey(xlinked2,"hsu")
            #setkey(tmp,"hsu")
            xlinked2<-xlinked2[tmp,on="hsu"]
            xlinked2$count<-as.numeric(xlinked2$count)
            xlinked2$tot<-as.numeric(xlinked2$tot)
            xlinked2$fraction<-as.numeric(xlinked2$count/xlinked2$tot)
            #setnames(xlinked2,old="value",new=xvbles)
            xlinked<-xlinked2[,c("hsu",xvbles,"fraction"),with=FALSE]
            
            lc1map<- fread(paste0(usciedatapath,"USCIE_PARAM_LC1_ID_CLC.csv"))
            xloaded<- xlinked[lc1map,on="LC1_ID"] #merge(corine_uscie, LC1_ID_CLC, all.x = TRUE)
            xloaded<-xloaded[,c("hsu","CLC","fraction")]
            spatunit<-"hsu"
            ndim<-2
            functs<-c("mean","median","min","max")
            xvbles<-"fraction"
            setnames(xloaded,"CLC","j")
            
        }
        if(xvbles[1]=="dep") {
            x <- loadcsv(xfulln, xvbles)
            ndim<-x[[2]]
            parn<-x[[3]]
            xloaded<-x[[1]]
        }
    }else{
        stop(paste0("No loading procedure for files with extension ",fileext," has been developed yet..."))
    }
    
    save(ndim,parn,xloaded,spatunit,functs,file="dem.rdata")

    #functs=c("max", "min", "mean", "sd", "median")
    if(spatunit=="s_uscierc"){  
        
        #save(xloaded,spatunit,xfrom=spatunit,xvbles,ndim,functs,file="xloaded336.rdata")
        xlinked<-linkxto2xfrom(x2agg=xloaded,xfrom=spatunit,xto="hsu")
        xhsu<-agguscie2hsu(xlinked,xfrom=spatunit,xto="hsu",xvbles = xvbles,ndim=ndim,functs=functs)
        
    }else if(spatunit=="mu_global"){
        
        hsudef<-fread(paste0(usciedatapath,"HSU2_DEFPARAM.csv"))
        hsudef<-select(hsudef,HSU2_IDRUN,MU_GLOBAL,SMU)
        setnames(hsudef,old="HSU2_IDRUN",new="hsu")
        setnames(hsudef,old="MU_GLOBAL",new=spatunit)
        hsudef2<-hsudef[mu_global%in%xloaded$mu_global]
        setkey(hsudef,mu_global)
        xloaded$mu_global<-as.numeric(as.character(xloaded$mu_global))
        xloaded<-xloaded[complete.cases(xloaded)]
        setkey(xloaded,mu_global)
        xloaded2<-merge(hsudef,xloaded,by="mu_global",all.x=TRUE)
    }else if(spatunit=="hsu"){
        xhsu<-xloaded
    }
    
    
    # Assumption here that regardless the initial unit, at this point we have hsu and 
    # are continuing from this common 'milestone'
    unittoagg<-"hsu"   #xavi: if you define it here, always will be hsu (if...else doesn't make sense)
    xstart <- preparedata(xhsu)
    #xstart<-xstart[!is.na(xstart$value)]
    
    # Aggregation from HSU to administrative regions must be area-weighted
    # If already some specific 'mean' had been defined leave it as is
    if("mean"%in%functs) {functs <- replace(functs, functs=="mean", "weighted.mean(., area)")}
    save(xstart,functs,xvbles,ndim,file="xstart339.rdata")
    xnuts3<-agguscie2hsu(xstart,xfrom="hsu",xto="nuts3",xvbles=xvbles,ndim=ndim,functs=functs)
    xnuts2<-agguscie2hsu(xstart,xfrom="hsu",xto="CAPRI_NUTSII",xvbles=xvbles,ndim=ndim,functs=functs)
    xnuts0<-agguscie2hsu(xstart,xfrom="hsu",xto="CAPRI_NUTS0" ,xvbles=xvbles,ndim=ndim,functs=functs)
    
    #xnuts3<-xnuts3[!is.infinite(xnuts3$max)]
    #xnuts2<-xnuts2[!is.infinite(xnuts2$max)]
    #xnuts0<-xnuts0[!is.infinite(xnuts0$max)]
    save(xnuts0,xnuts2,xnuts3,xhsu,xfields, file=paste0(parn,".rdata"))
    
    # Prepare to export to gdx file for CAPRI
    colnames(xnuts3)[1]<-"spatial_unit"
    colnames(xnuts2)[1]<-"spatial_unit"
    colnames(xnuts0)[1]<-"spatial_unit"
    
    
    xhsu$hsu<-paste0("U",xhsu$hsu)
    colnames(xhsu)[1]<-"spatial_unit"
    if(spatunit=="s_uscierc"){
        xall<-rbind(xnuts0,xnuts2,xnuts3,xhsu)
    }else{  
        xall<-rbind(xnuts0,xnuts2,xnuts3)
        #xallstats<-melt(xall,"spatial_unit",)
    }
    
    if(ndim==2) setnames(xall,old="j",new="variables") 
    if(ndim==3) setnames(xall,old=c("j", "k"), new=c("spatial_unit2", "variables"))
    
    if(!parn%in%c("p_domstutop")){
        #statistical moments exist
        export2gdx(xall, ndim=ndim, parn=parn)  # to export to gdx
    }else{
        #for soil, too many parameters, only mean is saved
        gall<-melt(xall,"spatial_unit",xvbles,"variables","value")
        ghsu<-melt(xhsu,"spatial_unit",xvbles,"variables","value")
        gall<-rbind(gall,ghsu)
        export2gdx(xall, ndim=2, parn=parn,statistics = 0)  # to export to gdx
    }
    
    return(list(xall,xnuts0,xnuts2,xnuts3,xhsu,xstart))
} #end of processdata


## Exporting to a gdx file with ##

#export2gdx<-function(x2gdx, ndim=ndim, xfulln=xfulln, parn=parn){
export2gdx<-function(x2gdx, ndim=ndim, parn=parn,statistics=1){
    
    print("Exporting to a gdx file...")
    x2gdx_noNA <- x2gdx[complete.cases(x2gdx)]  # to remove NA's
    x2gdx_noNA <- as.data.frame(x2gdx_noNA)
    x2gdx_noNA <- droplevels(x2gdx_noNA)
    
    #nm <- strsplit(xfulln,"*\\/|\\.")[[1]]   # to extract the name 
    #nm <- tail(nm, 2)[1]
    nm <- tolower(parn)
    
    if(statistics==1){
    if(ndim==1){
        
        symDim <- 2
        attr(x2gdx_noNA,"symName") <- nm
        attr(x2gdx_noNA, "ts") <- paste0("statistics calculated for ", nm)    #explanatory text for the symName
        myText <- c("spatial unit", "statistics")     # explanatory text for the extracted index sets
        lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
        wgdx.lst(paste0(nm, "_stats.gdx"), lst)
        
    }else if(ndim==2){
        
        symDim <- 3
        attr(x2gdx_noNA,"symName") <- nm
        attr(x2gdx_noNA, "ts") <- paste0("statistics calculated for ", nm)    #explanatory text for the symName
        myText <- c("spatial unit", "variables", "statistics")     # explanatory text for the extracted index sets
        lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
        wgdx.lst(paste0(nm, "_stats.gdx"), lst)
        
    }else if(ndim==3){
        
        symDim <- 4
        attr(x2gdx_noNA,"symName") <- nm
        attr(x2gdx_noNA, "ts") <- paste0("statistics calculated for ", nm)    #explanatory text for the symName
        myText <- c("spatial unit", "variables", "variables 2", "statistics")     # explanatory text for the extracted index sets
        lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,3,2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
        wgdx.lst(paste0(nm, "_stats.gdx"), lst)
        
    }else{
        stop("Function export2gdx not yet set-up for more than 3 dimensions")
    }
    }else{
        if(ndim==2){
            
            symDim <- 3
            attr(x2gdx_noNA,"symName") <- nm
            attr(x2gdx_noNA, "ts") <- paste0("Mean values for ", nm)    #explanatory text for the symName
            myText <- c("spatial unit", "variables", "value")     # explanatory text for the extracted index sets
            lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
            wgdx.lst(paste0(nm, "_stats.gdx"), lst)

        }else
        if(ndim==4){
            
            symDim <- 5
            attr(x2gdx_noNA,"symName") <- nm
            attr(x2gdx_noNA, "ts") <- paste0("Mean values for ", nm)    #explanatory text for the symName
            myText <- c("spatial unit","year","pointid", "variables", "value")     # explanatory text for the extracted index sets
            lst <- wgdx.reshape(x2gdx_noNA, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1,4,3,2,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
            wgdx.lst(paste0(nm, "_stats.gdx"), lst)

        }
    }
} #end of export2gdx



dataprep<-paste0(usciedatapath,"uscie_hsu2_nuts_marsgrid.rdata")

if(! "uscie_hsu"%in%ls()){
    if(! file.exists(dataprep)){
        ### Input 1: USCIE-HSU2 table  ####
        # It relates USCIE codes with HSU2 codes
        #coming from USCIE_HSU2.CSV. Reading the file directly from the CSV with fread(). 
        uscie_hsu <- fread(paste0(usciedatapath,"USCIE_HSU2.csv"), header=TRUE) # fread() much faster than read.csv - output is a data table
        names(uscie_hsu) <- c("s_uscierc","hsu")
        setkey(uscie_hsu, "s_uscierc") # to set a key column of the DataTable
        
        ### Input 2: HSU2-NUTS-CAPRI  ####
        # It relates HSU2 codes with NUTS2(Eurostat codes), CAPRI_NUTSII, CAPRI_NUTS0 (countries). It also includes the area of each HSU
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
        hsu2set<-list(name='s_hsu',ts='List of HSU codes',uels=list(shsu),type='set',ndim=1,form='full')
        ssmu<-unique(hsu2export$SMU)
        smuset<-list(name='s_smu',ts='List of soil mapping units',uels=list(ssmu),type='set',ndim=1,form='full')
        
        nutsexport<-unique(hsu2export[,.(nuts2,CAPRI_NUTSII,CAPRI_NUTS0,nHSU=.N,areanuts3_km2=sum(area/1000000)),by=nuts3])
        snuts3<-nutsexport$nuts3
        nuts3set<-list(name='s_nuts3',ts='List of ADMIN_EEZ codes, level NUTS3',uels=list(snuts3),type='set',ndim=1,form='full')
        snuts2<-nutsexport$nuts2
        nuts2set<-list(name='s_nuts2',ts='List of ADMIN_EEZ codes, level nuts2',uels=list(snuts2),type='set',ndim=1,form='full')
        
        scaprinuts2<-unique(nutsexport$CAPRI_NUTSII)
        scaprinuts2set<-list(name='s_srnuts2',ts='List of CAPRI NUTS2 available for HSU',uels=list(scaprinuts2),type='set',ndim=1,form='full')
        scapricountries<-unique(nutsexport$CAPRI_NUTS0)
        scapricountriesset<-list(name='s_countries',ts='List of CAPRI countries available for HSU',uels=list(scapricountries),type='set',ndim=1,form='full')
        
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
        load(file = dataprep, verbose = TRUE)
    }
}

