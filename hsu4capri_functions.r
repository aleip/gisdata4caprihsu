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
        
        if(length(functs2)>0){
            if(ndim==1){
                xres<-as.data.table(summarise_at(group_by(xtemp,xto),xvbles, functs2, na.rm=TRUE))
                # if (any(functs=="weighted.mean(., area)")){
                #     for(i in 1:length(xvbles)){
                #         #Rename column to 'value' so that the function works but name it back later so that the next can continue
                #         colnames(xtemp)[which(colnames(xtemp)==xvbles[i])]<-"value"
                #         xres2 <- ddply(xtemp, c("xto"), function(X) data.frame(value = weighted.mean(X$value, X$area,na.rm=TRUE)))
                #         xres <- cbind(xres, xres2$value)
                #         setnames(xres, "V2",xvbles[i])
                #         colnames(xres)[which(colnames(xres)=="value")]<-xvbles[i]
                #     }
                # }
            }else if(ndim==2){
                xres<-as.data.table(summarise_at(group_by(xtemp,j,xto),xvbles, functs2, na.rm=TRUE))
                # if (any(functs=="weighted.mean(., area)")){
                #     for(i in 1:length(xvbles)){
                #         colnames(xtemp)[which(colnames(xtemp)==xvbles[i])]<-"value"
                #         xres2 <- ddply(xtemp, c("j", "xto"), function(X) data.frame(value = weighted.mean(X$value, X$area)))
                #         xres <- cbind(xres,xres2$value)
                #         setnames(xres, "V2","value")
                #         colnames(xres)[which(colnames(xres)=="value")]<-xvbles[i]
                #     }
                # }
            }else if(ndim==3){
                xres<-as.data.table(summarise_at(group_by(xtemp,j,k,xto),xvbles, functs2, na.rm=TRUE))
                # if (any(functs=="weighted.mean(., area)")){
                #     for(i in 1:length(xvbles)){
                #         colnames(xtemp)[which(colnames(xtemp)==xvbles[i])]<-"value"
                #         xres2 <- ddply(xtemp, c("j", "k", "xto"), function(X) data.frame(value = weighted.mean(X$value, X$area)))
                #         xres <- cbind(xres, xres2$value)
                #         setnames(xres, "V2","value")
                #         colnames(xres)[which(colnames(xres)=="value")]<-xvbles[i]
                #     }
                # }
            }else if(ndim==4){
                xres<-as.data.table(summarise_at(group_by(xtemp,j,k,l,xto),xvbles, functs2, na.rm=TRUE))
                
            }else{
                stop("Function aggs_uscierc2hsu not yet set-up for more than 3 dimensions")
            }
        }
        
        if (any(functs=="weighted.mean(., area)")){
            lets<-letters[10:(10+ndim-2)]
            for(i in 1:length(xvbles)){
                colnames(xtemp)[which(colnames(xtemp)==xvbles[i])]<-"value"
                xres2 <- ddply(xtemp, c(lets,"xto"), function(X) data.frame(value = weighted.mean(X$value, X$area)))
                xres <- cbind(xres, xres2$value)
                setnames(xres, "V2","value")
                colnames(xres)[which(colnames(xres)=="value")]<-xvbles[i]
            }
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
    #print(colnames(xres))
    #print(xresn)
    xresn<-c(xresn,colnames(xres)[!colnames(xres)%in%xresn])
    xres<-xres[,xresn,with=FALSE]
    return(xres)  
    
} #end of agguscie2hsu


pardescription<-function(parn=NULL){
    
    # Description to be used in the gdx-file
    # Attention! Limit to 255 characters
    
    if(parn=="p_emepdeposition"){
        
        description<-paste0("Average DRY or WET deposition (mg N m-2 yr-1). ",
                            "Dry dep. is calculated with Land-cover-specific deposition estimates",
                            "Wet deposition data are constant within each EMEP grid cell. ",
                            "EMEP/EMEP-LC -> uscie -> HSU. ")
        
        
    }
    
    
    return(description)
}
