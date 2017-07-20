linkxto2xfrom<-function(x2agg,xfrom,xto){
    
    if(xfrom=="s_uscierc"){
        print(paste0("Linking ", xfrom, " data with ", xto))
        x2agg$s_uscierc<-as.integer(as.character(x2agg$s_uscierc))
        setkey(x2agg,s_uscierc)
        xhsu<-xhsu<-x2agg[uscie_hsu]   # merge HSU (left join)
        x2agg<-xhsu[complete.cases(xhsu)]
        
    }else if(xfrom=="marsgrid"){
        print(paste0("Linking ", xfrom, " data with ", xto))
        x2agg$marsgrid<-as.numeric(as.character(x2agg$marsgrid))
        #setkey(marsgrid_hsu, marsgrid)
        xhsu<-x2agg[marsgrid_hsu,on="marsgrid",allow.cartesian=TRUE]
        xhsu<-select(xhsu,-gridarea,-area)
        xhsu<-xhsu[complete.cases(xhsu)]
        xhsu$fracvalue<-xhsu$value*xhsu$fraction
        #xhsu<-x2agg[marsgrid_hsu,allow.cartesian=TRUE]
        if(ndim==3){
            x2agg<-as.data.table(summarise_at(group_by(xhsu,hsu,j,k),"fracvalue","sum", na.rm=TRUE))
            #x2agg <- xhsu[,.(value =  weighted.mean(value, fraction)), by=.(hsu, j, k)]  #xavi: this step takes a bit long
        }else if(ndim==4){  #xavi: for now, always ndim is 2 or 3
            x2agg<-as.data.table(summarise_at(group_by(xhsu,hsu,j,k,l),"fracvalue","sum", na.rm=TRUE))
        }else if(ndim==2){  #xavi: for now, always ndim is 2 or 3
            x2agg<-as.data.table(summarise_at(group_by(xhsu,hsu,j),"fracvalue","sum", na.rm=TRUE))
            #x2agg <- xhsu[,.(value =  weighted.mean(value, fraction)), by=.(hsu, j)]
        }else{stop("Number of dims not correct (function linkxto2xfrom)")}
        setnames(x2agg, old=c("fracvalue"), new=c("value"))
        
    }else if(spatunit=="mu_global"){
        
        hsudef<-fread(paste0(usciedatapath,"HSU2_DEFPARAM.csv"))
        hsudef<-select(hsudef,HSU2_IDRUN,MU_GLOBAL,SMU)
        setnames(hsudef,old="HSU2_IDRUN",new="hsu")
        setnames(hsudef,old="MU_GLOBAL",new=spatunit)
        hsudef2<-hsudef[mu_global%in%x2agg$mu_global]
        setkey(hsudef,mu_global)
        x2agg$mu_global<-as.numeric(as.character(x2agg$mu_global))
        x2agg<-x2agg[complete.cases(x2agg)]
        setkey(x2agg,mu_global)
        x2agg<-merge(hsudef,x2agg,by="mu_global",all.x=TRUE)
        
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
        xtemp
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

## Exporting to a gdx file with ##

#export2gdx<-function(x2gdx, ndim=ndim, xfulln=xfulln, parn=parn){
export2gdx<-function(x2gdx, ndim=ndim, parn=parn,statistics=1,pardesc=NULL,vars=NULL,myvars=NULL){
    
    print("Exporting to a gdx file...")
    x2gdxloc <- x2gdx[complete.cases(x2gdx)] # to remove NAs
    #x2gdxloc <- droplevels(x2gdxloc)
    x2gdxloc <- as.data.frame(x2gdxloc)
    
    if(! is.null(vars)&ndim>1){
        # Rename columns
        #print(names(x2gdxloc))
        #print(letters[10:(10+ndim-2)])
        #print(vars)
        oldn<-names(x2gdxloc)[which(names(x2gdxloc)%in%letters[10:(10+ndim-2)])]
        setnames(x2gdxloc,letters[10:(10+ndim-2)],vars)
    }
    
    nm <- tolower(parn)
    if(is.null(pardesc)) pardesc<-paste0("Data for ", nm) 
    
    symDim<-ndim+1
    attr(x2gdxloc,"symName") <- nm
    attr(x2gdxloc, "ts") <- pardesc   #explanatory text for the symName
    if(is.null(myvars)) myvars<-paste0("variables",c(1:(ndim-1)))
    #print(myvars)
    myText<-c("Spatial units: CAPRI-NUTS0, CAPRI-NUTS2, Gisco-NUTS3, HSU",
              myvars,
              paste0("Statistics calculated on the basis of uscie (HSU) or HSU (regions). ",
                     "For HSU value refers to the direct value if available or average over uscie. ",
                     "For regions, value is the area-weighted average."))
    lst <- wgdx.reshape(x2gdxloc, symDim, tName = "s_statistics", setsToo=TRUE, order=c(1:ndim,0), setNames = myText)   #to reshape the DF before to write the gdx. tName is the index set name for the new index position created by reshaping
    wgdx.lst(paste0(nm, "_stats.gdx"), lst)
} #end of export2gdx


pardescription<-function(parn=NULL){
    
    # Description to be used in the gdx-file
    # Attention! Limit to 255 characters
    
    if(parn=="p_emepdeposition"){
        
        description<-paste0("Average DRY or WET deposition (mg N m-2 yr-1). ",
                            "Dry dep. is calculated with Land-cover-specific deposition estimates",
                            "Wet deposition data are constant within each EMEP grid cell. ",
                            "EMEP/EMEP-LC -> uscie -> HSU. ")
        
        
    }else if(parn=="p_marsmeteomonths"){
        
        description<-paste0("Meteo parameters calculated from daily temperature and precipitation data.",
                            "Data are monthly data based on daily, data for the years 2001-2011.",
                            "First, monthly data, then decadal values are calculated.",
                            "Statistical info refers to the aggregation from HSUs.")
        
        #@inro     : Data processed from
        # Temperature and precipitation data at 25 km x 25 km MARS grid, sent from Irene Biavetti to Renate Koeble (July 2012)
        # File gridweather.csv.gz (about 500 MB) obtained from Renate July 2013
        # Processed (modified date-format, replace semicolon-comma etc.) and split into monthly files grid_weather_%year%%month%.csv
        # This file works in several steps: 
        # \\ies-ud01.jrc.it\D5_agrienv\Data\mars_grid_weather201207\hsu2_marsmeteo.gms
        #     (i) Read all csv-files and calculate monthly averages/sums (stored in files ..\dat\capdis\hsu2\mars_weather2010.gdx
        #     (ii) Read the annual gdx-files and calculate decadal monthly data & quartal data
        #     (iii) Map data from mars-grid to hsu2 and aggregate to nuts2 and countries
        
        
        # *  Tsum8 calculation in WOFOST depends on the crop, 
        # see http://marswiki.jrc.ec.europa.eu/agri4castwiki/index.php/The_Wofost_model 
        # under 'Phenological development'. 
        # 
        # Some explanations received:
        #     -------------------- Original message --------------------
        # from: fabien.ramos@jrc.ec.europa.eu
        # date: Thu, 25 Jul 2013 12:54:53 +0200
        # to: 'Adrian Leip' <adrian.leip@jrc.ec.europa.eu>, 'Renate Koeble' <renate.koeble@ext.jrc.ec.europa.eu>
        # cc: 'antonio zucchini' <antonio.zucchini@ext.jrc.ec.europa.eu>, 'Davide Fanchini' <davide.fanchini@ext.jrc.ec.europa.eu>
        #     subject: RE: harvested potential and water limited yield for Norway
        # 
        # Well, it is not so easy because WOFOST distinguishes before and after the emergence, and there is a cutoff.
        # Before the emergence, the threshold is constant and 
        # 
        # TSUM(n)=TSUM(N-1) +MAX(TCUTOFF,TMEAN-TBASE (With TBASE = cte))
        # 
        # After emergence WOFOST uses some curves crop specific that gives the
        # increase of TSUM function of the mean temperature of the day.
        # 
        # TSUM(N)= TSUM(N-1) + (a*TMEAN + b)
        # 
        # We can give you all the parameters used, but because it is a little bit of work before let us know if you really need all of them.
        # 
        # *  Here we don't need that level of detail, but use the example, i.e. 
        # Tsum8_day = min(27,T-8); 
        
        
        
        
    }else if(parn=="p_marsyieldsmu"){
        description<-paste0("MARS potential and water-limited yield for some crops.")
    }else if(parn=="p_center"){
        description<-paste0("Center of gravity and distance to HSU")
    }
    
    return(description)
}


alldistances<-function(xtmp){
    hsudistances<-function(xpassed){
        
        #function dist() could be used as well, but it requires a matrix
        #         Euclidian distance is too simple to need a special function
        calcdist<-function(x,n){
            x<-x2proc[x,on=n]
            setnames(x,xvbles,c("k","l"))
            x$dist<-sqrt((x$i-x$k)^2+(x$j-x$l)^2)/1000
            x<-select(x,-k,-l)
            setnames(x,"dist",paste0("dist",n,"km"))
            return(x)
        }
        
        x2proc<-copy(xpassed)
        setnames(x2proc,xvbles,c("i","j"))
        if(colnames(x2proc)[1]!="hsu") {
            m<-unique(select(hsu2_nuts,-hsu,-area,-SMU))
        }else{
            m<-select(hsu2_nuts,-area,-SMU)
        }
        x2proc<-x2proc[m,on=colnames(x2proc)[1]]
        x2proc<-x2proc[complete.cases(x2proc)]
        x2proc<-calcdist(xnuts3,"nuts3")
        x2proc<-calcdist(xnuts2,"CAPRI_NUTSII")
        x2proc<-calcdist(xnuts0,"CAPRI_NUTS0")
        
        # if(colnames(x2proc)[1]!="hsu") {
        #     x2proc$hsu<-0
        # }    
        setnames(x2proc,c("i","j"),xvbles)
        setnames(xloaded,orignames,newnames)
        return(x2proc)
        
    }
    xres<-list()
    for(i in 1:length(xtmp)){
        x<-hsudistances(xtmp[[i]])
        xres[[i]]<-x
        
        
    }
    return(xres)
    
}

meteoquartals<-function(xhsu){
    
    xhsu$j<-as.numeric(xhsu$j)
    xhsu$quartal<-ceiling(xhsu$j/3)
    xhsu$mdays[xhsu$j%in%c(1,3,5,7,8,10,12)]<-31
    xhsu$mdays[xhsu$j%in%c(4,6,9,11)]<-30
    xhsu$mdays[xhsu$j%in%c(2)]<-28
    
    xhsu$valmdays<-xhsu$value*xhsu$mdays
    
    # Sum for prec, precday, Tsum8mars,Tgt8days,Tsum8, E0
    sumpar<-c("prec", "precday","Tsum8mars", "Tgt8days","Tsum8", "E0")
    xresq<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%sumpar),hsu,quartal,k),"value","sum"))
    xresy<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%sumpar),hsu,k),"value","sum"))
    
    # Mean for Tav, ET0
    meanpar<-c("Tav","ET0")
    meanq<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%meanpar),hsu,quartal,k),c("mdays","valmdays"),"sum"))
    meany<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%meanpar),hsu,k),c("mdays","valmdays"),"sum"))
    meanq$value<-meanq$valmdays/meanq$mdays
    meany$value<-meany$valmdays/meany$mdays
    
    # Min for Tmin
    minpar<-c("Tmin")
    minq<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%minpar),hsu,quartal,k),"value","min"))
    miny<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%minpar),hsu,k),"value","min"))
    
    # Max for Tmax, precmax
    maxpar<-c("Tmax")
    maxq<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%maxpar),hsu,quartal,k),"value","max"))
    maxy<-as.data.table(summarise_at(group_by(filter(xhsu,k%in%maxpar),hsu,k),"value","max"))
    
    # Vegperiod more complicated
    # Calculation of the vegetation period
    # * This is a value that assigns a fraction (0.8) of "veg" days in a 'bordering' month to the vegetation period.
    # * Example: In May all days are > 8 deg C, thus it is fully part of the vegetation period
    # *        In April only 20 days are > 8 deg C. 
    # *        We assume that "veg" of the days are to be counted to the vegetation period, 
    # *        while (1-"veg") of the days are in a transition period (fluctuation around 8 deg C)
    
    mm<-c(12,1:11)
    xveg<-xhsu[k=="Tgt8days"]
    # Flag those month which are (almost) fully 'warm'
    xveg$flag<-xveg$mdays<=(xveg$value+1.5)
    
    # Flag the neighboring months. dcast as otherwise too long
    xveg2<-as.data.table(dcast(xveg,hsu ~ j, value.var = "flag"))
    xveg<-select(xveg,-flag) #Not needed any more
    xveg2[is.na(xveg2)]<-FALSE
    xveg2<-as.data.table(apply(xveg2,2,as.numeric))
    xveg3<-xveg2
    setnames(xveg3,2:13,paste0("m",1:12))
    
    # Mutliplicative factor veg=0.8. 
    # We assume that in a bordering month most warm days are extending the vegetation period
    # (1-veg) of the days are in a transition period and thus outside the vegetation period
    veg<-0.8
    xveg3[m1==0&m2==1]$m1<-veg
    xveg3[m2==0&m3==1]$m2<-veg
    xveg3[m3==0&m4==1]$m3<-veg
    xveg3[m4==0&m5==1]$m4<-veg
    xveg3[m5==0&m6==1]$m5<-veg
    xveg3[m6==0&m7==1]$m6<-veg
    xveg3[m7==0&m8==1]$m7<-veg
    xveg3[m8==0&m9==1]$m8<-veg
    xveg3[m9==0&m10==1]$m9<-veg
    xveg3[m10==0&m11==1]$m10<-veg
    xveg3[m11==0&m12==1]$m11<-veg
    xveg3[m12==0&m1==1]$m12<-veg

    xveg3[m1==1&m2==0]$m1<-veg
    xveg3[m2==1&m3==0]$m2<-veg
    xveg3[m3==1&m4==0]$m3<-veg
    xveg3[m4==1&m5==0]$m4<-veg
    xveg3[m5==1&m6==0]$m5<-veg
    xveg3[m6==1&m7==0]$m6<-veg
    xveg3[m7==1&m8==0]$m7<-veg
    xveg3[m8==1&m9==0]$m8<-veg
    xveg3[m9==1&m10==0]$m9<-veg
    xveg3[m10==1&m11==0]$m10<-veg
    xveg3[m11==1&m12==0]$m11<-veg
    xveg3[m12==1&m1==0]$m12<-veg
    setnames(xveg3,paste0("m",1:12),paste0(1:12))
    
    xveg4<-melt(xveg3,measure.vars = 2:13,variable.name = "j",value.name = "flag")
    xveg4$j<-as.numeric(as.character(xveg4$j))
    xveg5<-xveg[xveg4,on=c("hsu","j")]
    
    xveg5$value<-xveg5$value*xveg5$flag
    
    sumpar<-c("Tgt8days")
    vegpq<-as.data.table(summarise_at(group_by(filter(xveg5,k%in%sumpar),hsu,quartal,k),"value","sum"))
    vegpy<-as.data.table(summarise_at(group_by(filter(vegpq,k%in%sumpar),hsu,k),"value","sum"))
    vegpy$k<-"VegP"
    vegpq$k<-"VegP"
    
    
    xquart<-rbind(xresq,select(meanq,hsu,quartal,k,value),minq,maxq,vegpq)
    xyear<-rbind(xresy,select(meany,hsu,k,value),miny,maxy,vegpy)
    xyear$quartal<-"all"
    xmeteo<-rbind(xquart,xyear)
    
    xstart<-preparedata(xmeteo)
    functs=c("max", "min", "mean", "sd", "median")
    xvbles<-"value"
    setnames(xstart,"quartal","j")
    xfrom<-"hsu"
    xto<-"nuts3"
    orignames<-c("quartal","s_meteopars")
    newnames<-c("j","k")
    save(xstart,functs,xvbles,ndim,xfrom,xto,orignames,newnames,file="xstart339.rdata")
    xnuts3<-agguscie2hsu(xstart,xfrom="hsu",xto="nuts3",xvbles=xvbles,ndim=ndim,functs=functs)
    xnuts2<-agguscie2hsu(xstart,xfrom="hsu",xto="CAPRI_NUTSII",xvbles=xvbles,ndim=ndim,functs=functs)
    xnuts0<-agguscie2hsu(xstart,xfrom="hsu",xto="CAPRI_NUTS0" ,xvbles=xvbles,ndim=ndim,functs=functs)
    
    newparn<-gsub("p_marsmeteomonths","p_marsmeteoquartyears",parn)
    save(xmeteo,xnuts3,xnuts2,xnuts0,file=gsub("p_marsmeteomonths","p_marsmeteoquartyears",paste0(parn,".rdata")))
    
    # Prepare to export to gdx file for CAPRI
    colnames(xnuts3)[1]<-"spatial_unit"
    colnames(xnuts2)[1]<-"spatial_unit"
    colnames(xnuts0)[1]<-"spatial_unit"
    xmeteo$hsu<-paste0("U",xmeteo$hsu)
    colnames(xmeteo)[1]<-"spatial_unit"
    setnames(xmeteo,"k","s_meteopars")
    xall<-rbind(xnuts0,xnuts2,xnuts3)
    
    
    export2gdx(xmeteo, ndim=3, parn=newparn,pardesc=pardescription(parn),vars=NULL,myvars = origexpl)  # to export to gdx
    export2gdx(xall, ndim=3, parn=paste0(newparn,"nutd"),pardesc=pardescription(parn),vars=orignames,myvars = origexpl)  # to export to gdx
    
}