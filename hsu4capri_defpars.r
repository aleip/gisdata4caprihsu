# Defined parameters and gives explanations to be used in the files.
# Links to 'parn' (parameter name)

# Is called when the "loading procedure" is done


# EMEP deposition

if(parn=="p_emepdeposition"){
    
    # File preprocessed with \\ies-ud01.jrc.it\D5_agrienv\Data\uscie\hsu2_database_update_2016_02orig\USCIE_EMEP_HSU2.r
    # loaded.
    
    # Remove cols that are not needed here
    xloaded<-emepdep[,c("hsu","dep","cmp","06","07","08","09","10","frac_hsu")]
    xloaded<-melt(xloaded,measure.vars = c("06","07","08","09","10"),value.name = xvbles,variable.name = "year")
    xloaded$EMEPdep<-xloaded$EMEPdep*xloaded$frac_hsu
    xloaded<-as.data.table(summarise_at(group_by(xloaded,hsu,dep,cmp,year),"sharedep","sum", na.rm=TRUE))
    ndim<-4

    orignames<-c("dep","cmp","year")
    newnames<-c("j","k","l")
    setnames(xloaded,orignames,newnames)
    # Now set column names as they shoudl apprear in the gdx file
    orignames<-c("WetOrDryDep","Compound","Year")
    origexpl<-c("Indicates if value refers to wet (WDEP) or dry (DDEP) deposition.",
                "Deposited compound: OXN: oxidized nitrogen, RDN: reduced nitrogen, SOX: oxidized sulphur.",
                "Year")
    
    
    
}else if(parn=="p_marsmeteomonths"){
    
    # Data are all for the years 2001-2011 therefore remove that column (k)
    # Data are all averages therefore remove column n
    ndim<-3
    xloaded<-select(xloaded,marsgrid,k,l,value)
    orignames<-c("k","l")
    newnames<-c("j","k")
    setnames(xloaded,orignames,newnames)
    orignames<-c("month","s_meteopars")
    origexpl<-c("Month of the year. All data are averages over the years 2001-2011.",
                "Meteo-parameters; see set s_meteopars in file marsmeteo.gdx and file hsu2_marsmeteo.gms")
}else if(parn=="p_marsyieldsmu"){
    
    # Data are all for the years 2001-2011 therefore remove that column (k)
    # Data are all averages therefore remove column n
    ndim<-4
    orignames<-c("marssmu","j","k","l","m")
    newnames<-c("i","marsgrid","smu","j","k")
    setnames(xloaded,orignames,newnames)
    xloaded<-select(xloaded,marsgrid,smu,i,j,k,value)
    newnames<-c("j","k","l")
    orignames<-c("s_marscropcode","year","yield")
    origexpl<-c("MARS potential and water-limited yield for some crops.",
                "")
}else if(parn=="p_center"){
    
    ndim<-1#Will be grouped just by hsu
    orignames<-xvbles
    newnames<-xvbles
    #setnames(xloaded,orignames,newnames)
    origexpl<-xmetadata$DESCRIPTION[which(xmetadata$FIELD%in%xvbles)]
    orignames<-c(xvbles,c("distnuts3km","distCAPRI_NUTSIIkm","distCAPRI_NUTS0km"))
    newnames<-letters[10:(10+6-2)]
    
}