source("hsu4capri_header.r")
source("hsu4capri_functions.r")

doforest<-0
dodem<-0
dorusle<-0
doirri<-0
dosoil<-0
dolucas<-0
docorine<-0

dofssgrid<-0
dopesetagrid<-0
dodeposition<-0
dometeo<-0

docentroids<-0

domarsyield<-0





if(docentroids==1){
    #### FOREST SHARE ####
    #This dataset contains forest share data at USCIE level
    xresult<-processdata(xfulln = paste0(usciedatapath,"HSU2_CENTER_COORDINATES.csv"),
                         spatunit = "hsu",parn = "p_center")
}
if(doforest==1){
    #### FOREST SHARE ####
    #This dataset contains forest share data at USCIE level
    xresult<-processdata(paste0(capridat,"forestshare.gdx"))
}
if(dodem==1){
    #### DIGITAL ELEVATION MODEL ####
    #data incomplete - need to use new data from uscie
    xresult<-processdata(paste0(usciedatapath,"USCIE_DEM.csv"),
                         xvbles = c("ALTITUDE_M","SLOPE_PERC"),
                         parn="p_uscierc_dem")
}
if(dorusle==1){
    #### DIGITAL ELEVATION MODEL ####
    #data incomplete - need to use new data from uscie

    xloaded1<-fread(paste0(usciedatapath,"uscie_rusle_KSTfactor.csv"), header=TRUE)
    xloaded2<-fread(paste0(usciedatapath,"uscie_rusle_LSfactor.csv"), header=TRUE)
    xloaded3<-fread(paste0(usciedatapath,"uscie_rusle_Rfactor.csv"), header=TRUE)
    xloaded4<-merge(xloaded1,xloaded2,by="USCIE_RC",all=TRUE)
    xrusle<-merge(xloaded4,xloaded3,by="USCIE_RC",all=TRUE)
    write.csv(xrusle,"rusle")
    
    xresult<-processdata(xfulln="rusle.csv",
                         xvbles = c("LSfactor","Rfactor","KSTfactor"),
                         parn="p_rusle")
}
if(doirri==1){
    xresult<-processdata(paste0(capridat,"uscie_irrishare.gdx"))
}
if(dosoil==1){
    xresult<-processdata(xfulln = paste0(usciedatapath,"HSU2_HWSD_MU_SOILPARAM_SEQ1.csv"),
                         spatunit="hsu",
                         parn="p_domstutop")
}
if(docorine==1){
    xresult<-processdata(xfulln = paste0(usciedatapath,"USCIE_PARAM.csv"),xvbles = "LC1_ID",parn="CLC_fraction")
}
if(dofssgrid==1){
    #xresult<-processdata(
    xfulln = "x:/adrian/google/projects/fss2010gridded/final_data/USCIE_GRID10_NUTS2_3_HSU_NOGOAREA.csv"
    print("Loading csv file...")
    xloaded<-fread(xfulln, header=TRUE)
    x<-xloaded[,.N,by=c("HSU2_IDRUN","USCIE_GRID10_NUTS2_3","HSU2_CD_NG")]
    x<-x[!grepl("^ ",USCIE_GRID10_NUTS2_3)]
    z<-as.data.table(dcast(x,HSU2_IDRUN+USCIE_GRID10_NUTS2_3~HSU2_CD_NG,value.var="N"))
    setnames(z,c("0","1","HSU2_IDRUN","USCIE_GRID10_NUTS2_3"),c("GO","nogo","s_hsu","grid10n23"))
    z[is.na(z)]<-0
    z<-z[,area:=GO+nogo]
    z<-z[,hsuarea:=sum(area),by="s_hsu"]
    z<-z[,gridarea:=sum(area),by="grid10n23"]
    z<-z[,fracHSU:=area/hsuarea]
    z<-z[,.SD,.SDcols=c("s_hsu","grid10n23","nogo","area","gridarea","fracHSU")]
    z$s_hsu<-paste0("U",z$s_hsu)
    export2gdx(z,ndim=2,parn="p_hsu_grid10n23",statistics=0,
               pardesc="Mapping between FSS 10 km grid at NUTS2 level",
               mydim1exp="HSU",
               myvars="Uscie-bsed overlay of 10 km grid cells with FSS-NUTS3 regions",
               myvarsexp="Parameter with mapping between HSU and 10kmNuts23 grids. Area=unit area. fracHSU=fraction of HSU in gridcell (gridcellarea).",
               varname="fssgridpars")
    z$nuts3<-unlist(lapply(strsplit(z$grid10n23,"_10kmE"),function(x) x[[1]]))
    a<-unique(z[,.SD,.SDcols=c("grid10n23","nuts3")])
    a$set<-paste0(a$grid10n23,"  .  ",a$nuts3)
    setkey(x=a,set)
    currun<-file("m_hsu_fss10kmnuts2.gms",open="wt")
    cat("set m_grid10_n23(*,*) 'Mapping between FSS 10 km grid at NUTS2-3 level' /",file=currun)
    write.table(a$set,file=currun,quote=FALSE,row.names=FALSE)
    cat("/;",file=currun)
    close(currun)
}
if(dopesetagrid==1){
    #xresult<-processdata(
    xfulln = paste0(usciedatapath,"uscie_peseta_grid.csv")
    xresult<-processdata(xfulln = xfulln,xvbles = "PESETAidgrid",parn="PESETAgrid_fraction")}

if(dolucas==1){
    xresult<-getlucas(xfulln = paste0(usciedatapath,"../lucas/LUCAS09EU23_USCIERC.csv"),
                         parn="lucas",
                         xvbles = c("POINT_ID", "X_LAEA", "Y_LAEA", "Year", "USCIE_RC", "LC1"))
}

if(dodeposition==1){
    xresult<-processdata(xfulln = paste0(usciedatapath,"USCIE_EMEP_LC_HSU2_W-DDP.rdata"),spatunit="hsu",xvbles="EMEPdep",parn="p_emepdeposition")
}
if(dometeo==1){
    metpath<-gsub("uscie","mars_grid_weather201207",usciedatapath)
    metpath<-gsub("\\\\hsu2_database_update_2016_02orig","",metpath)
    xresult<-processdata(xfulln = paste0(metpath,"marsmeteo.gdx"),spatunit="marsgrid",parn="p_marsmeteomonths")
}
if(domarsyield==1){
    metpath<-gsub("uscie","marsyield",usciedatapath)
    metpath<-gsub("\\\\hsu2_database_update_2016_02orig","",metpath)
    xresult<-processdata(xfulln = paste0(metpath,"mars_yield.gdx"),spatunit="marsgrid",parn="p_marsyieldsmu")
}


