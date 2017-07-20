source("hsu4capri_header.r")
source("hsu4capri_functions.r")

doforest<-0
dodem<-0
doirri<-0
dosoil<-0
dolucas<-0
docorine<-0
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


