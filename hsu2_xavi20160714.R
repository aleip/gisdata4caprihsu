rm(list=objects())
source("X:/MARS_disaggregation/gisdata4caprihsu/hsu4capri_header.r") #this is only used while coding
#source("hsu4capri_header.r")


###### Data to be computed #####



#### CENTROIDS ####

hsu2_coo <- fread("HSU2_CENTER_COORDINATES.csv", header=TRUE) #load of csv file linking centroid coordinates and HSU numbers, dataset coming from capri/dat/capdis/hsu2
colnames(hsu2_coo)[1] <- "hsu2"
setkey(hsu2_coo, "hsu2") # to set a key column of the DataTable

hsu2_coo_nuts <- merge(hsu2_nuts, hsu2_coo, by.x = "hsu", by.y = "hsu2", all.x = TRUE)

#save(hsu2_coo_nuts, file = "hsu2_coo_nuts.rdata")
#load(file = "hsu2_coo_nuts.rdata")


## Calculating centroids for nuts3

by_nuts3 <- hsu2_coo_nuts %>% group_by(nuts3)           #grouping the DataTable before to be passed to the summarise_at()
coo_mean_nuts3 <- by_nuts3 %>% summarise_at(c("HSU2_CENTR_COOR_X_m", "HSU2_CENTR_COOR_Y_m"), mean)

coo_mean_nuts3 <- coo_mean_nuts3[!is.na(coo_mean_nuts3$nuts3),]

nearesthsu2nuts3 <- data.frame(nearest_hsu=character(), HSU2_CENTR_COOR_X_m=numeric(), HSU2_CENTR_COOR_Y_m=numeric(), stringsAsFactors=FALSE)

for (rw in 1:nrow(coo_mean_nuts3)){
  
  print(paste0("spatial unit num ", rw, "/", nrow(coo_mean_nuts3)))
  sp_unit <- as.character(coo_mean_nuts3[rw, 1])
  hsu_coo_spunit <- hsu2_coo_nuts[hsu2_coo_nuts$nuts3 %in% sp_unit, c(1,8,9), with=FALSE] #HSU in the Spatial Unit
  names(hsu_coo_spunit)[1] <- "spatialunit"
  
  coo_mean_nuts3_1 <- coo_mean_nuts3[rw,]
  names(coo_mean_nuts3_1)[1] <- "spatialunit"
  
  
  dist_mtx <- as.matrix(dist(rbind(coo_mean_nuts3_1, hsu_coo_spunit)))
  dist_col <- dist_mtx[,1]
  min_dist <- min(dist_col[dist_col>0])
  min_posit <- as.numeric(which(dist_col == min_dist, arr.ind = TRUE, useNames = TRUE)) - 1
  
  nearest_hsu <- hsu_coo_spunit[min_posit,]
  names(nearest_hsu)[1] <- "nearest_hsu"
  #print(nearest_hsu[1])
  
  if (any(is.na(nearest_hsu[1,])==TRUE)){
    nearesthsu2nuts3[rw, 1:3] <- c(NA, NA, NA)
  }else{
    nearesthsu2nuts3[rw, 1:3] <- nearest_hsu
  }
}

nuts3_nearesthsu <- cbind(coo_mean_nuts3, nearesthsu2nuts3) 
names(nuts3_nearesthsu) <- c("spatial_unit", "spunit_center_coor_x_m", "spunit_center_coor_y_m", "nearest_hsu", "nearest_hsu_coor_x_m", "nearest_hsu_coor_y_m")
nuts3_nearesthsu <- nuts3_nearesthsu[complete.cases(nuts3_nearesthsu),]
rm(coo_mean_nuts3, nearesthsu2nuts3, by_nuts3)



## Calculating centroids for CAPRI NUTSII


by_CAPRI_NUTSII <- hsu2_coo_nuts %>% group_by(CAPRI_NUTSII)           #grouping the DataTable before to be passed to the summarise_at()
coo_mean_CAPRI_NUTSII <- by_CAPRI_NUTSII %>% summarise_at(c("HSU2_CENTR_COOR_X_m", "HSU2_CENTR_COOR_Y_m"), mean)

coo_mean_CAPRI_NUTSII <- coo_mean_CAPRI_NUTSII[!is.na(coo_mean_CAPRI_NUTSII$CAPRI_NUTSII),]

nearesthsu2nuts2 <- data.frame(nearest_hsu=character(), HSU2_CENTR_COOR_X_m=numeric(), HSU2_CENTR_COOR_Y_m=numeric(), stringsAsFactors=FALSE)

for (rw in 1:nrow(coo_mean_CAPRI_NUTSII)){
  
  print(paste0("spatial unit num ", rw, "/", nrow(coo_mean_CAPRI_NUTSII)))
  sp_unit <- as.character(coo_mean_CAPRI_NUTSII[rw,1])
  hsu_coo_spunit <- hsu2_coo_nuts[hsu2_coo_nuts$CAPRI_NUTSII %in% sp_unit, c(1,8,9), with=FALSE] #HSU in the Spatial Unit
  names(hsu_coo_spunit)[1] <- "spatialunit"
  
  coo_mean_CAPRI_NUTSII_1 <- coo_mean_CAPRI_NUTSII[rw,]
  names(coo_mean_CAPRI_NUTSII_1)[1] <- "spatialunit"
  
  
  dist_mtx <- as.matrix(dist(rbind(coo_mean_CAPRI_NUTSII_1, hsu_coo_spunit)))
  dist_col <- dist_mtx[,1]
  min_dist <- min(dist_col[dist_col>0])
  min_posit <- as.numeric(which(dist_col == min_dist, arr.ind = TRUE, useNames = TRUE)) - 1
  
  nearest_hsu <- hsu_coo_spunit[min_posit,]
  names(nearest_hsu)[1] <- "nearest_hsu"
  
  if (any(is.na(nearest_hsu[rw]))==TRUE){
    nearesthsu2nuts2[rw, 1:3] <- c(NA, NA, NA)
  }else{
    nearesthsu2nuts2[rw, 1:3] <- nearest_hsu
  }
}

caprinuts2_nearesthsu <- cbind(coo_mean_CAPRI_NUTSII, nearesthsu2nuts2) 
names(caprinuts2_nearesthsu) <- c("spatial_unit", "spunit_center_coor_x_m", "spunit_center_coor_y_m", "nearest_hsu", "nearest_hsu_coor_x_m", "nearest_hsu_coor_y_m")
rm(coo_mean_CAPRI_NUTSII, nearesthsu2nuts2, by_CAPRI_NUTSII)

## Calculating centroids for CAPRI NUTS0

by_CAPRI_NUTS0 <- hsu2_coo_nuts %>% group_by(CAPRI_NUTS0)           #grouping the DataTable before to be passed to the summarise_at()
coo_mean_CAPRI_NUTS0 <- by_CAPRI_NUTS0 %>% summarise_at(c("HSU2_CENTR_COOR_X_m", "HSU2_CENTR_COOR_Y_m"), mean)

coo_mean_CAPRI_NUTS0 <- coo_mean_CAPRI_NUTS0[!is.na(coo_mean_CAPRI_NUTS0$CAPRI_NUTS0),]

nearesthsu2nuts0 <- data.frame(nearest_hsu=character(), HSU2_CENTR_COOR_X_m=numeric(), HSU2_CENTR_COOR_Y_m=numeric(), stringsAsFactors=FALSE)
memory.limit(size = 2048000)

for (rw in 1:nrow(coo_mean_CAPRI_NUTS0)){
  
  print(paste0("spatial unit num ", rw, "/", nrow(coo_mean_CAPRI_NUTS0)))
  sp_unit <- as.character(coo_mean_CAPRI_NUTS0[rw,1])
  hsu_coo_spunit <- hsu2_coo_nuts[hsu2_coo_nuts$CAPRI_NUTS0 %in% sp_unit, c(1,8,9), with=FALSE] #HSU in the Spatial Unit
  names(hsu_coo_spunit)[1] <- "spatialunit"
  
  coo_mean_CAPRI_NUTS0_1 <- coo_mean_CAPRI_NUTS0[rw,]
  names(coo_mean_CAPRI_NUTS0_1)[1] <- "spatialunit"
  
  dist_mtx <- as.matrix(dist(rbind(coo_mean_CAPRI_NUTS0_1, hsu_coo_spunit)))
  dist_col <- dist_mtx[,1]
  min_dist <- min(dist_col[dist_col>0])
  min_posit <- as.numeric(which(dist_col == min_dist, arr.ind = TRUE, useNames = TRUE)) - 1
  
  nearest_hsu <- hsu_coo_spunit[min_posit,]
  names(nearest_hsu)[1] <- "nearest_hsu"
  
  if (any(is.na(nearest_hsu[rw]))==TRUE){
    nearesthsu2nuts0[rw, 1:3] <- c(NA, NA, NA)
  }else{
    nearesthsu2nuts0[rw, 1:3] <- nearest_hsu
  }
  
}

caprinuts0_nearesthsu <- cbind(coo_mean_CAPRI_NUTS0, nearesthsu2nuts0) 
names(caprinuts0_nearesthsu) <- c("spatial_unit", "spunit_center_coor_x_m", "spunit_center_coor_y_m", "nearest_hsu", "nearest_hsu_coor_x_m", "nearest_hsu_coor_y_m")
rm(coo_mean_CAPRI_NUTS0, nearesthsu2nuts0, by_CAPRI_NUTS0)


# Exporting Centroids of Spatial Units to gdx

centroids_spatialunit <- rbind(nuts3_nearesthsu, caprinuts2_nearesthsu, caprinuts0_nearesthsu)
centroids_spatialunit$nearest_hsu <- paste0("U", centroids_spatialunit$nearest_hsu)

save(centroids_spatialunit, file = "centroids_spatialunit.rdata")
#load(file = "centroids_spatialunit.rdata")


symDim <- 3
attr(centroids_spatialunit,"symName") <- "p_spunit_centroids_hsu"
attr(centroids_spatialunit, "ts") <- "for each spatial unit (capri nuts0, capri nuts2, nuts3), the coordinates of its centroid and the nearest hsu to the spatial unit centroid"   #explanatory text for the symName
myText <- c("spatial unit", "nearest hsu","coordinates of the spatial unit centroid")     # explanatory text for the extracted index sets
lst <- wgdx.reshape(centroids_spatialunit, symDim, order=c(1,4,0), tName = "s_spatialunit_coordinates", setNames = myText, setsToo=FALSE)

symDim <- 2
centroids_spatialunit1 <- centroids_spatialunit[, c(1:3)]
attr(centroids_spatialunit1,"symName") <- "p_centroids_spatialunit"
attr(centroids_spatialunit1, "ts") <- "for each spatial unit (capri nuts0, capri nuts2, nuts3), the coordinates of its centroid"   #explanatory text for the symName
lst1 <- wgdx.reshape(centroids_spatialunit1, symDim, order=c(1,0), tName = "s_spatialunit_coordinates", setNames = myText, setsToo=TRUE)

symDim <- 2
centroids_spatialunit2 <- centroids_spatialunit[, c(4:6)]
attr(centroids_spatialunit2,"symName") <- "p_centroids_hsu"
attr(centroids_spatialunit2, "ts") <- "for each spatial unit (capri nuts0, capri nuts2, nuts3), the coordinates of the nearest hsu to the spatial unit centroid, also with hsu's coordinates"   #explanatory text for the symName
lst2 <- wgdx.reshape(centroids_spatialunit2, symDim, order=c(1,0), tName = "s_hsu_coordinates", setNames = myText, setsToo=TRUE)


wgdx.lst("centroids_spatialunit", c(lst, lst1, lst2))
#wgdx.lst("centroids_spatialunit", c(lst))



#### FOREST SHARE ####
#This dataset contains forest share data at USCIE level
xresult<-processdata(paste0(capridat,"forestshare.gdx"))

            


#### DIGITAL ELEVATION MODEL ####


#gdxInfo("uscie_dem.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
#This dataset contains forest share data at USCIE level
uscie_dem <- rgdx.param("uscie_dem.gdx","p_uscierc_dem") #load gdx file linking uscie and dem, dataset coming from capri/dat/capdis/uscie
colnames(uscie_dem)[1] <- "s_uscierc"
colnames(uscie_dem)[3] <- "variables" 
uscie_dem2 <- dcast(uscie_dem, s_uscierc ~ variables, drop=TRUE, value.var="value")  #to split the variables in the data set (altitude_m, slope_perc) in two different columns 

## Running Function 3: Preparing data set to run function 1

dem <- preparedata(uscie_dem2)

### Running Function1: 

# Statistics can be computed aggregating data either by USCIE or by HSU2
# By USCIE
dem_uscie_nuts <- agg2admins(dem, data2ag = c("uscie"), functs = c("max", "min", "mean", "sd", "median"), vbles = c("altitude_m", "slope_perc"), filenm4gdx = "dem_uscie_stats", na.rm=TRUE)

# By HSU2
dem_hsu_nuts <- agg2admins(dem, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("altitude_m", "slope_perc"), filenm4gdx = "dem_hsu_stats", na.rm=TRUE)


save(dem_uscie_nuts, file="dem_uscie_nuts.rdata")
#load(file = "dem_uscie_nuts.rdata")
save(dem_hsu_nuts, file="dem_hsu_nuts.rdata")
remove(dem_uscie_nuts)
remove(dem_hsu_nuts)
remove(uscie_dem, uscie_dem2, dem)






#### SOIL ####

#gdxInfo("soil_hwsd.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
#This dataset contains soil data at HSU2 level.
adf_soil <- rgdx.param("soil_hwsd.gdx", "p_soildom")  #load of gdx linking soil properties and hsu, dataset coming from capri/dat/capdis/hsu2
names(adf_soil) <- c("hsu", "variables", "value")
hsu2_soil <- dcast(adf_soil, hsu ~ variables, drop=TRUE, value.var="value")  #to split the variables in columns containing soil info

## Running Function 3: Preparing data set to run function 1

soil <- preparedata(hsu2_soil)


### Running Function1: 
# xavi: I couldn't find soil data at uscie level, so it only can be computed aggregating data at HSU level

vrbles <- names(hsu2_soil)[-1]
soil_hsu_nuts2 <- agg2admins(soil, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = vrbles, filenm4gdx = "soil_hsu_stats", na.rm=TRUE)


save(soil_hsu_nuts2, file="soil_hsu_nuts2.rdata")






#### MARS_YIELD ####

#gdxInfo("mars_yield.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
#This dataset contains soil data at HSU2 level.
mars_yield <- rgdx.param("mars_yield.gdx", "p_marsyield")  #load of gdx linking yield properties and hsu, dataset coming from capri/dat/capdis/hsu2
names(mars_yield) <- c("s_hsu2","crop","yield","year","value")
#head(mars_yield)

mars_yield_smth <- rgdx.param("mars_yield.gdx", "p_marsyieldsmooth")  #load of gdx linking yield properties and hsu, dataset coming from capri/dat/capdis/hsu2
names(mars_yield_smth) <- c("s_hsu2","crop","yield","year","value")
#head(mars_yield_smth)
mars_yield_smth2 <- dcast(mars_yield_smth, s_hsu2 + crop + year ~ yield, drop=TRUE, value.var="value")  #to split the variables in columns containing yield/crp/year info
#head(mars_yield_smth2)

#Mean of years per crop
#by_hsu_crop <- mars_yield_smth2 %>% group_by(s_hsu2, crop)           #grouping the DataTable before to be passed to the summarise_at(), to be faster
#mars_yield_smth3 <- by_hsu_crop %>% summarise_at(c("PYLD", "WYLD"), mean)
#Sum of crops (yield) per hsu   #xavi: does it make any sense? would it be better the average?
#by_hsu <- mars_yield_smth3 %>% group_by(s_hsu2)           #grouping the DataTable before to be passed to the summarise_at(), to be faster
#mars_yield_smth4 <- by_hsu %>% summarise_at(c("PYLD", "WYLD"), sum)


mars_yield_smu <- rgdx.param("mars_yield.gdx", "p_marsyieldsmu")  #load of gdx linking yield properties and hsu, dataset coming from capri/dat/capdis/hsu2
names(mars_yield_smu) <- c("crop","grid", "smu","years","yields","value")  
head(mars_yield_smu)




## Running Function 3: Preparing data set to run function 1

yield_smth_hsu <- func3(mars_yield_smth4)


### Running Function1: 
# xavi: I couldn't find yield data at uscie level, so it only can be computed aggregating data at HSU level

yield_smth_hsu2_nuts <- agg2admins(yield_smth_hsu, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("PYLD", "WYLD"), filenm4gdx = "yield_hsu_sta", na.rm=TRUE)
# xavi: I'm not sure about the results. There are only data for NO and SE

# xavi: Another way to compute statistics for yield could be subsetting data for each crop and computing statistics per hsu and crop







#### IRRISHARE ####



#gdxInfo("uscie_irrishare.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
#This dataset contains irrigation data at USCIE level.
uscie_irr <- rgdx.param("uscie_irrishare.gdx", "irr_share2000")  #load of gdx linking irrigation data and hsu, dataset coming from capri/dat/capdis/hsu2
names(uscie_irr) <- c("s_uscierc", "irr")
#No need to dcast because there is only one variable

## Running Function 3: Preparing data set to run function 1

irrishare <- preparedata(uscie_irr)


### Running Function1: 

irr_uscie_nuts <- agg2admins(irrishare, data2ag = c("uscie"), functs = c("max", "min", "mean", "sd", "median"), vbles = "irr", filenm4gdx = "irrishare_uscie_stats", na.rm=TRUE)

# By HSU
irr_hsu_nuts <- func1(irrishare, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = "irr", filenm4gdx = "irrishare_hsu_stats", na.rm=TRUE)


save(irr_uscie_nuts, file="irr_uscie_nuts.rdata")
save(irr_hsu_nuts, file="irr_hsu_nuts.rdata")

rm(uscie_irr, irrishare, irr_uscie_nuts, irr_hsu_nuts)





#### METEO ####


##Meteo per month

#gdxInfo("marsmeteo.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
#This dataset contains meteo data at GRID level.
grid_meteo_mth <- rgdx.param("marsmeteo.gdx", "p_marsmeteomonths")  #load of gdx linking soil properties and hsu, dataset coming from capri/dat/capdis/hsu2
names(grid_meteo_mth) <- c("gridno", "years", "month", "variable", "avg", "value")
grid_meteo_mth <- grid_meteo_mth[, c(1, 3, 4, 6)]
grid_meteo_mth <- as.data.table(grid_meteo_mth)
setkey(grid_meteo_mth, "gridno") # to set a key column of the DataTable

#linking to USCIE
uscie_meteo_mth <- merge(grid_meteo_mth, uscie_grid, by.x = "gridno", by.y = "GRIDNO", all.x = TRUE)
head(uscie_meteo_mth)


uscie_meteo_mth2 <- dcast(uscie_meteo_mth, s_uscierc ~ variable + month, drop=TRUE, value.var="value")  #to split the variables in columns containing yield/crp/year info
head(uscie_meteo_mth2)



## Running Function 3: Preparing data set to run function 

meteo_month <- func3(uscie_meteo_mth2)
head(meteo_month)

### Running Function1: 
# xavi: I couldn't find soil data at uscie level, so it only can be computed aggregating data at HSU level

vrbles <- names(uscie_meteo_mth2)[-1]
meteo_uscie_month_nuts <- agg2admins(meteo_month, data2ag = c("uscie"), functs = c("max", "min", "mean", "sd", "median"), vbles = vrbles, filenm4gdx = "meteo_uscie_month_stats", na.rm=TRUE)


save(meteo_uscie_month_nuts, file="soil_hsu_nuts2.rdata")



















#meteo
uscie.par <- read.csv(file="USCIE_PARAM.csv") #load csv file linking uscie and grid numbers, dataset coming from capri/dat/capdis/uscie
colnames(uscie.par)[1] <- "USCIE_RC"
#load(file="hsu_uscie.rdata")
#hsu_uscie1 <- hsu_uscie
hsu.met <- join(hsu_uscie1,uscie.par) #join to link new hsu and uscie parameters
save(hsu.met,file="hsu.met.rdata")
load(file="hsu.met.rdata")
tst <- dcast(hsu.met,GRIDNO~HSU2_IDRUN,drop=T,value.var = "USCIE_RC")
tst <- unique(t(apply(hsu.met1, 1, sort)))
tst <- unique(hsu.met[,c("GRIDNO","HSU2_IDRUN")])
hsu.met1 <- hsu.met[,c(2,4)]
mars.met.months <- rgdx.param("marsmeteo.gdx","p_marsmeteomonths") #load gdx file with mars meteo monthly data
mars.met.q <- rgdx.param("marsmeteo.gdx","p_marsmeteoquartals")#load gdx file with mars meteo quartals data
names(mars.met.months) <- c("GRIDNO","years","month","data","ave","value")
save(mars.met.months,file="mars.met.months.rdata")
#load(file="mars.met.months.rdata")
mars.met.months1 <- mars.met.months[,c(1,3,4,6)]
hsu.met1$GRIDNO <- as.factor(hsu.met1$GRIDNO)   
hsu.mars.met <- join(mars.met.months1,tst) #join to link mars meteo monthly data and new hsu 
hsu.mars.met1 <- na.omit(hsu.mars.met)
head(hsu.mars.met1)
save(hsu.mars.met1,file = "hsu.mars.met1.rdata")
load(file="hsu.mars.met1.rdata")
hsu.mars.met1$month <- mgsub(c("01","02","03","04","05","06","07","08","09","10","11","12"),c("jan","feb","mar","apr","may","jun","jul","ago","sep","oct","nov","dec"),hsu.mars.met1$month)
hsu.mars.met1$trimester <- gsub("jan|feb|mar","1st.trim",hsu.mars.met1$month)
hsu.mars.met1$trimester <- gsub("apr|may|jun","2nd.trim",hsu.mars.met1$trimester)
hsu.mars.met1$trimester <- gsub("jul|ago|sep","3rd.trim",hsu.mars.met1$trimester)
hsu.mars.met1$trimester <- gsub("oct|nov|dec","4th.trim",hsu.mars.met1$trimester)
hsu.mars.met1$month <- factor(hsu.mars.met1$month,levels=c("jan","feb","mar","apr","may","jun","jul","ago","sep","oct","nov","dec"))
#calculation of mean mars meteo data. Dataset has been divided in pieces of 10 millions entries to avoid memory overloads
hsu.mars.met2 <- hsu.mars.met1[1:10000000,]
hsu2.met.month1 <- ddply(hsu.mars.met2,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester1 <- ddply(hsu.mars.met2,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month1,file="hsu2.met.month1.rdata")
save(hsu2.met.trimester1,file="hsu2.met.trimester1.rdata")
hsu.mars.met3 <- hsu.mars.met1[10000001:20000000,]
hsu2.met.month2 <- ddply(hsu.mars.met3,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester2 <- ddply(hsu.mars.met3,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month2,file="hsu2.met.month2.rdata")
save(hsu2.met.trimester2,file="hsu2.met.trimester2.rdata")
hsu.mars.met4 <- hsu.mars.met1[20000001:30000000,]
hsu2.met.month3 <- ddply(hsu.mars.met4,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester3 <- ddply(hsu.mars.met4,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month3,file="hsu2.met.month3.rdata")
save(hsu2.met.trimester3,file="hsu2.met.trimester3.rdata")
gc()
hsu.mars.met5 <- hsu.mars.met1[30000001:40000000,]
hsu2.met.month4 <- ddply(hsu.mars.met5,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester4 <- ddply(hsu.mars.met5,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month4,file="hsu2.met.month4.rdata")
save(hsu2.met.trimester4,file="hsu2.met.trimester4.rdata")
gc()
hsu.mars.met6 <- hsu.mars.met1[40000001:50000000,]
hsu2.met.month5 <- ddply(hsu.mars.met6,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester5 <- ddply(hsu.mars.met6,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month5,file="hsu2.met.month5.rdata")
save(hsu2.met.trimester5,file="hsu2.met.trimester5.rdata")
gc()
hsu.mars.met7 <- hsu.mars.met1[50000001:60000000,]
hsu2.met.month6 <- ddply(hsu.mars.met7,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester6 <- ddply(hsu.mars.met7,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month6,file="hsu2.met.month6.rdata")
save(hsu2.met.trimester6,file="hsu2.met.trimester6.rdata")
gc()
hsu.mars.met8 <- hsu.mars.met1[60000001:70000000,]
hsu2.met.month7 <- ddply(hsu.mars.met8,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester7 <- ddply(hsu.mars.met8,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month7,file="hsu2.met.month7.rdata")
save(hsu2.met.trimester7,file="hsu2.met.trimester7.rdata")
gc()
hsu.mars.met9 <- hsu.mars.met1[70000001:80000000,]
hsu2.met.month8 <- ddply(hsu.mars.met9,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester8 <- ddply(hsu.mars.met9,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month8,file="hsu2.met.month8.rdata")
save(hsu2.met.trimester8,file="hsu2.met.trimester8.rdata")
gc()
hsu.mars.met10 <- hsu.mars.met1[80000001:86883490,]
hsu2.met.month9 <- ddply(hsu.mars.met10,c("HSU2_IDRUN","month","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
hsu2.met.trimester9 <- ddply(hsu.mars.met10,c("HSU2_IDRUN","trimester","data"),function(df)data.frame(mean2001_2011=round(mean(df$value,na.rm=T),2)))
save(hsu2.met.month9,file="hsu2.met.month9.rdata")
save(hsu2.met.trimester9,file="hsu2.met.trimester9.rdata")
gc()
load(file="hsu2.met.trimester1.rdata")
load(file="hsu2.met.trimester2.rdata")
load(file="hsu2.met.trimester3.rdata")
load(file="hsu2.met.trimester4.rdata")
load(file="hsu2.met.trimester5.rdata")
load(file="hsu2.met.trimester6.rdata")
load(file="hsu2.met.trimester7.rdata")
load(file="hsu2.met.trimester8.rdata")
load(file="hsu2.met.trimester9.rdata")
hsu2.met.trimester <- rbind(hsu2.met.trimester1,hsu2.met.trimester2,hsu2.met.trimester3,hsu2.met.trimester4,hsu2.met.trimester5,hsu2.met.trimester6,hsu2.met.trimester7,hsu2.met.trimester8,hsu2.met.trimester9)
save(hsu2.met.trimester,file="hsu2.met.trimester.rdata")
hsu2.met.month <- rbind(hsu2.met.month1,hsu2.met.month2,hsu2.met.month3,hsu2.met.month4,hsu2.met.month5,hsu2.met.month6,hsu2.met.month7,hsu2.met.month8,hsu2.met.month9)
save(hsu2.met.month,file="hsu2.met.month.rdata")
load(file="hsu2.met.month.rdata")
colnames(hsu2.met.month)[1] <- "Spatial_unit"
hsu2.met.month$Spatial_unit <- paste0("U",hsu2.met.month$Spatial_unit,sep="")
hsu2.month.nuts <- join(hsu2.met.month,hsu2_nuts)
hsu2.month.nuts$hsu2 <- as.numeric(as.character(hsu2.month.nuts$hsu2))
month.nuts3 <-ddply(hsu2.month.nuts,c("nuts3","month","data"),function(df)data.frame(min.hsu2=min(df$hsu2),max.hsu2=max(df$hsu2),mean.2001.2011=weighted.mean(df$mean2001_2011,df$area)))
colnames(month.nuts3)[1] <- "Nuts"
month.nuts2 <-ddply(hsu2.month.nuts,c("nuts2","month","data"),function(df)data.frame(min.hsu2=min(df$hsu2),max.hsu2=max(df$hsu2),mean.2001.2011=weighted.mean(df$mean2001_2011,df$area)))
colnames(month.nuts2)[1] <- "Nuts"
month.caprinuts <-ddply(hsu2.month.nuts,c("CAPRI_NUTSII","month","data"),function(df)data.frame(min.hsu2=min(df$hsu2),max.hsu2=max(df$hsu2),mean.2001.2011=weighted.mean(df$mean2001_2011,df$area)))
colnames(month.caprinuts)[1] <- "Nuts"
meteo.month.nuts <- rbind(month.nuts3,month.nuts2,month.caprinuts)
meteo.month.nuts1 <- meteo.month.nuts[!is.na(meteo.month.nuts$Nuts),]
save(meteo.month.nuts1,file = "meteo.month.nuts.rdata")
load(file = "meteo.month.nuts.rdata")
colnames(meteo.month.nuts1)[1] <- "Spatial_unit"
colnames(meteo.month.nuts1)[6] <- "mean2001_2011"
meteo.month.hsu.nuts <- rbind(hsu2.met.month,meteo.month.nuts1[,c(1:3,6)])
save(meteo.month.hsu.nuts,file = "meteo.month.hsu.nuts.rdata")

load(file="hsu2.met.trimester.rdata")
colnames(hsu2.met.trimester)[1] <- "Spatial_unit"
hsu2.met.trimester$Spatial_unit <- paste0("U",hsu2.met.trimester$Spatial_unit,sep="")
hsu2.trimester.nuts <- join(hsu2.met.trimester,hsu2_nuts)
hsu2.trimester.nuts$hsu2 <- as.numeric(as.character(hsu2.trimester.nuts$hsu2))
trimester.nuts3 <-ddply(hsu2.trimester.nuts,c("nuts3","trimester","data"),function(df)data.frame(min.hsu2=min(df$hsu2),max.hsu2=max(df$hsu2),mean.2001.2011=weighted.mean(df$mean2001_2011,df$area)))
colnames(trimester.nuts3)[1] <- "Nuts"
trimester.nuts2 <-ddply(hsu2.trimester.nuts,c("nuts2","trimester","data"),function(df)data.frame(min.hsu2=min(df$hsu2),max.hsu2=max(df$hsu2),mean.2001.2011=weighted.mean(df$mean2001_2011,df$area)))
colnames(trimester.nuts2)[1] <- "Nuts"
trimester.caprinuts <-ddply(hsu2.trimester.nuts,c("CAPRI_NUTSII","trimester","data"),function(df)data.frame(min.hsu2=min(df$hsu2),max.hsu2=max(df$hsu2),mean.2001.2011=weighted.mean(df$mean2001_2011,df$area)))
colnames(trimester.caprinuts)[1] <- "Nuts"
meteo.trimester.nuts <- rbind(trimester.nuts3,trimester.nuts2,trimester.caprinuts)
meteo.trimester.nuts1 <- meteo.trimester.nuts[!is.na(meteo.trimester.nuts$Nuts),]
save(meteo.trimester.nuts1,file = "meteo.trimester.nuts.rdata")
load(file = "meteo.trimester.nuts.rdata")
colnames(meteo.trimester.nuts1)[1] <- "Spatial_unit"
colnames(meteo.trimester.nuts1)[6] <- "mean2001_2011"
meteo.trimester.hsu.nuts <- rbind(hsu2.met.trimester,meteo.trimester.nuts1[,c(1:3,6)])
save(meteo.trimester.hsu.nuts,file = "meteo.trimester.hsu.nuts.rdata")

#creation of new gdx files ofr monthly and trimestral mars meteo data
load(file = "meteo.month.hsu.nuts.rdata")
colnames(meteo.month.hsu.nuts)[2] <- "m_months"
colnames(meteo.month.hsu.nuts)[3] <- "s_meteopars"
meteo.month.hsu.nuts$s_meteopars <- factor(meteo.month.hsu.nuts$s_meteopars,levels = c("prec","Tav","Tmin","Tmax","precday","precmax","Tgt8days","Tsum8mars","Tsum8", "VegP","E0","ET0"))
symDim <- 4
ciao <- na.omit(meteo.month.hsu.nuts)
attr(ciao,"symName") <- "p_marsmeteo"
lst <- wgdx.reshape(ciao,symDim,tName = "data")
wgdx.lst("marsmeteo.months.hsu.nuts.gdx",lst)

load(file = "meteo.trimester.hsu.nuts.rdata")
colnames(meteo.trimester.hsu.nuts)[2] <- "m_trimester"
colnames(meteo.trimester.hsu.nuts)[3] <- "s_meteopars"
meteo.trimester.hsu.nuts$s_meteopars <- factor(meteo.trimester.hsu.nuts$s_meteopars,levels = c("prec","Tav","Tmin","Tmax","precday","precmax","Tgt8days","Tsum8mars","Tsum8", "VegP","E0","ET0"))
meteo.trimester.hsu.nuts$m_trimester <- factor(meteo.trimester.hsu.nuts$m_trimester,levels = c("1st.trim","2nd.trim","3rd.trim","4th.trim"))
symDim <- 4
ciao <- na.omit(meteo.trimester.hsu.nuts)
attr(ciao,"symName") <- "p_marsmeteo"
lst <- wgdx.reshape(ciao,symDim,tName = "data")
wgdx.lst("marsmeteo.trimester.hsu.nuts.gdx",lst)



#### WET DEPOSITION FOR OXN, RDN, and SOX ####

#memory.limit(size = 2048000)

# WET DEPOSITION FOR OXN
deposition<-function(depfile,deptype,tser){
    #depfile: Deposition file including path in csv format
    #deptype: Column-header of deposition values (year-independent part)
    #tser: Years for which deposition data are avaialble (part of column header)
    dep<-fread(depfile,header=TRUE,select=c("USCIE_RC","i50_j50","HSU2_IDRUN","LAND_SEA",paste0(deptype,tser)))
    setkey(dep,USCIE_RC) # to set a key column of the DataTable
    names(dep)[1] <- "s_uscierc"
    return(dep)
}
tser<-c("06","07","08","09","10")
dep<-deposition(paste0(usciedatapath,"USCIE_EMEP_HSU2_CTRY_WDEP_OXN.csv"),deptype="WDON",tser=tser)
dep<-deposition(paste0("test.csv"),deptype="WDON",tser=tser)

wdep_oxn_uscie <- wdep.oxn[, c(1, 7:11), with=FALSE]
remove(wdep.oxn)
#No need to dcast, it's already in "wide form"
# Running Function 3: Preparing data set to run function 1
wdep_oxn <- func3(wdep_oxn_uscie)
remove(wdep_oxn_uscie)
# Running Function1: 
# Statistics can be computed aggregating data either by USCIE or by HSU2
# By USCIE
wdep_oxn_uscie_nuts <- agg2admins(wdep_oxn, data2ag = c("uscie"), functs = c("max", "min", "mean", "sd", "median"), vbles = c("WDON06", "WDON07", "WDON08", "WDON09", "WDON10"), filenm4gdx = "wdep_oxn_uscie_stats", na.rm=TRUE)

# By HSU2
wdep_oxn_hsu_nuts <- agg2admins(wdep_oxn, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("WDON06", "WDON07", "WDON08", "WDON09", "WDON10"), filenm4gdx = "wdep_oxn_hsu_stats", na.rm=TRUE)
save(wdep_oxn_uscie_nuts, file="wdep_oxn_uscie_nuts.rdata")
save(wdep_oxn_hsu_nuts, file="wdep_oxn_hsu_nuts.rdata")

#remove(wdep_oxn)
remove(wdep_oxn_uscie_nuts)
remove(wdep_oxn_hsu_nuts)
gc()

# WET DEPOSITION FOR RDN
wdep.rdn <- fread("USCIE_EMEP_HSU2_CTRY_WDEP_RDN.csv") #load csv file linking uscie and RDN wet deposition, dataset coming from capri/dat/capdis/uscie
setkey(wdep.rdn, "USCIE_RC") # to set a key column of the DataTable
wdep_rdn_uscie <- wdep.rdn[, c(1, 7:11), with=FALSE]
names(wdep_rdn_uscie)[1] <- "s_uscierc"
remove(wdep.rdn)
gc()
#No need to dcast, it's already in "wide form"
# Running Function 3: Preparing data set to run function 1
wdep_rdn <- func3(wdep_rdn_uscie)
remove(wdep_rdn_uscie)
gc()
# Running Function1: 
# Statistics can be computed aggregating data either by USCIE or by HSU2
# By USCIE
wdep_rdn_uscie_nuts <- agg2admins(wdep_rdn, data2ag = c("uscie"), functs = c("max", "min", "mean", "sd", "median"), vbles = c("WDRN06", "WDRN07", "WDRN08", "WDRN09", "WDRN10"), filenm4gdx = "wdep_rdn_uscie_stats", na.rm=TRUE)
# By HSU2
wdep_rdn_hsu_nuts <- agg2admins(wdep_rdn, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("WDRN06", "WDRN07", "WDRN08", "WDRN09", "WDRN10"), filenm4gdx = "wdep_rdn_hsu_stats", na.rm=TRUE)
save(wdep_rdn_uscie_nuts, file="wdep_rdn_uscie_nuts.rdata")
save(wdep_rdn_hsu_nuts, file="wdep_rdn_hsu_nuts.rdata")

#remove(wdep_rdn)
remove(wdep_rdn_uscie_nuts)
remove(wdep_rdn_hsu_nuts)
gc()





# WET DEPOSITION FOR SOX

wdep.sox <- fread("USCIE_EMEP_HSU2_CTRY_WDEP_SOX.csv") #load csv file linking uscie and SOX wet deposition, dataset coming from capri/dat/capdis/uscie
setkey(wdep.sox, "USCIE_RC") # to set a key column of the DataTable

wdep_sox_uscie <- wdep.sox[, c(1, 7:11), with=FALSE]
names(wdep_sox_uscie)[1] <- "s_uscierc"
remove(wdep.sox)
gc()
#No need to dcast, it's already in "wide form"

# Running Function 3: Preparing data set to run function 1

wdep_sox <- func3(wdep_sox_uscie)
remove(wdep_sox_uscie)
gc()

# Running Function1: 

# Statistics can be computed aggregating data either by USCIE or by HSU2
# By USCIE
wdep_sox_uscie_nuts <- agg2admins(wdep_sox, data2ag = c("uscie"), functs = c("max", "min", "mean", "sd", "median"), vbles = c("WDOS06", "WDOS07", "WDOS08", "WDOS09", "WDOS10"), filenm4gdx = "wdep_sox_uscie_stats", na.rm=TRUE)
# By HSU2
wdep_sox_hsu_nuts <- agg2admins(wdep_sox, data2ag = c("hsu"), functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("WDOS06", "WDOS07", "WDOS08", "WDOS09", "WDOS10"), filenm4gdx = "wdep_sox_hsu_stats", na.rm=TRUE)


save(wdep_sox_uscie_nuts, file="wdep_sox_uscie_nuts.rdata")
save(wdep_sox_hsu_nuts, file="wdep_sox_hsu_nuts.rdata")

remove(wdep_sox)
remove(wdep_sox_uscie_nuts)
remove(wdep_sox_hsu_nuts)
gc()












#all wet deposition
load(file = "wdep.oxn.hsu.nuts.rdata")
wdep.oxn.hsu.nuts$element <- "OxN"
colnames(wdep.oxn.hsu.nuts)[2:6] <- c("DD06","DD07","DD08","DD09","DD10")
load(file = "wdep.rdn.hsu.nuts.rdata")
wdep.rdn.hsu.nuts$element <- "RdN"
colnames(wdep.rdn.hsu.nuts)[2:6] <- c("DD06","DD07","DD08","DD09","DD10")
load(file = "wdep.sox.hsu.nuts.rdata")
wdep.sox.hsu.nuts$element <- "SoX"
colnames(wdep.sox.hsu.nuts)[2:6] <- c("DD06","DD07","DD08","DD09","DD10")
wdep.hsu.nuts <- rbind(wdep.oxn.hsu.nuts,wdep.rdn.hsu.nuts,wdep.sox.hsu.nuts)
save(wdep.hsu.nuts,file = "wdep.hsu.nuts.rdata")
ciao <- wdep.hsu.nuts
symDim <- 3 
attr(ciao,"symName") <- "p_wdep_nuts3_2_1_0"
lst <- wgdx.reshape(ciao,symDim,order=c(1,7,-1),tName="data")
wgdx.lst("wdep.gdx",lst)

#Dry deposition OXN
ddep.oxn <- read.csv(file = "USCIE_EMEP_LC_HSU2_CTRY_DDEP_OXN.csv") #load csv file linking uscie and OXN dry deposition, dataset coming from capri/dat/capdis/uscie
ddep.oxn1 <- ddep.oxn[,c(3,4,9:13)]
colnames(ddep.oxn1)[1:2] <- c("hsu2","nuts3")
save(ddep.oxn1,file = "ddep.oxn.rdata")
load(file = "ddep.oxn.rdata")
hsu2_nuts1 <- hsu2_nuts[,c(1,2,3:6)]
hsu2_nuts1$hsu2 <- as.factor(hsu2_nuts1$hsu2)
ddep.oxn.nuts <- join(ddep.oxn1,hsu2_nuts1)
ddep.oxn.hsu2 <- ddply(ddep.oxn.nuts,"hsu2",function(df)data.frame(DDON06=weighted.mean(df$DDON06,df$area),DDON07=weighted.mean(df$DDON07,df$area),DDON08=weighted.mean(df$DDON08,df$area),DDON09=weighted.mean(df$DDON09,df$area),DDON10=weighted.mean(df$DDON10,df$area)))
colnames(ddep.oxn.hsu2)[1] <- "Spatial_unit"
ddep.oxn.hsu2$Spatial_unit <- paste0("U",ddep.oxn.hsu2$Spatial_unit,sep="")
ddep.oxn.nuts3 <- ddply(ddep.oxn.nuts,"nuts3",function(df)data.frame(DDON06=mean(df$DDON06),DDON07=mean(df$DDON07),DDON08=mean(df$DDON08),DDON09=mean(df$DDON09),DDON10=mean(df$DDON10)))
colnames(ddep.oxn.nuts3)[1]="Spatial_unit"
ddep.oxn.nuts2 <- ddply(na.omit(ddep.oxn.nuts),"nuts2",function(df)data.frame(DDON06=mean(df$DDON06),DDON07=mean(df$DDON07),DDON08=mean(df$DDON08),DDON09=mean(df$DDON09),DDON10=mean(df$DDON10)))
colnames(ddep.oxn.nuts2)[1]="Spatial_unit"
ddep.oxn.caprinuts <- ddply(na.omit(ddep.oxn.nuts),"CAPRI_NUTSII",function(df)data.frame(DDON06=mean(df$DDON06),DDON07=mean(df$DDON07),DDON08=mean(df$DDON08),DDON09=mean(df$DDON09),DDON10=mean(df$DDON10)))
colnames(ddep.oxn.caprinuts)[1]="Spatial_unit"
ddep.oxn.hsu.nuts <- rbind(ddep.oxn.hsu2,ddep.oxn.nuts3,ddep.oxn.nuts2,ddep.oxn.caprinuts)
save(ddep.oxn.hsu.nuts,file = "ddep.oxn.hsu.nuts.rdata")

#Dry deposition RDN
ddep.rdn <- read.csv(file = "USCIE_EMEP_LC_HSU2_CTRY_DDEP_RDN.csv") #load csv file linking uscie and RDN dry deposition, dataset coming from capri/dat/capdis/uscie
ddep.rdn1 <- ddep.rdn[,c(3,4,9:13)]
colnames(ddep.rdn1)[1:2] <- c("hsu2","nuts3")
save(ddep.rdn1,file = "ddep.rdn.rdata")
load(file = "ddep.rdn.rdata")
hsu2_nuts1 <- hsu2_nuts[,c(1,2,3:6)]
hsu2_nuts1$hsu2 <- as.factor(hsu2_nuts1$hsu2)
ddep.rdn.nuts <- join(ddep.rdn1,hsu2_nuts1)
ddep.rdn.hsu2 <- ddply(ddep.rdn.nuts,"hsu2",function(df)data.frame(DDRN06=weighted.mean(df$DDRN06,df$area),DDRN07=weighted.mean(df$DDRN07,df$area),DDRN08=weighted.mean(df$DDRN08,df$area),DDRN09=weighted.mean(df$DDRN09,df$area),DDRN10=weighted.mean(df$DDRN10,df$area)))
colnames(ddep.rdn.hsu2)[1] <- "Spatial_unit"
ddep.rdn.hsu2$Spatial_unit <- paste0("U",ddep.rdn.hsu2$Spatial_unit,sep="")
ddep.rdn.nuts3 <- ddply(ddep.rdn.nuts,"nuts3",function(df)data.frame(DDRN06=mean(df$DDRN06),DDRN07=mean(df$DDRN07),DDRN08=mean(df$DDRN08),DDRN09=mean(df$DDRN09),DDRN10=mean(df$DDRN10)))
colnames(ddep.rdn.nuts3)[1]="Spatial_unit"
ddep.rdn.nuts2 <- ddply(na.omit(ddep.rdn.nuts),"nuts2",function(df)data.frame(DDRN06=mean(df$DDRN06),DDRN07=mean(df$DDRN07),DDRN08=mean(df$DDRN08),DDRN09=mean(df$DDRN09),DDRN10=mean(df$DDRN10)))
colnames(ddep.rdn.nuts2)[1]="Spatial_unit"
ddep.rdn.caprinuts <- ddply(na.omit(ddep.rdn.nuts),"CAPRI_NUTSII",function(df)data.frame(DDRN06=mean(df$DDRN06),DDRN07=mean(df$DDRN07),DDRN08=mean(df$DDRN08),DDRN09=mean(df$DDRN09),DDRN10=mean(df$DDRN10)))
colnames(ddep.rdn.caprinuts)[1]="Spatial_unit"
ddep.rdn.hsu.nuts <- rbind(ddep.rdn.hsu2,ddep.rdn.nuts3,ddep.rdn.nuts2,ddep.rdn.caprinuts)
save(ddep.rdn.hsu.nuts,file = "ddep.rdn.hsu.nuts.rdata")

#Dry deposition SOX
ddep.sox <- read.csv(file = "USCIE_EMEP_LC_HSU2_CTRY_DDEP_SOX.csv") #load csv file linking uscie and SOX dry deposition, dataset coming from capri/dat/capdis/uscie
ddep.sox1 <- ddep.sox[,c(3,4,9:13)]
colnames(ddep.sox1)[1:2] <- c("hsu2","nuts3")
save(ddep.sox1,file = "ddep.sox.rdata")
load(file = "ddep.sox.rdata")
hsu2_nuts1 <- hsu2_nuts[,c(1,2,3:6)]
hsu2_nuts1$hsu2 <- as.factor(hsu2_nuts1$hsu2)
ddep.sox.nuts <- join(ddep.sox1,hsu2_nuts1)
ddep.sox.hsu2 <- ddply(ddep.sox.nuts,"hsu2",function(df)data.frame(DDOS06=weighted.mean(df$DDOS06,df$area),DDOS07=weighted.mean(df$DDOS07,df$area),DDOS08=weighted.mean(df$DDOS08,df$area),DDOS09=weighted.mean(df$DDOS09,df$area),DDOS10=weighted.mean(df$DDOS10,df$area)))
colnames(ddep.sox.hsu2)[1] <- "Spatial_unit"
ddep.sox.hsu2$Spatial_unit <- paste0("U",ddep.sox.hsu2$Spatial_unit,sep="")
ddep.sox.nuts3 <- ddply(ddep.sox.nuts,"nuts3",function(df)data.frame(DDOS06=mean(df$DDOS06),DDOS07=mean(df$DDOS07),DDOS08=mean(df$DDOS08),DDOS09=mean(df$DDOS09),DDOS10=mean(df$DDOS10)))
colnames(ddep.sox.nuts3)[1]="Spatial_unit"
ddep.sox.nuts2 <- ddply(na.omit(ddep.sox.nuts),"nuts2",function(df)data.frame(DDOS06=mean(df$DDOS06),DDOS07=mean(df$DDOS07),DDOS08=mean(df$DDOS08),DDOS09=mean(df$DDOS09),DDOS10=mean(df$DDOS10)))
colnames(ddep.sox.nuts2)[1]="Spatial_unit"
ddep.sox.caprinuts <- ddply(na.omit(ddep.sox.nuts),"CAPRI_NUTSII",function(df)data.frame(DDOS06=mean(df$DDOS06),DDOS07=mean(df$DDOS07),DDOS08=mean(df$DDOS08),DDOS09=mean(df$DDOS09),DDOS10=mean(df$DDOS10)))
colnames(ddep.sox.caprinuts)[1]="Spatial_unit"
ddep.sox.hsu.nuts <- rbind(ddep.sox.hsu2,ddep.sox.nuts3,ddep.sox.nuts2,ddep.sox.caprinuts)
save(ddep.sox.hsu.nuts,file = "ddep.sox.hsu.nuts.rdata")

#all dry deposition
load(file = "ddep.oxn.hsu.nuts.rdata")
ddep.oxn.hsu.nuts$element <- "OxN"
colnames(ddep.oxn.hsu.nuts)[2:6] <- c("DD06","DD07","DD08","DD09","DD10")
load(file = "ddep.rdn.hsu.nuts.rdata")
ddep.rdn.hsu.nuts$element <- "RdN"
colnames(ddep.rdn.hsu.nuts)[2:6] <- c("DD06","DD07","DD08","DD09","DD10")
load(file = "ddep.sox.hsu.nuts.rdata")
ddep.sox.hsu.nuts$element <- "SoX"
colnames(ddep.sox.hsu.nuts)[2:6] <- c("DD06","DD07","DD08","DD09","DD10")
ddep.hsu.nuts <- rbind(ddep.oxn.hsu.nuts,ddep.rdn.hsu.nuts,ddep.sox.hsu.nuts)
save(ddep.hsu.nuts,file = "ddep.hsu.nuts.rdata")
ciao <- ddep.hsu.nuts
symDim <- 3 
attr(ciao,"symName") <- "p_ddep_nuts3_2_1_0"
lst <- wgdx.reshape(ciao,symDim,order=c(1,7,-1),tName="data")
wgdx.lst("ddep.gdx",lst)

#Lucas and updated hsu
lucas <- read.csv(file = "LUCAS12EU27_USCIERC.csv")
lucas1 <- lucas[,c(1,2,3,4,8:10,27,28,30,31,32:35,40,47)]
colnames(hsu_uscie1)[2] <- "hsu2"
lucas1.hsu2 <- join(lucas1,hsu_uscie1)
lucas.nuts <- join(lucas1.hsu2,hsu2_nuts)
lucas.nuts1 <- lucas.nuts[,c(1:6,8:18,20:22)]
lucas.hsu <- ddply(lucas.nuts1,"hsu2",function(df)
  data.frame(data.frame(POINT_ID=df$POINT_ID,X_LAEA=df$X_LAEA,Y_LAEA=df$Y_LAEA,
                        USCIE_RC=df$USCIE_RC,NUTS0=df$NUTS0,NUTS1=df$NUTS1,LC1=df$LC1,
                        LC2=df$LC2,LU1=df$LU1,LU2=df$LU2,LC1_SPEC=df$LC1_SPEC,
                        LC1_PERC=df$LC1_PERC,LC2_SPEC=df$LC2_SPEC,LC2_PERC=df$LC2_PERC,
                        LAND_MNGT=df$LAND_MNGT,SOIL_SURV=df$SOIL_SURV)))
colnames(lucas.hsu)[1] <- "Spatial_unit"
lucas.hsu$Spatial_unit <- paste0("U",lucas.hsu$Spatial_unit,sep="")
lucas.nuts3 <- ddply(lucas.nuts1,"nuts3",function(df)
  data.frame(POINT_ID=df$POINT_ID,X_LAEA=df$X_LAEA,Y_LAEA=df$Y_LAEA,
             USCIE_RC=df$USCIE_RC,NUTS0=df$NUTS0,NUTS1=df$NUTS1,LC1=df$LC1,
             LC2=df$LC2,LU1=df$LU1,LU2=df$LU2,LC1_SPEC=df$LC1_SPEC,
             LC1_PERC=df$LC1_PERC,LC2_SPEC=df$LC2_SPEC,LC2_PERC=df$LC2_PERC,
             LAND_MNGT=df$LAND_MNGT,SOIL_SURV=df$SOIL_SURV))
colnames(lucas.nuts3)[1] <- "Spatial_unit"
lucas.nuts2 <- ddply(lucas.nuts1[!is.na(lucas.nuts1$nuts2),],"nuts2",function(df)
  data.frame(POINT_ID=df$POINT_ID,X_LAEA=df$X_LAEA,Y_LAEA=df$Y_LAEA,
             USCIE_RC=df$USCIE_RC,NUTS0=df$NUTS0,NUTS1=df$NUTS1,LC1=df$LC1,
             LC2=df$LC2,LU1=df$LU1,LU2=df$LU2,LC1_SPEC=df$LC1_SPEC,
             LC1_PERC=df$LC1_PERC,LC2_SPEC=df$LC2_SPEC,LC2_PERC=df$LC2_PERC,
             LAND_MNGT=df$LAND_MNGT,SOIL_SURV=df$SOIL_SURV))
colnames(lucas.nuts2)[1] <- "Spatial_unit"
lucas.caprinuts <- ddply(lucas.nuts1[!is.na(lucas.nuts1$CAPRI_NUTSII),],"CAPRI_NUTSII",function(df)
  data.frame(POINT_ID=df$POINT_ID,X_LAEA=df$X_LAEA,Y_LAEA=df$Y_LAEA,
             USCIE_RC=df$USCIE_RC,NUTS0=df$NUTS0,NUTS1=df$NUTS1,LC1=df$LC1,
             LC2=df$LC2,LU1=df$LU1,LU2=df$LU2,LC1_SPEC=df$LC1_SPEC,
             LC1_PERC=df$LC1_PERC,LC2_SPEC=df$LC2_SPEC,LC2_PERC=df$LC2_PERC,
             LAND_MNGT=df$LAND_MNGT,SOIL_SURV=df$SOIL_SURV))
colnames(lucas.caprinuts)[1] <- "Spatial_unit"
lucas.hsu.nuts <- rbind(lucas.hsu,lucas.nuts3,lucas.nuts2,lucas.caprinuts)
lucas.hsu.nuts1 <- lucas.hsu.nuts[!is.na(lucas.hsu.nuts$Spatial_unit),]
save(lucas.hsu.nuts1,file = "lucas.hsu.nuts1.rdata") 
load("lucas.hsu.nuts1.rdata")
ciao <- lucas.hsu.nuts1
ciao$LC2 <- as.numeric(ciao$LC2)
symDim <- 2 
attr(ciao,"symName") <- "p_lucas_nuts"
lst <- wgdx.reshape(ciao,symDim,order=c(1,6,7,10,11,-1),tName="data")
#lst <- wgdx.reshape(ciao,symDim,order=c(1,6,7,10,11,-1),tName="data")
wgdx.lst("lucas.hsu.nuts.gdx",lst)




#gisconuts
gis_nuts <- read.csv(file="LINK_HSU2_CAPRI_GISCO2010_NUTS_CODES_uniqueGISCO2010.csv") #load of new hsu and gisconuts
gis.nuts <- gis_nuts[,c(2,4,6,8,11,13,14)]
colnames(gis.nuts)[5] <- "nuts3"
gis.hsu2_nuts <- na.omit(merge(hsu2_nuts,gis.nuts))
gis.hsu2_nuts <- gis.hsu2_nuts[,-5]
save(gis.hsu2_nuts,file="gis.hsu2_nuts.rdata")
load(file="gis.hsu2_nuts.rdata")
gis.hsu2_nuts1 <- gis.hsu2_nuts
colnames(gis.hsu2_nuts1)[6:9] <- c("s_gisco2010nuts0","s_gisco2010nuts1","s_gisco2010nuts2","s_gisco2010nuts3")
gis.hsu2 <- gis.hsu2_nuts1[,c(4,6:9)]
colnames(gis.hsu2)[1] <- "Spatial_unit"
gis.hsu2$Spatial_unit <- paste0("U",gis.hsu2$Spatial_unit,sep="")
gis.nuts3 <- gis.hsu2_nuts1[,c(1,6:9)]
colnames(gis.nuts3)[1] <- "Spatial_unit"
gis.nuts2 <- gis.hsu2_nuts1[,c(5,6:9)]
colnames(gis.nuts2)[1] <- "Spatial_unit"
gis.capri.nuts2 <- gis.hsu2_nuts1[,c(2,6:9)]
colnames(gis.capri.nuts2)[1] <- "Spatial_unit"
ciao <- rbind(gis.hsu2,gis.nuts3,gis.nuts2,gis.capri.nuts2)
ciao$not_to_use <- "1"
ciao$not_to_use <- as.numeric(ciao$not_to_use)
symDim <- 6 
attr(ciao,"symName") <- "m_gisco2010_nuts3_2_1_0"
lst <- wgdx.reshape(ciao,symDim,tName="not_to_use")
wgdx.lst("gisco.hsu.nuts.gdx",lst)


