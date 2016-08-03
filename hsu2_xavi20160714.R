rm(list=objects())
source("hsu4capri_header.r")


###### Input 3: Data to be computed #####

# It has to be done outside the function because each original data set has different number of columns. It would do the function too much complex


#### FOREST SHARE ####
gdxInfo("forestshare.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
adf <- rgdx.param("forestshare.gdx","p_forshares") #load of gdx linking forest shares and USCIE numbers, dataset coming from capri/dat/capdis/hsu2
names(adf) <- c("s_uscierc","s_years","value")
adfm <- dcast(adf, s_uscierc ~ s_years, drop=TRUE, value.var="value")  #to split the years in two columns containing forshare info

#from here to 'endfun3' into new function
adfm$s_uscierc <- as.numeric(as.character(adfm$s_uscierc)) # to transform the column to numeric
adfm2 <- as.data.table(adfm) # to transform adfm to a DataTable
setkey(adfm2, "s_uscierc") # to set a key column
#str(adfm2)
#tables()  # to see info of the tables in memory
#remove(adfm)
#gc()

usciehsu_forshare2 <- merge(adfm2, hsu_uscie1, by="s_uscierc", all.x=TRUE) #to join of old gdx file with new hsu. Type of joining "left"
usciehsu_forshare2$s_uscierc <- factor(usciehsu_forshare2$s_uscierc) # to transform the column to factor
usciehsu_forshare2$s_hsu2 <- factor(usciehsu_forshare2$s_hsu2) # to transform the column to factor
setkey(usciehsu_forshare2, "s_hsu2") # to set a key column

#Put ealier before new function3
setnames(usciehsu_forshare2, old = c("2000","2006"), new = c("f_2000", "f_2006")) # to change colnames
#str(usciehsu_forshare2)

forshare2 <- merge(usciehsu_forshare2, hsu2.nutsDT, by.y="hsu2", by.x="s_hsu2", all.x=TRUE)
setnames(forshare2,old="s_hsu2",new="hsu2")
#endfun3

#head(forshare2)
#str(forshare2)

#### ####

## Running Function 2: Computing statistis HSU-USCIE
# how many USCIES per HSU (max, min, mean...)
stats.hsu.uscie <- func2(forshare2)
#head(stats.hsu.uscie)

### Running Function1: 
#to check for minimum
forest.hsu.nuts2 <- func1(forshare2, functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("f_2000", "f_2006"), na.rm=TRUE)


#head(forest.hsu.nuts2)
#str(forest.hsu.nuts2)


#remove(forest.hsu.nuts2)


save(forest.hsu.nuts2, file="forest.hsu.nuts2.rdata")
#load(file = "forest.hsu.nuts2.rdata")






#### DIGITAL ELEVATION MODEL ####



gdxInfo("uscie_dem.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
uscie.dem <- rgdx.param("uscie_dem.gdx","p_uscierc_dem") #load gdx file linking uscie and dem, dataset coming from capri/dat/capdis/uscie
colnames(uscie.dem)[1] <- "s_uscierc"
colnames(uscie.dem)[3] <- "variables" 
dem <- dcast(uscie.dem, s_uscierc ~ variables, drop=TRUE, value.var="value")  #to split the variables in the data set (altitude_m, slope_perc) in two different columns 
dem$s_uscierc <- as.numeric(as.character(dem$s_uscierc)) # to transform the column to numeric
dem2 <- as.data.table(dem) # to transform dem to a DataTable
setkey(dem2, "s_uscierc") # to set a key column
#str(dem2)
#remove(dem)
#gc()
#head(dem2)

usciehsu_dem <- merge(dem2, hsu_uscie1, by="s_uscierc", all.x=TRUE) #to join of old gdx file with new hsu. Type of joining "left"
head(usciehsu_dem)

usciehsu_dem$s_uscierc <- factor(usciehsu_dem$s_uscierc) # to transform the column to factor
usciehsu_dem$s_hsu2 <- factor(usciehsu_dem$s_hsu2) # to transform the column to factor
setkey(usciehsu_dem, "s_hsu2") # to set a key column
#str(usciehsu_dem)

DEM3 <- merge(usciehsu_dem, hsu2.nutsDT, by.y="hsu2", by.x="s_hsu2", all.x=TRUE)
setnames(DEM3,old="s_hsu2",new="hsu2")
#head(DEM3)
#str(DEM3)



### Running Function1: 

dem.hsu.nuts2 <- func1(DEM3, functs = c("max", "min", "weighted.mean", "sd", "median"), vbles = c("altitude_m", "slope_perc"), na.rm=TRUE)

#head(dem.hsu.nuts2)
#str(dem.hsu.nuts2)
#sum(is.na(dem.hsu.nuts2$Spatial_unit))  # there are 2 NA
#dem.hsu.nuts3 <- dem.hsu.nuts2[!is.na(dem.hsu.nuts2$Spatial_unit),]  # to remove NA
#dem.hsu.nuts3 <- droplevels(dem.hsu.nuts3) # to definetly remove them

#remove(prova_dem.hsu.nuts2)

save(dem.hsu.nuts2, file="dem.hsu.nuts2.rdata")
#load(file = "dem.hsu.nuts2.rdata")
#head(dem.hsu.nuts2)








#### SOIL ####

gdxInfo("soil_hwsd.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file
gdxInfo("hsu_hsmu.gdx", dump=FALSE, returnList=FALSE, returnDF=TRUE) # to get info of the gdx file


adf <- rgdx.param("soil_hwsd.gdx","p_soildom") #load of gdx linking forest shares and USCIE numbers, dataset coming from capri/dat/capdis/hsu2
names(adf) <- c("s_uscierc","s_years","value")
adfm <- dcast(adf, s_uscierc ~ s_years, drop=TRUE, value.var="value")  #to split the years in two columns containing forshare info
adfm$s_uscierc <- as.numeric(as.character(adfm$s_uscierc)) # to transform the column to numeric
adfm2 <- as.data.table(adfm) # to transform adfm to a DataTable
setkey(adfm2, "s_uscierc") # to set a key column
#str(adfm2)
#tables()  # to see info of the tables in memory
#remove(adfm)
#gc()



gdx.s <- rgdx.param("soil_hwsd.gdx","p_soildom") #load of gdx linking soil properties and old hsu, dataset coming from capri/dat/capdis/hsu2
gdx.sm <- dcast(gdx.s,i~j,drop=T,value.var="value")
hsu2.soil <- gdx.sm
save(hsu2.soil,file="hsu_soil.rdata")
load(file="hsu_soil.rdata")

hsuhsmu <- rgdx.param("hsu_hsmu.gdx","p_hsu_hsmu")#load of gdx linking old and new hsu,dataset coming from capri/dat/capdis/hsu2
names(hsuhsmu) <- c("hsu2","hsmu")
hsuhsmu$hsu2 <- gsub("U","",hsuhsmu$hsu2)
hsuhsmu$hsmu <- gsub("H|U|HE","",hsuhsmu$hsmu)
hsuhsmu1 <- hsuhsmu[,1:2]
colnames(hsu2.soil)[1] <- "hsmu"
hsu2.soil$hsmu <- gsub("U","",hsu2.soil$hsmu)

hsu2soil <- join(hsuhsmu1,hsu2.soil) #join to link new hsu and soil properties
hsu2soil1 <- hsu2soil[,c(1,3:17)]
hsu.nuts.soil <- join(hsu2soil1,hsu2.nuts) #join to link soil database with nuts

#calculation of weighted mean (by area) soil properties value per hsu
hsu2.soilprop <-ddply(hsu.nuts.soil[,1:17],c("hsu2"),function(df){
  numcolwise(weighted.mean)(df, na.rm = T, w=df$area)
  numcolwise(round,2)(df)})
colnames(hsu2.soilprop)[1] <- "Spatial_unit"
hsu2.soilprop$Spatial_unit <- paste0("U",hsu2.soilprop$Spatial_unit,sep="")
#calculation of mean soil properties value per nuts3
soil.nuts3 <-ddply(hsu.nuts.soil[,2:18],c("nuts3"),function(df){
  numcolwise(mean)(df, na.rm = T)
  numcolwise(round,2)(df)})
colnames(soil.nuts3)[1] <- "Spatial_unit"
#calculation of mean soil properties value per nuts2
soil.nuts2 <-ddply(hsu.nuts.soil[,c(2:17,20)],c("nuts2"),function(df){
  numcolwise(mean)(df, na.rm = T)
  numcolwise(round,2)(df)})
colnames(soil.nuts2)[1] <- "Spatial_unit"
#calculation of mean soil properties value per capri nuts
soil.caprinuts <-ddply(hsu.nuts.soil[,c(2:17,21)],c("CAPRI_NUTSII"),function(df){
  numcolwise(mean)(df, na.rm = T)
  numcolwise(round,2)(df)})
colnames(soil.caprinuts)[1] <- "Spatial_unit"
soil.hsu.nuts <- rbind(hsu2.soilprop,soil.nuts3,soil.nuts2,soil.caprinuts)
soil.hsu.nuts <- soil.hsu.nuts[,1:16]
save(soil.hsu.nuts,file = "soil.hsu.nuts.rdata")
#load(file = "soil.hsu.nuts.rdata")

#Create gdx file with new hsu and soil properties
symDim <- 2
ciao <- soil.hsu.nuts[!is.na(soil.hsu.nuts$Spatial_unit),]
attr(ciao,"symName") <- "p_soil"
lst <- wgdx.reshape(ciao,symDim,tName = "p_soil_properties")
wgdx.lst("soil.hsu.nuts.gdx",lst)














#
save.image("X:/MARS_disaggregation/hsu2_database_togdx_201605_nocita/hsu2_xavi.RData")
#













#centroids
hsu2.coo <- read.csv(file="HSU2_CENTER_COORDINATES.csv") #load of csv file linking coordinated and HSU numbers, dataset coming from capri/dat/capdis/hsu2
colnames(hsu2.coo)[1] <- "hsu2"
hsu2.coo.nuts <- join(hsu2.coo,hsu2.nuts) #link between hsu coordinated and nuts
save(hsu2.coo.nuts,file = "hsu2.coo.nuts.rdata")
load(file = "hsu2.coo.nuts.rdata")
hsu2.coo.nuts$hsu2 <- paste0("U",hsu2.coo.nuts$hsu2,sep="")
hsu2.coo1 <- hsu2.coo.nuts[,1:3]
colnames(hsu2.coo1)[1] <- "Spatial_unit"
hsu2.nuts3 <- hsu2.coo.nuts[,c(4,2,3)]
colnames(hsu2.nuts3)[1] <- "Spatial_unit"
hsu2.nuts2 <- hsu2.coo.nuts[,c(5,2,3)]
colnames(hsu2.nuts2)[1] <- "Spatial_unit"
hsu2.CAPRI_NUTSII <- hsu2.coo.nuts[,c(6,2,3)] 
colnames(hsu2.CAPRI_NUTSII)[1] <- "Spatial_unit"
centres.hsu.nuts <- rbind(hsu2.coo1,hsu2.nuts3,hsu2.nuts2,hsu2.CAPRI_NUTSII)
save(centres.hsu.nuts,file = "coo.hsu.nuts.rdata")
#creation of gdx file with updated hsu
ciao <- centres.hsu.nuts[!is.na(centres.hsu.nuts$Spatial_unit),]
symDim <- 2
attr(ciao,"symName") <- "p_centres"
lst <- wgdx.reshape(ciao,symDim,tName = "coordinates")
wgdx.lst("coo.hsu.nuts.gdx",lst)

#mars_yield
mars.yield <- rgdx.param("mars_yield.gdx","p_marsyield") #load gdx file linking mars yields with old hsu (hsmu), dataset coming from capri/dat/capdis/hsu2 
names(mars.yield) <- c("HSMUID","crop","yield","year","value")
mars.yield$HSMUID <- factor(mars.yield$HSMUID,levels = unique(mars.yield$HSMUID)) 
mars.yield$crop <- factor(mars.yield$crop,levels = unique(mars.yield$crop))
mars.yield$yield <- factor(mars.yield$yield,levels = unique(mars.yield$yield)) 
mars.yield$year <- factor(mars.yield$year,levels = unique(mars.yield$year)) 
mars.yield.smu <- rgdx.param("mars_yield.gdx","p_marsyieldsmu") #load gdx file linking old hsu (hsmu) with soil mapping unit (smu), dataset coming from capri/dat/capdis/hsu2 
mars.yield.smu1 <- mars.yield.smu[,2:5]
names(mars.yield.smu1) <- c("HSMUID","SMU","year","yield")
mars.yield.smu1$HSMUID <- factor(mars.yield.smu1$HSMUID,levels = unique(mars.yield.smu1$HSMUID)) 
mars.yield.smu1$HSMUID <- paste0("U",mars.yield.smu1$HSMUID,sep="")
mars.yield.smu1$SMU <- factor(mars.yield.smu1$SMU,levels = unique(mars.yield.smu1$SMU))
mars.yield.smu1$yield <- factor(mars.yield.smu1$yield,levels = unique(mars.yield.smu1$yield)) 
mars.yield.smu1$year <- factor(mars.yield.smu1$year,levels = unique(mars.yield.smu1$year)) 
mars.yield.smu2 <- mars.yield.smu1[,1:2]
mars.yield.smu2$HSMUID <- as.factor(mars.yield.smu2$HSMUID)
mars.hsmu.smu <- join(mars.yield,mars.yield.smu2) #join to link mars yields and smu
mars.hsmu.smu1 <- mars.hsmu.smu[!is.na(mars.hsmu.smu$SMU),]
mars.smu <- ddply(mars.hsmu.smu1,c("SMU","crop","yield","year"),function(df)data.frame(value=mean(df$value)))
mars.smu.hsu2 <- join(mars.smu,hsu2.nuts) #join to link mars yield smu with new hsu and nuts
mars.smu.hsu2$hsu2 <- as.factor(mars.smu.hsu2$hsu2)
#calculation of yields weighted mean per hsu area
mars.hsu2 <- ddply(mars.smu.hsu2,c("hsu2","crop","yield"),function(df)data.frame(mean=round(weighted.mean(df$value,df$area),2)))
mars.hsu2$hsu2 <- paste0("U",mars.hsu2$hsu2,sep="")
colnames(mars.hsu2)[1] <- "Spatial_unit"
#calculation of yields mean per nuts3
mars.nuts3 <- ddply(mars.smu.hsu2,c("nuts3","crop","yield"),function(df)data.frame(mean=round(mean(df$value),2)))
colnames(mars.nuts3)[1] <- "Spatial_unit"
#calculation of yields mean per nuts2
mars.nuts2 <- ddply(mars.smu.hsu2,c("nuts2","crop","yield"),function(df)data.frame(mean=round(mean(df$value),2)))
colnames(mars.nuts2)[1] <- "Spatial_unit"
#calculation of yields mean per capri nuts
mars.caprinuts3 <-ddply(mars.smu.hsu2,c("CAPRI_NUTSII","crop","yield"),function(df)data.frame(mean=round(mean(df$value),2)))
colnames(mars.caprinuts3)[1] <- "Spatial_unit"
mars.hsu.nuts <- rbind(mars.hsu2,mars.nuts3,mars.nuts2,mars.caprinuts3)
mars.hsu.nuts1 <- mars.hsu.nuts[!is.na(mars.hsu.nuts$Spatial_unit),]
save(mars.hsu.nuts1,file = "mars.hsu.nuts.rdata")
#creation of new gdx, linking mars yields, new hsu and nuts
symDim <- 4
ciao <- mars.hsu.nuts1
attr(ciao,"symName") <- "p_marsyield"
lst <- wgdx.reshape(ciao,symDim,tName = "p_marsyield_data")
wgdx.lst("marsyield.hsu.nuts.gdx",lst)

#irrishare
uscie.irr <- rgdx.param("uscie_irrishare.gdx","irr_share2000") #load gdx file linking uscie and irrigation shares, dataset coming from capri/dat/capdis/uscie
colnames(uscie.irr)[1] <- "USCIE_RC"
hsu.irr <- join(uscie.irr,hsu_uscie1) #join to link uscie and new hsu for irrigation shares
colnames(hsu.irr)[3] <- "hsu2"
hsu.irr1 <- join(hsu.irr,hsu2.nuts) #join to link new hsu and nuts for irrigation shares  
hsu.irr2 <- hsu.irr1[,2:8]
save(hsu.irr2,file="hsu.irrshare.rdata")
load(file="hsu.irrshare.rdata")
colnames(hsu.irr2)[2] <- c("hsu2")
colnames(hsu.irr2)[1] <- c("irrshare")
temp.hsu.nuts <- hsu.irr2
#calculation of irrigation shares weighted mean per hsu area
temp.hsu <-ddply(temp.hsu.nuts,c("hsu2"),function(df)data.frame(mean.irrshare=round(weighted.mean(df$irrshare,df$area),2),min.irrshare=min(df$irrshare),max.irrshare=max(df$irrshare)))
colnames(temp.hsu)[1] <- "Spatial_unit"
temp.hsu$Spatial_unit <- paste0("U",temp.hsu$Spatial_unit,sep="")
#calculation of irrigation shares mean per nuts3
temp.nuts3 <-ddply(temp.hsu.nuts,c("nuts3"),function(df)data.frame(mean.irrshare=round(weighted.mean(df$irrshare,df$area),2),min.irrshare=min(df$irrshare),max.irrshare=max(df$irrshare)))
colnames(temp.nuts3)[1] <- "Spatial_unit"
#calculation of irrigation shares mean per nuts2
temp.nuts2 <-ddply(temp.hsu.nuts,c("nuts2"),function(df)data.frame(mean.irrshare=round(weighted.mean(df$irrshare,df$area),2),min.irrshare=min(df$irrshare),max.irrshare=max(df$irrshare)))
colnames(temp.nuts2)[1] <- "Spatial_unit"
#calculation of irrigation shares mean per capri nuts
temp.caprinuts <-ddply(temp.hsu.nuts,c("CAPRI_NUTSII"),function(df)data.frame(mean.irrshare=round(weighted.mean(df$irrshare,df$area),2),min.irrshare=min(df$irrshare),max.irrshare=max(df$irrshare)))
colnames(temp.caprinuts)[1] <- "Spatial_unit"
irrshare.hsu.nuts <- rbind(temp.hsu,temp.nuts3,temp.nuts2,temp.caprinuts)
save(irrshare.hsu.nuts,file = "irrshare.hsu.nuts.rdata")
#load(file = "irrishare.nuts.rdata")
#creation of new 2 dimensions gdx, linking irrigation shares, new hsu and nuts
symDim <- 2
ciao <- irrshare.hsu.nuts[!is.na(irrshare.hsu.nuts$Spatial_unit),]
attr(ciao,"symName") <- "p_irrshare"
lst <- wgdx.reshape(ciao,symDim,tName = "p_irrshare_stats")
wgdx.lst("irrshare.hsu.nuts.gdx",lst)

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
hsu2.month.nuts <- join(hsu2.met.month,hsu2.nuts)
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
hsu2.trimester.nuts <- join(hsu2.met.trimester,hsu2.nuts)
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

#gisconuts
gis_nuts <- read.csv(file="LINK_HSU2_CAPRI_GISCO2010_NUTS_CODES_uniqueGISCO2010.csv") #load of new hsu and gisconuts
gis.nuts <- gis_nuts[,c(2,4,6,8,11,13,14)]
colnames(gis.nuts)[5] <- "nuts3"
gis.hsu2.nuts <- na.omit(merge(hsu2.nuts,gis.nuts))
gis.hsu2.nuts <- gis.hsu2.nuts[,-5]
save(gis.hsu2.nuts,file="gis.hsu2.nuts.rdata")
load(file="gis.hsu2.nuts.rdata")
gis.hsu2.nuts1 <- gis.hsu2.nuts
colnames(gis.hsu2.nuts1)[6:9] <- c("s_gisco2010nuts0","s_gisco2010nuts1","s_gisco2010nuts2","s_gisco2010nuts3")
gis.hsu2 <- gis.hsu2.nuts1[,c(4,6:9)]
colnames(gis.hsu2)[1] <- "Spatial_unit"
gis.hsu2$Spatial_unit <- paste0("U",gis.hsu2$Spatial_unit,sep="")
gis.nuts3 <- gis.hsu2.nuts1[,c(1,6:9)]
colnames(gis.nuts3)[1] <- "Spatial_unit"
gis.nuts2 <- gis.hsu2.nuts1[,c(5,6:9)]
colnames(gis.nuts2)[1] <- "Spatial_unit"
gis.capri.nuts2 <- gis.hsu2.nuts1[,c(2,6:9)]
colnames(gis.capri.nuts2)[1] <- "Spatial_unit"
ciao <- rbind(gis.hsu2,gis.nuts3,gis.nuts2,gis.capri.nuts2)
ciao$not_to_use <- "1"
ciao$not_to_use <- as.numeric(ciao$not_to_use)
symDim <- 6 
attr(ciao,"symName") <- "m_gisco2010_nuts3_2_1_0"
lst <- wgdx.reshape(ciao,symDim,tName="not_to_use")
wgdx.lst("gisco.hsu.nuts.gdx",lst)

#wet deposition for OXN, RDN, and SOX

#Wet deposition OXN
wdep.oxn <- read.csv(file = "USCIE_EMEP_HSU2_CTRY_WDEP_OXN.csv") #load csv file linking uscie and OXN wet deposition, dataset coming from capri/dat/capdis/uscie
wdep.oxn1 <- wdep.oxn[,c(3,4,7:11)]
colnames(wdep.oxn1)[1:2] <- c("hsu2","nuts3")
save(wdep.oxn1,file = "wdep.oxn.rdata")
load(file = "wdep.oxn.rdata")
hsu2.nuts1 <- hsu2.nuts[,c(1,2,3:6)]
hsu2.nuts1$hsu2 <- as.factor(hsu2.nuts1$hsu2)
wdep.oxn.nuts <- join(wdep.oxn1,hsu2.nuts1)
wdep.oxn.hsu2 <- ddply(wdep.oxn.nuts,"hsu2",function(df)data.frame(WDON06=weighted.mean(df$WDON06,df$area),WDON07=weighted.mean(df$WDON07,df$area),WDON08=weighted.mean(df$WDON08,df$area),WDON09=weighted.mean(df$WDON09,df$area),WDON10=weighted.mean(df$WDON10,df$area)))
colnames(wdep.oxn.hsu2)[1] <- "Spatial_unit"
wdep.oxn.hsu2$Spatial_unit <- paste0("U",wdep.oxn.hsu2$Spatial_unit,sep="")
wdep.oxn.nuts3 <- ddply(wdep.oxn.nuts,"nuts3",function(df)data.frame(WDON06=mean(df$WDON06),WDON07=mean(df$WDON07),WDON08=mean(df$WDON08),WDON09=mean(df$WDON09),WDON10=mean(df$WDON10)))
colnames(wdep.oxn.nuts3)[1]="Spatial_unit"
wdep.oxn.nuts2 <- ddply(na.omit(wdep.oxn.nuts),"nuts2",function(df)data.frame(WDON06=mean(df$WDON06),WDON07=mean(df$WDON07),WDON08=mean(df$WDON08),WDON09=mean(df$WDON09),WDON10=mean(df$WDON10)))
colnames(wdep.oxn.nuts2)[1]="Spatial_unit"
wdep.oxn.caprinuts <- ddply(na.omit(wdep.oxn.nuts),"CAPRI_NUTSII",function(df)data.frame(WDON06=mean(df$WDON06),WDON07=mean(df$WDON07),WDON08=mean(df$WDON08),WDON09=mean(df$WDON09),WDON10=mean(df$WDON10)))
colnames(wdep.oxn.caprinuts)[1]="Spatial_unit"
wdep.oxn.hsu.nuts <- rbind(wdep.oxn.hsu2,wdep.oxn.nuts3,wdep.oxn.nuts2,wdep.oxn.caprinuts)
save(wdep.oxn.hsu.nuts,file = "wdep.oxn.hsu.nuts.rdata")


#Wet deposition RDN
wdep.rdn <- read.csv(file = "USCIE_EMEP_HSU2_CTRY_WDEP_RDN.csv") #load csv file linking uscie and RDN wet deposition, dataset coming from capri/dat/capdis/uscie
wdep.rdn1 <- wdep.rdn[,c(3,4,7:11)]
colnames(wdep.rdn1)[1:2] <- c("hsu2","nuts3")
save(wdep.rdn1,file = "wdep.rdn.rdata")
load(file = "wdep.rdn.rdata")
hsu2.nuts1 <- hsu2.nuts[,c(1,2,3:6)]
wdep.rdn.nuts <- join(wdep.rdn1,hsu2.nuts1)
wdep.rdn.nuts$hsu2 <- as.factor(wdep.rdn.nuts$hsu2)
wdep.rdn.hsu2 <- ddply(wdep.rdn.nuts,"hsu2",function(df)data.frame(WDRN06=weighted.mean(df$WDRN06,df$area),WDRN07=weighted.mean(df$WDRN07,df$area),WDRN08=weighted.mean(df$WDRN08,df$area),WDRN09=weighted.mean(df$WDRN09,df$area),WDRN10=weighted.mean(df$WDRN10,df$area)))
colnames(wdep.rdn.hsu2)[1] <- "Spatial_unit"
wdep.rdn.hsu2$Spatial_unit <- paste0("U",wdep.rdn.hsu2$Spatial_unit,sep="")
wdep.rdn.nuts3 <- ddply(wdep.rdn.nuts,"nuts3",function(df)data.frame(WDRN06=mean(df$WDRN06),WDRN07=mean(df$WDRN07),WDRN08=mean(df$WDRN08),WDRN09=mean(df$WDRN09),WDRN10=mean(df$WDRN10)))
colnames(wdep.rdn.nuts3)[1] <- "Spatial_unit"
wdep.rdn.nuts2 <- ddply(na.omit(wdep.rdn.nuts),"nuts2",function(df)data.frame(WDRN06=mean(df$WDRN06),WDRN07=mean(df$WDRN07),WDRN08=mean(df$WDRN08),WDRN09=mean(df$WDRN09),WDRN10=mean(df$WDRN10)))
colnames(wdep.rdn.nuts2)[1] <- "Spatial_unit"
wdep.rdn.caprinuts <- ddply(na.omit(wdep.rdn.nuts),"CAPRI_NUTSII",function(df)data.frame(WDRN06=mean(df$WDRN06),WDRN07=mean(df$WDRN07),WDRN08=mean(df$WDRN08),WDRN09=mean(df$WDRN09),WDRN10=mean(df$WDRN10)))
colnames(wdep.rdn.caprinuts)[1] <- "Spatial_unit"
wdep.rdn.hsu.nuts <- rbind(wdep.rdn.hsu2,wdep.rdn.nuts3,wdep.rdn.nuts2,wdep.rdn.caprinuts)
save(wdep.rdn.hsu.nuts,file = "wdep.rdn.hsu.nuts.rdata")
#load(file = "wdep.rdn.hsu.nuts.rdata")

#Wet deposition SOX
wdep.sox <- read.csv(file = "USCIE_EMEP_HSU2_CTRY_WDEP_SOX.csv") #load csv file linking uscie and SOX wet deposition, dataset coming from capri/dat/capdis/uscie
wdep.sox1 <- wdep.sox[,c(3,4,7:11)]
colnames(wdep.sox1)[1:2] <- c("hsu2","nuts3")
save(wdep.sox1,file = "wdep.sox.rdata")
load(file = "wdep.sox.rdata")
hsu2.nuts1 <- hsu2.nuts[,c(1,2,3:6)]
hsu2.nuts1$hsu2 <- as.factor(hsu2.nuts1$hsu2)
wdep.sox.nuts <- join(wdep.sox1,hsu2.nuts1)
wdep.sox.hsu2 <- ddply(wdep.sox.nuts,"hsu2",function(df)data.frame(WDOS06=weighted.mean(df$WDOS06,df$area),WDOS07=weighted.mean(df$WDOS07,df$area),WDOS08=weighted.mean(df$WDOS08,df$area),WDOS09=weighted.mean(df$WDOS09,df$area),WDOS10=weighted.mean(df$WDOS10,df$area)))
colnames(wdep.sox.hsu2)[1]="Spatial_unit"
wdep.sox.hsu2$Spatial_unit <- paste0("U",wdep.sox.hsu2$Spatial_unit,sep="")
wdep.sox.nuts3 <- ddply(wdep.sox.nuts,"nuts3",function(df)data.frame(WDOS06=mean(df$WDOS06),WDOS07=mean(df$WDOS07),WDOS08=mean(df$WDOS08),WDOS09=mean(df$WDOS09),WDOS10=mean(df$WDOS10)))
colnames(wdep.sox.nuts3)[1]="Spatial_unit"
wdep.sox.nuts2 <- ddply(na.omit(wdep.sox.nuts),"nuts2",function(df)data.frame(WDOS06=mean(df$WDOS06),WDOS07=mean(df$WDOS07),WDOS08=mean(df$WDOS08),WDOS09=mean(df$WDOS09),WDOS10=mean(df$WDOS10)))
colnames(wdep.sox.nuts2)[1]="Spatial_unit"
wdep.sox.caprinuts <- ddply(na.omit(wdep.sox.nuts),"CAPRI_NUTSII",function(df)data.frame(WDOS06=mean(df$WDOS06),WDOS07=mean(df$WDOS07),WDOS08=mean(df$WDOS08),WDOS09=mean(df$WDOS09),WDOS10=mean(df$WDOS10)))
colnames(wdep.sox.caprinuts)[1]="Spatial_unit"
wdep.sox.hsu.nuts <- rbind(wdep.sox.hsu2,wdep.sox.nuts3,wdep.sox.nuts2,wdep.sox.caprinuts)
save(wdep.sox.hsu.nuts,file = "wdep.sox.hsu.nuts.rdata")

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
hsu2.nuts1 <- hsu2.nuts[,c(1,2,3:6)]
hsu2.nuts1$hsu2 <- as.factor(hsu2.nuts1$hsu2)
ddep.oxn.nuts <- join(ddep.oxn1,hsu2.nuts1)
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
hsu2.nuts1 <- hsu2.nuts[,c(1,2,3:6)]
hsu2.nuts1$hsu2 <- as.factor(hsu2.nuts1$hsu2)
ddep.rdn.nuts <- join(ddep.rdn1,hsu2.nuts1)
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
hsu2.nuts1 <- hsu2.nuts[,c(1,2,3:6)]
hsu2.nuts1$hsu2 <- as.factor(hsu2.nuts1$hsu2)
ddep.sox.nuts <- join(ddep.sox1,hsu2.nuts1)
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
lucas.nuts <- join(lucas1.hsu2,hsu2.nuts)
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





