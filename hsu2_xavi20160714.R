rm(list=objects())
source("/Users/xavi/Documents/JRC_MARS/gisdata4caprihsu/hsu4capri_header.r") #this is only used while coding
#source("hsu4capri_header.r")


###### Data to be computed #####
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

