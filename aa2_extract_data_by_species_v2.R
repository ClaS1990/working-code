# Processes species occurrence, NVIS major vegetation groups and Austraits growth forms

setwd('H:/WSU/data')

library(raster)
library(dplyr)
library(stringr)

#### extract variables from Austraits ####_____________________________________________________________________

library(austraits)

austraits <- load_austraits()
trait_names <- austraits$traits$trait_name %>% unique()
growth_form <- extract_trait(austraits, "plant_growth_form")
plant_height <- extract_trait(austraits, "plant_height")
leaf_phenology <- extract_trait(austraits, "leaf_phenology")
regen_strategy <- extract_trait(austraits, "regen_strategy")
nitrogen_fixing <- extract_trait(austraits, "nitrogen_fixing")
photosynthetic_pathway <- extract_trait(austraits, "photosynthetic_pathway")
resource_use <- extract_trait(austraits, "plant_type_by_resource_use")   # indicates mesic / xeric
life_history <- extract_trait(austraits, "life_history")   # indicates annual / perennial
specific_leaf_area <- extract_trait(austraits, "specific_leaf_area") # low values indicate sclerophyll, high values indicate mesophyll

#### select a consistent value for each trait, either the mean reported value or the mode ####

find.mode <- function(x){
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
return(u)}

find.mean <- function(x){
  u <- mean(x,na.rm=T)
  return(u)}

austraits_growth_form <- data.frame(taxon=growth_form$traits$taxon_name,growth_form=growth_form$traits$value)
austraits_growth_form <- aggregate(austraits_growth_form$growth_form,by=list(austraits_growth_form$taxon),FUN='find.mode') # assumes most common value is best
colnames(austraits_growth_form) <- c('taxon','growth_form')

austraits_plant_height <- data.frame(taxon=plant_height$traits$taxon_name,plant_height=plant_height$traits$value,type=plant_height$traits$value_type)
austraits_plant_height <- austraits_plant_height[which(austraits_plant_height$type=='expert_max'),]   # theoretical maximums rather that minimums or site maximums
austraits_plant_height <- aggregate(austraits_plant_height$plant_height,by=list(austraits_plant_height$taxon),FUN='find.mean')
colnames(austraits_plant_height) <- c('taxon','plant_height')

austraits_leaf_phenology <- data.frame(taxon=leaf_phenology$traits$taxon_name,leaf_phenology=leaf_phenology$traits$value)
austraits_leaf_phenology <- aggregate(austraits_leaf_phenology$leaf_phenology,by=list(austraits_leaf_phenology$taxon),FUN='find.mode')
colnames(austraits_leaf_phenology) <- c('taxon','leaf_phenology')

austraits_regen_strategy <- data.frame(taxon=regen_strategy$traits$taxon_name,regen_strategy=regen_strategy$traits$value)
austraits_regen_strategy <- aggregate(austraits_regen_strategy$regen_strategy,by=list(austraits_regen_strategy$taxon),FUN='find.mode')
colnames(austraits_regen_strategy) <- c('taxon','regen_strategy')

austraits_nitrogen_fixing <- data.frame(taxon=nitrogen_fixing$traits$taxon_name,nitrogen_fixing=nitrogen_fixing$traits$value)
austraits_nitrogen_fixing <- aggregate(austraits_nitrogen_fixing$nitrogen_fixing,by=list(austraits_nitrogen_fixing$taxon),FUN='find.mode')
colnames(austraits_nitrogen_fixing) <- c('taxon','nitrogen_fixing')

austraits_photosynthetic_pathway <- data.frame(taxon=photosynthetic_pathway$traits$taxon_name,photosynthetic_pathway=photosynthetic_pathway$traits$value)
austraits_photosynthetic_pathway <- aggregate(austraits_photosynthetic_pathway$photosynthetic_pathway,by=list(austraits_photosynthetic_pathway$taxon),FUN='find.mode')
colnames(austraits_photosynthetic_pathway) <- c('taxon','photosynthetic_pathway')

austraits_resource_use <- data.frame(taxon=resource_use$traits$taxon_name,resource_use=resource_use$traits$value)
austraits_resource_use <- aggregate(austraits_resource_use$resource_use,by=list(austraits_resource_use$taxon),FUN='find.mode')
colnames(austraits_resource_use) <- c('taxon','resource_use')

austraits_life_history <- data.frame(taxon=life_history$traits$taxon_name,life_history=life_history$traits$value)
austraits_life_history <- aggregate(austraits_life_history$life_history,by=list(austraits_life_history$taxon),FUN='find.mode')
colnames(austraits_life_history) <- c('taxon','life_history')

austraits_specific_leaf_area <- data.frame(taxon=specific_leaf_area$traits$taxon_name,specific_leaf_area=specific_leaf_area$traits$value)
austraits_specific_leaf_area <- aggregate(austraits_specific_leaf_area$specific_leaf_area,by=list(austraits_specific_leaf_area$taxon),FUN='find.mean')
colnames(austraits_specific_leaf_area) <- c('taxon','specific_leaf_area')

#### species occurrence ####_____________________________________________________________________

species_occ <- read.csv('aus_flora_occurrence_corrected_euc.csv')

# plot(species_occ$longitude,species_occ$latitude)

# merge species occurrence with austraits data #

base1 <- merge(species_occ, austraits_growth_form, by = 'taxon',all.x=T,all.y=F)
base2 <- merge(base1,austraits_plant_height, by = 'taxon',all.x=T,all.y=F)
base3 <- merge(base2,austraits_leaf_phenology, by = 'taxon',all.x=T,all.y=F)
base4 <- merge(base3,austraits_regen_strategy, by = 'taxon',all.x=T,all.y=F)
base5 <- merge(base4,austraits_nitrogen_fixing, by = 'taxon',all.x=T,all.y=F)
base6 <- merge(base5,austraits_photosynthetic_pathway, by = 'taxon',all.x=T,all.y=F)
base7 <- merge(base6,austraits_resource_use, by = 'taxon',all.x=T,all.y=F)
base8 <- merge(base7,austraits_life_history, by = 'taxon',all.x=T,all.y=F)
species_occ_dat <- merge(base8,austraits_specific_leaf_area, by = 'taxon',all.x=T,all.y=F)

#### climate - annual rainfall ####_____________________________________________________________________

ann_rain <- raster('clim/rainan.txt') # downloaded from http://www.bom.gov.au/jsp/ncc/climate_averages/rainfall/index.jsp

grid_vals <- matrix(1:612226,691,886)
grid <- raster(grid_vals,xmn=111.975,xmx=156.275,ymn=-44.525,ymx=-9.975)

pnts <- cbind(species_occ_dat$longitude,species_occ_dat$latitude)

pts_rain <- extract(ann_rain,pnts)
pts_loc <- extract(grid,pnts) # assign values based on grid cell

df_loc <- data.frame(locs=paste(species_occ_dat$taxon,pts_loc,sep=' '))
unique_locs <- mutate_at(df_loc,vars(locs), funs(replace(., duplicated(.), NA))) # identify values from unique grid cells, only count once per cell

pts_rain[which(is.na(unique_locs$locs))] <- NA # remove duplicate values extracted from same grid cell to reduce collection bias

species_occ_dat$ann_rain <- pts_rain

#check#
cbind(species_occ_dat$taxon,species_occ_dat$ann_rain)[which(pts_loc==412058),]


#### NVIS data ####_____________________________________________________________________

# mvgExtant <- raster('NVIS_MVG/GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf')
# mvgExtant2 <- projectRaster(mvgExtant,crs='+proj=longlat +datum=WGS84 +no_defs',method='ngb')
# writeRaster(mvgExtant2,'NVIS_MVG/GRID_NVIS6_0_AUST_EXT_MVG/mvg_extant2.tif')

mvgExtant2 <- raster('NVIS_MVG/GRID_NVIS6_0_AUST_EXT_MVG/mvg_extant2.tif') # tiff file type and lat / long projection

plot(mvgExtant2)
# points(pnts,pch=16,cex=0.1,col='green')

grid_vals2 <- matrix(1:1900260176,40616,46786)
grid2 <- raster(grid_vals2,xmn=109.4993,xmx=157.221,ymn=-44.32338,ymx=-8.134523)

pts_mvg <- extract(mvgExtant2,pnts)
pts_loc2 <- extract(grid2,pnts) # assign values based on grid cell

df_loc2 <- data.frame(locs=paste(species_occ_dat$taxon,pts_loc2,sep=' '))
unique_locs2 <- mutate_at(df_loc2,vars(locs), funs(replace(., duplicated(.), NA))) # identify values from unique grid cells, only count once per cell

pts_mvg[which(is.na(unique_locs2$locs))] <- NA

species_occ_dat$mvg <- pts_mvg

# check #

cbind(species_occ_dat$taxon,species_occ_dat$mvg)[which(pts_loc2==1543056988),] # should see duplicates for the same species in the same cell are NA

# summarise occurrence of species in major vegetation groups #

find.num <- function(x){
  u <- sum(x,na.rm=T)/mean(x,na.rm=T)
  return(u)}

dat_mvg <- data.frame(cbind(species_occ_dat$taxon,species_occ_dat$mvg))
colnames(dat_mvg) <- c('taxon','mvg')
dat_mvg$mvg[which(dat_mvg$mvg == 0)] <- 0.1

occ_in_mvg <- aggregate(as.numeric(dat_mvg$mvg),by=list(as.numeric(dat_mvg$mvg),dat_mvg$taxon),FUN='find.num')
colnames(occ_in_mvg) <- c('mvg','taxon','cell_count')
occ_in_mvg$mvg[which(occ_in_mvg$mvg == 0.1)] <- 0

species_mvg <- matrix(NA,length(unique(species_occ_dat$taxon)),length(sort(unique(species_occ_dat$mvg))))
colnames(species_mvg) <- sort(unique(species_occ_dat$mvg))
rownames(species_mvg) <- unique(species_occ_dat$taxon)

for (i in 1:length(unique(species_occ_dat$taxon))){
  species <- unique(occ_in_mvg$taxon)[i]
  dat <- occ_in_mvg[which(occ_in_mvg$taxon==species),]
  species_mvg[i,which(as.numeric(colnames(species_mvg)) %in% dat$mvg)] <- dat$cell_count
}

### version with unique values for species ###_____________________________________________________________________

# aggregate austraits data and average rainfall by species
data_summ <- aggregate(species_occ_dat[,-c(1:3,14,15)],by=list(species_occ_dat$taxon),FUN='find.mode')  # mode for everything except rain and latitude
data_summ2 <- aggregate(species_occ_dat[,c(14)],by=list(species_occ_dat$taxon),FUN='find.mean')         # mean for rain 
data_summ3 <- aggregate(species_occ_dat[,c(3)],by=list(species_occ_dat$taxon),FUN=function(X) quantile(X,0.02))   # 2nd percentile for latitude (i.e. southernmost latitude)

data_by_species <- cbind(data_summ,data_summ2$x,data_summ3$x)
colnames(data_by_species) <- c("taxon","family", "growth_form","plant_height","leaf_phenology","regen_strategy",
                               "nitrogen_fixing","photosynthetic_pathway","resource_use","life_history","specific_leaf_area","ann.rain","lat_02")

# add NVIS occurrence matrix
data_by_species2 <- cbind(data_by_species,species_mvg)

# add Rachael G's species data on aridity ranges, temperature ranges and soil properties
nb_2022 <- read.csv('RG_niche_data/nb_2022_edited.csv',header=T)
nb_2022_extract <- data.frame(taxon=nb_2022$taxon,PTO_02=nb_2022$PTO_02,PTO_98=nb_2022$PTO_98,temp_02=nb_2022$bio1_02,temp_98=nb_2022$bio1_98,arid_02=nb_2022$aridity_02,arid_98=nb_2022$aridity_98) # use phosphorous, temperature and aridity limits
data_by_species3 <- merge(data_by_species2, nb_2022_extract, by = 'taxon',all.x=T,all.y=F)

save(data_by_species3,file='data_for_PFT_definition.RData')

