setwd('H:/WSU/data')

library(raster)
library(dplyr)
library(stringr)
library(austraits)

### load data upon which PFT definitions are based ###________________________________________________

load('data_for_PFT_definition.RData')

### define PFTs based on data ###_____________________________________________________________________

### SHRUBS ###
mesic_shrub <- data_by_species3[which(data_by_species3$growth_form=='shrub' & data_by_species3$nitrogen_fixing=='0' & data_by_species3$ann.rain>1000),]
xeric_shrub <- data_by_species3[which(data_by_species3$growth_form=='shrub' & data_by_species3$nitrogen_fixing=='0' & data_by_species3$ann.rain<500),]
mesic_shrub_Nfix <- data_by_species3[which(data_by_species3$growth_form=='shrub' & data_by_species3$nitrogen_fixing=='1' & data_by_species3$ann.rain>1000),]
xeric_shrub_Nfix <- data_by_species3[which(data_by_species3$growth_form=='shrub' & data_by_species3$nitrogen_fixing=='1' & data_by_species3$ann.rain<500),]

### GRASSES ###
grasses <- c('herb','graminoid','herb_large','prostrate_herb','cushion','erect_leafy','graminoid_not_tussock','graminoid_not_tussock_tall','graminoid_tussock','graminoid_tussock_tall',
             'rosette','rosette_erect','tussock')

ann_c3_grass <- data_by_species3[which(data_by_species3$growth_form %in% grasses & data_by_species3$photosynthetic_pathway=='c3' & data_by_species3$life_history=='annual'),]
ann_c4_grass <- data_by_species3[which(data_by_species3$growth_form %in% grasses & data_by_species3$photosynthetic_pathway=='c4' & data_by_species3$life_history=='annual'),]
perenn_tussock_c3 <- data_by_species3[which(data_by_species3$growth_form %in% grasses & data_by_species3$photosynthetic_pathway=='c3' & data_by_species3$life_history=='perennial' & data_by_species3$`19`>data_by_species3$`20`),]
perenn_tussock_c4 <- data_by_species3[which(data_by_species3$growth_form %in% grasses & data_by_species3$photosynthetic_pathway=='c4' & data_by_species3$life_history=='perennial' & data_by_species3$`19`>data_by_species3$`20`),]
perenn_hummock_c4 <- data_by_species3[which(data_by_species3$growth_form %in% grasses & data_by_species3$photosynthetic_pathway=='c4' & data_by_species3$life_history=='perennial' & data_by_species3$`20`>data_by_species3$`19`),]

### TREES ###
data_acacias <- data_by_species3[which(lapply(data_by_species3$taxon,FUN=function(X) (str_split(X,pattern=' ')[[1]][1]))=='Acacia'),]
acacia_mesic_midstory <- data_acacias[(data_acacias$growth_form=='tree' & data_acacias$ann.rain >1000),]
acacia_harpophylla <- data_acacias[which(data_acacias$taxon=='Acacia harpophylla'),]
acacia_mulga <- data_acacias[which(data_acacias$taxon=='Acacia aneura'),]

# remove acacias from consideration for subsequent tree PFTs
data_trees <- data_by_species3[-which(lapply(data_by_species3$taxon,FUN=function(X) (str_split(X,pattern=' ')[[1]][1]))=='Acacia'),]

savanna_tree_index <- which(data_trees$growth_form=='tree' & (data_trees$`5`>1 | data_trees$`12`>1) & data_trees$lat_02 > -23.44) # open woodlands - MVGs selected based on map
savanna_tree <- data_trees[savanna_tree_index,]

# remove savanna trees from consideration for subsequent tree PFTs
data_trees2 <- data_trees[-savanna_tree_index,]

subalpine_tree_index <- which(data_trees2$growth_form=='tree' & data_trees2$temp_98<18 & data_trees2$temp_98>12 & data_trees2$temp_02<8 & data_trees2$temp_02>4) # defined ranges by looking at values for species listed here: https://www.environment.nsw.gov.au/threatenedspeciesapp/VegClass.aspx?vegClassName=Subalpine%20Woodlands
subalpine_tree <- data_trees2[subalpine_tree_index,]

data_trees3 <- data_trees2[-subalpine_tree_index,]

floodplain_sclerophyll_index <- which(data_trees3$taxon %in% c('Eucalyptus camaldulensis','Eucalyptus largiflorens','Eucalyptus melliodora','Eucalyptus microcarpa','Eucalyptus marginata'))
floodplain_sclerophyll <- data_trees3[floodplain_sclerophyll_index,]

# for remaining trees, do not consider species that already fell into one of the first 4 categories
data_trees4 <- data_trees3[-floodplain_sclerophyll_index,]

cold_mesophyll_tree <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area > 12 & data_trees4$temp_98<18),] # need data to separate sclerophyll from mesophyll
warm_mesophyll_tree <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area > 12 & data_trees4$temp_02>18),]
tall_sclerophyll_seed <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area < 8 & data_trees4$plant_height>30 & sapply(data_trees4$regen_strategy, FUN=function(X) "fire_killed" %in% X)),]
tall_schlerophyll_resprout <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area < 8 & data_trees4$plant_height>30 & sapply(data_trees4$regen_strategy, FUN=function(X) ("resprouts" %in% X | "basal" %in% X | "basal lignotuber" %in% X | "root_crown" %in% X | "epicormic lignotuber" %in% X | "epicormic" %in% X))),]
med_sclerophyll_seed <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area < 8 & data_trees4$plant_height<=30 & data_trees4$plant_height>10 & sapply(data_trees4$regen_strategy, FUN=function(X) "fire_killed" %in% X)),]
med_schlerophyll_resprout <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area < 8 & data_trees4$plant_height<=30 & data_trees4$plant_height>10 & sapply(data_trees4$regen_strategy, FUN=function(X) ("resprouts" %in% X | "basal" %in% X | "basal lignotuber" %in% X | "root_crown" %in% X | "epicormic lignotuber" %in% X | "epicormic" %in% X))),]
# med_schlerophyll_nutrient 
short_sclerophyll_seed <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area < 8 & data_trees4$plant_height<=10 & sapply(data_trees4$regen_strategy, FUN=function(X) "fire_killed" %in% X)),]
short_schlerophyll_resprout <- data_trees4[which(data_trees4$growth_form=='tree' & data_trees4$specific_leaf_area < 8 & data_trees4$plant_height<=10 & sapply(data_trees4$regen_strategy, FUN=function(X) ("resprouts" %in% X | "basal" %in% X | "basal lignotuber" %in% X | "root_crown" %in% X | "epicormic lignotuber" %in% X | "epicormic" %in% X))),]

### load desired parameters ###_______________________________________________________________________

austraits <- load_austraits()

param_vars <- c("specific_leaf_area","bark_thickness","Vcmax_per_area","Jmax_per_area","germination","leaf_angle","leaf_absorption","leaf_reflectance","leaf_transmission","leaf_CN_ratio",
                "leaf_dark_respiration_per_dry_mass","stem_respiration_per_dry_mass","leaf_hydraulic_conductivity","leaf_hydraulic_vulnerability","leaf_length","leaf_width","leaf_lifespan",
                "leaf_mass_to_stem_mass","leaf_C_per_dry_mass","leaf_N_per_dry_mass","leaf_P_per_dry_mass","wood_C_per_dry_mass","wood_N_per_dry_mass","wood_P_per_dry_mass","leaf_transpiration_at_Amax",
                "stomatal_conductance_per_area_at_Amax","sapwood_specific_conductivity","stem_hydraulic_conductivity","basal_diameter",
                "wood_density")

veg_types <- c("evergreen_cold_mesophyll_tree","evergreen_warm_mesophyll_tree","drought_decid_mesophyll_tree","evergreen_savanna_tree","drought_decid_savanna_tree","tall_sclerophyll_seed",
               "tall_schlerophyll_resprout","med_sclerophyll_seed","med_schlerophyll_resprout","short_sclerophyll_seed","short_schlerophyll_resprout","subalpine_tree","floodplain_sclerophyll",
               "acacia_mesic_midstory","acacia_harpophylla","acacia_mulga","mesic_shrub","xeric_shrub","mesic_shrub_Nfix","xeric_shrub_Nfix","ann_c3_grass","ann_c4_grass","perenn_tussock_c3",
               "perenn_tussock_c4","perenn_hummock_c4")

pars <- matrix(NA,25,length(param_vars))
rownames(pars) <- veg_types
colnames(pars) <- param_vars

find.mean <- function(x){
  u <- mean(x,na.rm=T)
  return(u)}

for (i in 1:length(param_vars)){
  
  param <- param_vars[i]
  
  vals <- extract_trait(austraits, param)
  
  austraits_vals <- data.frame(taxon=vals$traits$taxon_name,var=vals$traits$value)
  austraits_vals <- aggregate(austraits_vals$var,by=list(austraits_vals$taxon),FUN='find.mean')
  colnames(austraits_vals) <- c('taxon',param)
  
  pars[1,i] <- mean(merge(evergreen_cold_mesophyll_tree[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[2,i] <- mean(merge(evergreen_warm_mesophyll_tree[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[3,i] <- mean(merge(drought_decid_mesophyll_tree[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[4,i] <- mean(merge(evergreen_savanna_tree[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[5,i] <- mean(merge(drought_decid_savanna_tree[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[6,i] <- mean(merge(tall_sclerophyll_seed[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[7,i] <- mean(merge(tall_schlerophyll_resprout[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[8,i] <- mean(merge(med_sclerophyll_seed[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[9,i] <- mean(merge(med_schlerophyll_resprout[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[10,i] <- mean(merge(short_sclerophyll_seed[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[11,i] <- mean(merge(short_schlerophyll_resprout[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[12,i] <- mean(merge(subalpine_tree[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[13,i] <- mean(merge(floodplain_sclerophyll[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[14,i] <- mean(merge(acacia_mesic_midstory[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[15,i] <- mean(merge(acacia_harpophylla[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[16,i] <- mean(merge(acacia_mulga[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[17,i] <- mean(merge(mesic_shrub[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[18,i] <- mean(merge(xeric_shrub[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[19,i] <- mean(merge(mesic_shrub_Nfix[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[20,i] <- mean(merge(xeric_shrub_Nfix[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[21,i] <- mean(merge(ann_c3_grass[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[22,i] <- mean(merge(ann_c4_grass[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[23,i] <- mean(merge(perenn_tussock_c3[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[24,i] <- mean(merge(perenn_tussock_c4[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  pars[25,i] <- mean(merge(perenn_hummock_c4[,1:2],austraits_vals, by = 'taxon',all.x=T,all.y=F)[,3],na.rm=T)
  
}








