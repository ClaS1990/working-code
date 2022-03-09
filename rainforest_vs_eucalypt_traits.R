### use rainforest species list from Rachael G to differentiate from eucalypts and other sclerophyllous trees
### check trait distributions to see how we can define groups a priori

setwd("~/Documents/data/aa_PFT_project")

rf_list <- read.csv('rainforest_trees.csv')

library(stringr)

# remotes::install_github("traitecoevo/austraits", build_vignettes = TRUE)
library(austraits)
austraits <- load_austraits()

trait_names <- austraits$traits$trait_name %>% unique()
growth_form <- extract_trait(austraits, "plant_growth_form")
specific_leaf_area <- extract_trait(austraits, "specific_leaf_area") 
leaf_length <- extract_trait(austraits, "leaf_length") 
leaf_width <- extract_trait(austraits, "leaf_width") 
leaf_area <- extract_trait(austraits, "leaf_area") 
snow_tolerance <- extract_trait(austraits, "snow_tolerance") 

### find species average ###

find.mean <- function(x){
  u <- mean(x,na.rm=T) 
return(u)}

find.mode <- function(x){
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
return(u)}

austraits_growth_form <- data.frame(taxon=growth_form$traits$taxon_name,growth_form=growth_form$traits$value)
austraits_growth_form <- aggregate(austraits_growth_form$growth_form,by=list(austraits_growth_form$taxon),FUN='find.mode') # assumes most common value is best
colnames(austraits_growth_form) <- c('taxon','growth_form')

austraits_specific_leaf_area <- data.frame(taxon=specific_leaf_area$traits$taxon_name,specific_leaf_area=specific_leaf_area$traits$value)
austraits_specific_leaf_area <- aggregate(austraits_specific_leaf_area$specific_leaf_area,by=list(austraits_specific_leaf_area$taxon),FUN='find.mean')
colnames(austraits_specific_leaf_area) <- c('taxon','specific_leaf_area')

austraits_leaf_length <- data.frame(taxon=leaf_length$traits$taxon_name,leaf_length=leaf_length$traits$value)
austraits_leaf_length <- aggregate(austraits_leaf_length$leaf_length,by=list(austraits_leaf_length$taxon),FUN='find.mean')
colnames(austraits_leaf_length) <- c('taxon','leaf_length')

austraits_leaf_width <- data.frame(taxon=leaf_width$traits$taxon_name,leaf_width=leaf_width$traits$value)
austraits_leaf_width <- aggregate(austraits_leaf_width$leaf_width,by=list(austraits_leaf_width$taxon),FUN='find.mean')
colnames(austraits_leaf_width) <- c('taxon','leaf_width')

austraits_leaf_area <- data.frame(taxon=leaf_area$traits$taxon_name,leaf_area=leaf_area$traits$value)
austraits_leaf_area <- aggregate(austraits_leaf_area$leaf_area,by=list(austraits_leaf_area$taxon),FUN='find.mean')
colnames(austraits_leaf_area) <- c('taxon','leaf_area')

austraits_snow_tolerance <- data.frame(taxon=snow_tolerance$traits$taxon_name,snow_tolerance=snow_tolerance$traits$value)
austraits_snow_tolerance <- aggregate(austraits_snow_tolerance$snow_tolerance,by=list(austraits_snow_tolerance$taxon),FUN='find.mode') # assumes most common value is best
colnames(austraits_snow_tolerance) <- c('taxon','snow_tolerance')


### create dataset that combines species level data for all variables except snow tolerance (managed separately) ###

base1 <- merge(austraits_growth_form, austraits_specific_leaf_area, by = 'taxon',all.x=T,all.y=T)
base2 <- merge(base1,austraits_leaf_length, by = 'taxon',all.x=T,all.y=T)
base3 <- merge(base2,austraits_leaf_width, by = 'taxon',all.x=T,all.y=T)
austraits_data_comb <- merge(base3,austraits_leaf_area, by = 'taxon',all.x=T,all.y=T)

austraits_data_comb2 <- austraits_data_comb[which(austraits_data_comb$growth_form == 'tree'),-2] # only keep trees
austraits_data_comb3 <- austraits_data_comb2[-which(is.na(rowMeans(austraits_data_comb2[,2:5],na.rm=T))),] # remove rows with no data other than growth form

### check snow tolerance ###

base4 <- merge(austraits_growth_form, austraits_snow_tolerance, by = 'taxon',all.x=T,all.y=T)
snow_tol2 <- base4[which(base4$growth_form == 'tree'),-2] # only keep trees
snow_tol2 <- snow_tol2[-which(is.na(snow_tol2$snow_tolerance)),]

snowtol_rf <- snow_tol2[which(snow_tol2$taxon %in% rf_list$species),]
snowtol_scl <- snow_tol2[-which(snow_tol2$taxon %in% rf_list$species),]

### optional - remove eucalypts ###

austraits_data_comb3 <- austraits_data_comb3[-which(lapply(austraits_data_comb3$taxon,FUN=function(X) (str_split(X,pattern=' ')[[1]][1]))=='Eucalyptus'),]

### split into rainforest and non-rainforest ###

austraits_rf <- austraits_data_comb3[which(austraits_data_comb3$taxon %in% rf_list$species),]
austraits_scl <- austraits_data_comb3[-which(austraits_data_comb3$taxon %in% rf_list$species),]

mycol <- rgb(255, 0, 0, max = 255, alpha = 125, names = "red50")

hist(austraits_rf$specific_leaf_area,col='green',breaks=pretty(0:34,n=34),xlab='Specific leaf area',ylim=c(0,150),main='SLA')
hist(austraits_scl$specific_leaf_area,col=mycol,breaks=pretty(0:34,n=34),add=T)
legend('topright',c('rainforest','sclerophyll'),fill=c('green',mycol))

hist(austraits_rf$leaf_length,col='green',breaks=pretty(0:900,n=90),xlab='Leaf length',ylim=c(0,150),main='Leaf length')
hist(austraits_scl$leaf_length,col=mycol,breaks=pretty(0:900,n=90),add=T)
legend('topright',c('rainforest','sclerophyll'),fill=c('green',mycol))

hist(austraits_rf$leaf_width,col='green',breaks=pretty(0:210,n=42),xlab='Leaf width',ylim=c(0,250),main='Leaf width')
hist(austraits_scl$leaf_width,col=mycol,breaks=pretty(0:210,n=42),add=T)
legend('topright',c('rainforest','sclerophyll'),fill=c('green',mycol))

hist(austraits_rf$leaf_area,col='green',breaks=pretty(0:73000,n=146),xlab='Leaf area',ylim=c(0,100),main='Leaf area',xlim=c(0,20000))
hist(austraits_scl$leaf_area,col=mycol,breaks=pretty(0:73000,n=146),add=T)
legend('topright',c('rainforest','sclerophyll'),fill=c('green',mycol))
