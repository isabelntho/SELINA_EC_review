library(dplyr)
library(ggplot2)
library(shinydashboard)
library(DT)
library(knitr)
library(stringr)
library(shinythemes)
library(readxl)
library(openxlsx)
library(kableExtra)
suppressPackageStartupMessages(library(ComplexHeatmap))
library(UpSetR)
library(sf)
library(tidyr)
library(tibble)
library(ggsankey)


#review_drive <- "Y:/EU_BioES_SELINA/WP3/1. T3.2_review/SLR_data/"
#dir <- "Y:/EU_BioES_SELINA/WP3/1. T3.2_review/SLR_data/"
#viz <- paste0(dir, "Review_data_viz/")

world <- st_read("./world-administrative-boundaries/world-administrative-boundaries.shp")
pubs <- read.xlsx("./publications_recoded.xlsx")
#journals <- read.csv(paste0(review_drive, "Review_data_viz/journals.csv"))
inds <- read_excel("./indicators_recoded1.xlsx")
#ds <- read_excel(paste0(dir,"datasets_recoded.xlsx"), na="NA")
#t <- read_xlsx("Y:/EU_BioES_SELINA/WP3/1. T3.2_review/Use_of_indicators-forest.xlsx")
#ds_sankey <- read_excel(paste0(dir,"datasets_recoded.xlsx"), na="NA")
#for sankeys
# ds_sankey <- read_excel("./datasets_recoded.xlsx", na="NA")
# 
#  pubs <- read_excel("./publications_recoded.xlsx")
#  inds <- read_excel("./indicators_recoded1.xlsx")
#  world <- st_read("./world-administrative-boundaries/world-administrative-boundaries.shp")
#  journals <- read.csv("./journals.csv")
#  ds <- read_excel("./datasets_recoded.xlsx", na="NA")
#  t <- read_xlsx("./Use_of_indicators-forest.xlsx")
# 
# P5 <- unlist(strsplit(pubs$P5, "; "))
# P5 <- unlist(strsplit(P5, ", "))
# countries <- as.data.frame(as.matrix(table(P5)))
# 
# P7 <- unlist(strsplit(pubs$P7, ","))
# ET_pubs <- as.data.frame(as.matrix(table(P7)))
# ET_pubs <- tibble::rownames_to_column(ET_pubs, "ET")
# ET_pubs$ET <- factor(ET_pubs$ET, levels=c("Urban", "Cropland", "Grassland",
#                                           "Woodland and forest",
#                                           "Heathland and shrub", "Sparsely vegetated land",
#                                           "Wetlands", "Rivers and lakes",
#                                           "Marine inlets and transitional water", "Coastal","Marine",
#                                           "Other","Unclear/not specified/all"))


## Remove duplicates
alloc <- read.csv("Y:/EU_BioES_SELINA/WP3/1. T3.2_review/SLR_data/completion_220124.csv")
dups <- alloc[duplicated(alloc$src_id),]

pubs_dup <- pubs[pubs$P0%in%dups$src_id,]

pubs_dup_keep <- pubs_dup%>%
  group_by(P0) %>%
  slice_tail()
pubs_dup_rem <- pubs_dup[!(pubs_dup$id%in%pubs_dup_keep$id),]

pubs <- pubs%>%filter(!(id%in%pubs_dup_rem$id))

##

#Remove unnecessary columns and filter to selected publication
pubs <- pubs %>% select(-R2, -R4, -P2)
selected <- pubs %>%
  filter(SQ1=="Yes"&SQ2=="Yes")
#selected <- pubs[!(pubs$P0%in%rejected$P0),]# why is there a difference

#Identify marine publications and remove
selected_marine <- selected[grepl("Marine",selected$P7),]%>%filter(!(id %in% c(150,346)))
selected_coast <- selected[selected$P7=="Coastal",]
selected_mar_coast <- bind_rows(selected_marine, selected_coast)

selected_terr <- selected[!(selected$P0%in%selected_mar_coast$P0),]

pubs_et_overview <- selected_terr
pubs_et_overview$P7<- gsub("Cropland,Grassland", "Agroecosystems", pubs_et_overview$P7)
pubs_et_overview$P7<- gsub("Cropland", "Agroecosystems", pubs_et_overview$P7)
pubs_et_overview$P7<- gsub("Grassland", "Heath- & Grasslands", pubs_et_overview$P7)
pubs_et_overview$P7<- gsub("Heathland and shrub", "Heath- & Grasslands", pubs_et_overview$P7)
#pubs_et_overview$P7[grepl(",", selected_terr$P7)]<- "Multiple"
pubs_et_overview$P7<- gsub("Sparsely vegetated land","Other", pubs_et_overview$P7)
pubs_et_overview$P7<- gsub("Woodland and forest","Forest", pubs_et_overview$P7)
#########

#Identify indicators only applied to a single ecosystem type
selected_terr_nom <- selected_terr%>%filter(P7!="Multiple")
selected_terr <- selected_terr %>%
  mutate(P7 = strsplit(as.character(P7), ",")) %>% 
  unnest(P7)
selected_terr$P7<- gsub("Cropland", "Agroecosystems", selected_terr$P7)
selected_terr$P7<- gsub("Cropland", "Agroecosystems", selected_terr$P7)
selected_terr$P7<- gsub("Grassland", "Heath- & Grasslands", selected_terr$P7)
selected_terr$P7<- gsub("Heathland and shrub", "Heath- & Grasslands", selected_terr$P7)
selected_terr$P7<- gsub("Sparsely vegetated land","Other", selected_terr$P7)
selected_terr$P7<- gsub("Woodland and forest", "Forest", selected_terr$P7)
#withvar$P7[str_count(withvar$P7,",")>2]<- "Various"
inds_terr <- inds%>%filter(survey_id%in%selected_terr$id)

write.xlsx(inds_terr, paste0(review_drive,"indicators_recoded_selected.xlsx"), rowNames=F)

inds_terr <- inds_terr %>% 
  mutate(M1_recoded = coalesce(M1_recoded,M1))
inds_terr <- inds_terr %>% 
  mutate(M3_reclassified = coalesce(M3_reclassified,M3))
inds_terr <- inds_terr %>% filter(!(M2%in%c("Composite EC index", "Composite indicator")))

indicators_terr <- inds_terr

multiple_inds <- inds_terr %>%
  add_count(M1_recoded) %>% 
  filter(n!=1) %>%
  select(-n)
length(unique(multiple_inds$M1_recoded))
#convert cropland/grassland to Agroecosystems
indicators_terr <- indicators_terr %>%
  mutate(M4 = strsplit(as.character(M4), ",")) %>% 
  unnest(M4)

indicators_terr$M4<- gsub("Cropland,Grassland", "Agroecosystems", indicators_terr$M4)
indicators_terr$M4<- gsub("Cropland", "Agroecosystems", indicators_terr$M4)
indicators_terr$M4<- gsub("Grassland", "Heath- & Grasslands", indicators_terr$M4)
indicators_terr$M4<- gsub("Heathland and shrub", "Heath- & Grasslands", indicators_terr$M4)
indicators_terr$M4[indicators_terr$M4=="Sparsely vegetated land"]<-"Other"
indicators_terr$M4[indicators_terr$M4=="Woodland and forest"]<-"Forest"

top_10 <- indicators_terr%>%group_by(M3_reclassified, M1_recoded)%>%summarise(freq=n())%>%
  ungroup()%>%group_by(M3_reclassified)%>% slice_max(order_by=freq,n = 10)
#write.csv(top_10, paste0(dir, "top_inds_class.csv"), row.names=F)





ds_sel <- ds_sankey %>% filter(survey_id %in%selected_terr$id)
ds_sel <- left_join(ds_sel, indicators_terr, by=c("indicator_id"="id"))
ds_type <- c("Land use or land cover dataset (e.g. CORINE, national land use map)" , "Other, specify:" , "Biodiversity index (e.g. red list index, LPI, not calculated by publication)",                                                                                                                                                                                                             
             "Remote sensing data product (e.g. aerial photography, LiDAR, MODIS or Landsat data product)","Expert opinion, e.g. definition of values by stakeholders" ,
             "Socio-economic data (population, GDP etc)" , "Soil, topography, geological or lithology data (e.g. DEM, soil parameters)" ,"Meteorological/climate data","Measurement/field data")
ds_sel <- ds_sel%>%filter(S11%in%ds_type)

ds_sel <- ds_sel %>% 
  mutate(S14_recoded = coalesce(S14_recoded,S14))

ds_sel$S16_end<- as.numeric(ds_sel$S16_end)
ds_sel$S16_st<- as.numeric(ds_sel$S16_st)
ds_sel$S16_len <- ds_sel$S16_end - ds_sel$S16_st
ds_sel <- ds_sel %>% 
  mutate(S16_len = coalesce(S16_len,S16_st))
ds_sel$S16_len[ds_sel$S16_len>1900]<-1

ect_by_prim <- ds_sel%>%
  group_by(M3_reclassified, S11)%>%
  count()

ggplot(data=ect_by_prim, aes(x=S11, y=n, fill=M3_reclassified))+
  geom_bar(stat="identity", position="dodge")+
  coord_flip()

####

meas <- ds_sel%>%filter(S11=="Measurement/field data")%>%filter(S12=="Secondary")

point_data <- ds_sel[(grepl("point|vector|polygon", ds_sel$S14_recoded)),]
point_data <- point_data %>% group_by(S2)%>%summarise(freq=n())
ggplot(data=point_data, aes(x=S2, y=freq,))+
  geom_bar(stat="identity", position="dodge")+
  coord_flip()


####


#indicators <- indicators_terr[!grepl(",", indicators_terr$M4),]

# 
# 
# indicators <- inds[!is.na(inds$M1_recoded),]
# 
# sub <- indicators%>%
#   filter(M2!="Composite indicator")%>%
#   group_by(M1_recoded)%>%
#   mutate(n_study = n_distinct(survey_id))
# 
# multi_paper <- sub %>%
#   filter(n_study > 1)
# 
# ind_sub <- indicators[indicators$M1_recoded%in%multi_paper$M1_recoded,]
# 
# tbl_ind <- datatable(ind_sub%>%
#                        filter(M2!="Composite indicator")%>%
#                        group_by(M1_recoded)%>%
#                        dplyr::count()%>%
#                        filter(n>1)%>%
#                        left_join(select(sub, c(M1_recoded,n_study)), by="M1_recoded", multiple="first")%>%
#                        arrange(-n),
#                      rownames= FALSE)
# 
# tbl_nonECT <- indicators%>%
#   filter(!is.na(M3_justification))%>%
#   dplyr::count(M3_justification)
# 
# inds <- inds[inds$survey_id%in%selected_terrest$id,]
# comp_inds <- inds[inds$M2=="Composite indicator",]
# inds <- inds[inds$M2!="Composite indicator",]
# 
# ets_list <- unlist(strsplit(inds$M4, ","))
# 
# yr <- selected_terrest %>%
#   count(P3)
# 
# inds_pubs <- inds %>% 
#   group_by(survey_id)%>%
#   summarise(n=n())%>%
#   drop_na()%>%
#   count(n)
# 
# #### Journals ####
# 
# #journals <- sel_orig %>%
# #count(journal)
# 
# top_journals <- journals %>% arrange(-n) %>% filter(n>3)
# top_journals$journal <- gsub("\\(Switzerland\\)", "", top_journals$journal)
# top_journals$journal <- gsub("International Archives of the Photogrammetry, Remote Sensing and Spatial Information Sciences - ISPRS Archives", "ISPRS Archives", top_journals$journal)	
# names(top_journals)<- c("Journal", "Count")
# 
# #####
# 

 
#world_vals$cat <- cut(world_vals$n, breaks=(c(0,1,5,15,30,max(world_vals$n, na.rm=T))))
#world_vals <- world_vals[!(is.na(world_vals$cat)),]
#world_vals$cat <- gsub("\\(|]", "", world_vals$cat)
#world_vals$cat <- gsub(",", " - ", world_vals$cat)
#world_vals$cat <- factor(world_vals$cat)
#levels(world_vals$cat)[4]<- "> 30" 
#world_vals$cat <- factor(world_vals$cat, levels=c("0 - 1","1 - 5","5 - 15","15 - 30","> 30"))

# 
# #####
# 
# inds_et <- inds %>%
#   mutate(M4 = strsplit(as.character(M4), ",")) %>% 
#   unnest(M4)
# 
# m3_uniqueM1 <- inds_et %>%
#   group_by(M4)%>%
#   distinct(M1_recoded, .keep_all = TRUE)%>%
#   count(M4, M3_reclassified)
# 
# ect_colours <- c('white','#a6cee3','#1f78b4','white','#b2df8a','#33a02c','#005a32','white','#feb24c', "white",'grey')
# 
# m3_uniqueM1$M4 <- factor(m3_uniqueM1$M4, levels=c("Urban", "Cropland", "Grassland", "Woodland and forest", "Heathland and shrub", "Sparsely vegetated land",
#                                                   "Wetlands", "Rivers and lakes", "Other", "Unclear/not specified"))
# m3_uniqueM1$M3_reclassified <- factor(m3_uniqueM1$M3_reclassified, levels=c("Abiotic", "A1: Physical state characteristics" , "A2: Chemical state characteristics", "Biotic","B1: Compositional state characteristics", "B2: Structural state characteristics","B3: Functional state characteristics",
#                                                                             "Landscape", "C1: Landscape and seascape characteristics", "","X: No appropriate ECT class"))
# m3_uniqueM1 <- m3_uniqueM1[!is.na(m3_uniqueM1$M3_reclassified),]
# 
# 
ect_labs <- c("A1: Physical state characteristics" , "A2: Chemical state characteristics","B1: Compositional state characteristics", "B2: Structural state characteristics","B3: Functional state characteristics",
               "C1: Landscape and seascape characteristics")

non_ect_labs <- c("Pressures", "Ecosystem extent","Stable environmental characteristics",
             "Ecosystem services","Other", "Natural resource management", "Accessibility",
             "Embedded subtypes")

all_labs <- c(ect_labs, non_ect_labs)
