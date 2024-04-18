library(sf)
library(dplyr)
library(tidyr)
library(plotly)

#dir <- "Y:/EU_BioES_SELINA/WP3/1. T3.2_review/SLR_data/"
#viz <- paste0(dir, "Review_data_viz/")
source("./data_manip.R")

### Map of publications
world <- st_read(paste0(viz, "world-administrative-boundaries/world-administrative-boundaries.shp"))

world_map_et <- function(ET, x){
  selected_terr <- selected_terr[selected_terr$P0 %in% x$P0,]
  countries <- selected_terr %>% select(P0,P7,P5_recoded)
  countries <- countries %>% 
    mutate(P5_recoded = strsplit(as.character(P5_recoded), "; ")) %>% 
    unnest(P5_recoded)
  country_sum <- countries %>%
    group_by(P5_recoded)%>%
    summarise(all= n())
  
  countries_by_ET <- countries %>%
    group_by(P7)%>%
    count(P5_recoded)%>%
    pivot_wider(names_from=P7, values_from=n)
  country_sum <- left_join(country_sum, countries_by_ET, by="P5_recoded")
  
  world$name <- case_when(world$name=="U.K. of Great Britain and Northern Ireland"~"United Kingdom",
                          world$name=="United States of America"~"USA",
                          world$name=="Iran (Islamic Republic of)"~"Iran",
                          world$name=="United Republic of Tanzania"~"Tanzania",
                          world$name=="Russian Federation"~"Russia",
                          .default=world$name)
  
  world_vals <- left_join(world,country_sum, by=c("name"="P5_recoded"))
  
  new_vals <- world_vals
  names(new_vals)[names(new_vals)==ET]<-"n"
  
  new_vals$cat <- cut(new_vals$n, breaks=(c(0,1,5,10,20,max(new_vals$n, na.rm=T))))
  new_vals <- new_vals[!(is.na(new_vals$cat)),]
  levels(new_vals$cat) <- gsub("\\(|]", "", levels(new_vals$cat))
  levels(new_vals$cat) <- gsub(",", " - ", levels(new_vals$cat))
  levels(new_vals$cat)[5]<- "> 20"
  levels(new_vals$cat)[1]<- "1"
  
  pub_map <- ggplot()+
    geom_sf(data=world_vals, aes(), fill="#cccccc", color=NA)+
    geom_sf(data=new_vals, aes(fill=cat), color=NA)+
    scale_fill_manual(values = c('#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494', 'black'), name="Number of publications", drop=FALSE)+
    theme_minimal()+
    theme(panel.grid.major=element_blank(),
          axis.text = element_blank(),
          legend.position=c(0.1,0.2))
  pub_map <- ggplotly(pub_map)%>% layout(height = 500, width = 800)
  pub_map
}

### Indicators by ECT ####

ect_cols<- rev(c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#005a32','#feb24c'))

ect_graph_et <- function(ET, x){
  indicators_ect <- x
  indicators_ect$M3_reclassified<- factor(indicators_ect$M3_reclassified, levels=rev(ect_labs))
  ect_cols <-setNames(ect_cols, levels(indicators_ect$M3_reclassified))
  indicators_ect <- indicators_ect[!is.na(indicators_ect$M3_reclassified),]
  unique_ect <- indicators_ect %>% group_by(M4)%>%
    distinct(M1_recoded, .keep_all=TRUE)
  indicators_ect$unique[indicators_ect$id%in%unique_ect$id]<-"Unique"
  indicators_ect$unique[!(indicators_ect$id%in%unique_ect$id)]<-"Total"

    ggplot(filter(indicators_ect, M4==ET), aes(x=M3_reclassified,fill=M3_reclassified, alpha=unique))+
    geom_bar(width=0.7)+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25), drop=FALSE)+
    scale_fill_manual(values=ect_cols, drop=FALSE, guide="none")+
    scale_alpha_discrete(range=c(0.6,1), name="")+
    theme_minimal()+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    labs(x="", y="Number of indicators")+
    coord_flip()
}
