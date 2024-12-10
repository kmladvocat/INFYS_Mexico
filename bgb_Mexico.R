### belowground biomass ### 
source_vegetation_table_climate<-read.csv('source_vegetation_class2.csv', h = T) %>% 
  select (climate.classification, Forest.Type..Spanish.)

source_veg_class_table<-merge(source_veg_class_table,source_vegetation_table_climate,by.x="std_source_veg_class_ES",by.y="Forest.Type..Spanish.")


tropical_bgb <- function(carbon) {
  exp(-1.0587 + 0.8836 * log(carbon))
}

temperate_bgb<- function(carbon) { 
  exp(-1.0587 + 0.8836 * log(carbon) + 0.2840)
}
 
to_view<-long_data_final %>% 
  mutate(
    climate = source_veg_class_table$climate.classification[match(source_vegetation_class_ES,source_veg_class_table$std_source_veg_class_ES)],
    carbon_Mg = agb_Mgha_1 * 0.47,
    bgb_Mg = ifelse(climate == "Tropical", tropical_bgb(carbon_Mg), ifelse (climate == "Temperate", temperate_bgb(carbon_Mg), NA))
      )

write.csv(to_view, 'bgb_Mexico_INFYS.csv',row.names=F) 

