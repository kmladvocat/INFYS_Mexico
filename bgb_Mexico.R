### belowground biomass ### 

tropical_bgb <- function(carbon) {
  exp(-1.0587 + 0.8836 * log(carbon))
}

temperate_bgb<- function(carbon) { 
  exp(-1.0587 + 0.8836 * log(carbon) + 0.2840)
}
 
to_view<-long_data_final %>% 
  mutate(
    climate = source_veg_class_table$climate[match(source_vegetation_class_ES,source_veg_class_table$source_veg_class_std_ES)],
    carbon_Mg = agb_Mgha_1 * 0.47,
    bgb_Mg = ifelse(climate == "Tropical", tropical_bgb(carbon_Mg), ifelse (climate == "Temperate", temperate_bgb(carbon_Mg), NA))
      ) %>% 
  select(-c(forest_type,State,ecoregion_DESECON4, state_std,cve_state,source_veg_class))

write.csv(to_view, 'Mexico_INFYS_including_bgb.csv',row.names=F) 

