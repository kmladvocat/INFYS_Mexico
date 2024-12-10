setwd("/Users/kmelgaco/Documents/Rfile")#set working directory

library(tidyverse)
library(dplyr)
library(BIOMASS)
library(stringi)


mexico_census1_plot<-read.csv("Mexico_Census1_PLOT.csv", h = TRUE);nrow(mexico_census1_plot)
mexico_census2_plot<-read.csv("Mexico_Census2_PLOT.csv", h = TRUE);nrow(mexico_census2_plot)
mexico_census3_plot<-read.csv("Mexico_Census3_PLOT.csv", h = TRUE) ;nrow(mexico_census3_plot)
metadata_joined_v2 <- read.csv('Joined_tables_v2.csv', h = TRUE)
source_vegetation_table_climate<-read.csv('source_vegetation_class2.csv', h = T) %>% 
  select (climate.classification, Forest.Type..Spanish.)
source_veg_class_table<-read.csv("source_veg_class.csv",h=T) 

###### std source_veg_class
source_veg_class_table$std_source_veg_class_ES <- tolower( str_replace_all(source_veg_class_table$Forest.Type..Spanish., "\u00A0", " "))
source_veg_class_table$std_source_veg_class <- tolower( str_replace_all(source_veg_class_table$Forest.Type..English., "\u00A0", " "))

source_veg_class_table<-merge(source_veg_class_table,source_vegetation_table_climate,by.x="std_source_veg_class_ES",by.y="Forest.Type..Spanish.")

#### Join metadata ###
metadata_joined <- metadata_joined_v2 %>%
  select(source_code, forest_type, State, ecoregion_DESECON4) %>%
  mutate(
    state_std = State %>% tolower() %>% stri_trans_general("Latin-ASCII"),
    cve_state = state_id$Cve_Estado_C3[match(State, state_id$Estado_C3)],
    source_veg_class = forest_type %>% tolower() %>% stri_trans_general("Latin-ASCII")
  ) %>%
  distinct(source_code, .keep_all = TRUE)


merging_census1_2 <- mexico_census1_plot %>% 
  full_join(mexico_census2_plot, by = "source_code") %>%  # 19,454 plots
  full_join(mexico_census3_plot, by = "source_code") %>% 
 rename_with(~ str_replace_all(., "\\.x$", ".c1"), everything()) %>%  # Replace .x with .c1 for Census 1
 rename_with(~ str_replace_all(., "\\.y$", ".c2"), everything()) %>%  # Replace .y with .c2 for Census 2
 rename_with(~ if_else(!str_detect(., "\\."), paste0(., ".c3"), .), everything()) %>%   # Add .c3 to Census 3 columns
 mutate(latitude = ifelse(!is.na(lat.c3), lat.c3, ifelse(!is.na(lat.c2), lat.c2, lat.c1)),
         longitude = ifelse(!is.na(long.c3), long.c3, ifelse(!is.na(long.c2), long.c2, long.c1)))

### Pivot longer table ####
see_check <- merging_census1_2 %>% 
  pivot_longer(
    cols = c(starts_with("agb_Mgha_1."),
             starts_with("agb2_Mgha_1."),
             starts_with("census_date."),
             starts_with("source_vegetation_class."),
             starts_with("source_vegetation_class_ES."),
             starts_with("Height."),
             starts_with("land_ownership."),
             starts_with("plot_area_ha."),
             starts_with('source_vegetation_status.'),
             starts_with('source_vegetation_status_ES.'),
             starts_with('source_vegetation_status_year.'),
             starts_with('AGB_eq1.'),
             starts_with('AGB_eq2.'),
             starts_with('stand_age.'),
             starts_with('subplot_count.'),
             starts_with('UMAFOR.'),
             starts_with('UPMID.'),
             starts_with('ecoregion.')
                 ), 
    names_to = c(".value", "census"),
    names_pattern = "(.*)\\.(c\\d)"
  ) %>% 
  arrange(source_code.c3, census_date) # Ensure order by plot and census

long_data <- see_check %>%
  group_by(source_code.c3) %>%
  mutate(
    source_vegetation_class = tolower(str_replace_all(source_vegetation_class,"\u00A0", " ")),
    source_vegetation_class_ES = tolower(str_replace_all(source_vegetation_class_ES,"\u00A0", " ")),
    collection_code = coalesce(collection_code.c1,collection_code.c2,collection_code.c3),
    year_diff = as.numeric(census_date - lag(census_date)),
    agb_change = round(as.numeric((agb_Mgha_1 - lag(agb_Mgha_1))/year_diff),2),
    total_year_diff = as.numeric(census_date - first(census_date)),
    agb_change_total = (agb_Mgha_1 - first(agb_Mgha_1))/total_year_diff
  ) %>%
  ungroup() %>% 
  filter (!is.na(agb_Mgha_1)) 

pivot_data_mexico<-
  long_data %>%  
  group_by(collection_code) %>% 
  mutate(source_group_num = dense_rank(source_code.c3)
  ) %>%  # Create a grouping number for each source_code within collection_code
  group_by(collection_code, source_code.c3) %>%  
  mutate(
    plot_code_temp = paste("PL", sprintf('%04d', source_group_num), sep = ""),
    plot_code=paste(collection_code,plot_code_temp,sep="_"),
    minimum_dbh_plot = 75,
    country = "Mexico",
    stem_level_available = 1,
    usage_policy = 0, 
  ) %>% 
  select(-c(plot_code_temp,
            std_Estado.c3,
            collection_code.c1,
            collection_code.c2,
            collection_code.c3,
            #plot_code.c2,
            source_group_num,
            lat.c1,
            lat.c2,
            lat.c3,
            long.c1,
            long.c2,
            long.c3,
            country.c3,
            plot_code.c3,
            #plot_code.c2,
            stem_level_available.c3,
            usage_policy.c3,
            minimum_dbh_plot.c3,
            total_year_diff,
            agb_change_total, 
            #disturbance.c3
            )
         ) %>% 
  rename(source_code = source_code.c3,
         lat = latitude,
         long = longitude,
         which_census= census) %>% 
  relocate(
    collection_code,
    plot_code,
    source_code)

long_data_final <- pivot_data_mexico %>%
  mutate(
    source_vegetation_class = coalesce(
      source_veg_class_table$std_source_veg_class[match(source_vegetation_class_ES, source_veg_class_table$std_source_veg_class_ES)],
      source_vegetation_class_ES
    )
  ) %>% 
  select(-c(Cve_Estado_C3.c3,DESCRIP_S7_C3.c3,Altitud_C3.c3,subplot_count))


write.csv(long_data_final,"INFYS_PLOT_to_check_test.csv",row.names = F)         


### checking change of landownership and vegetation class ### 

view <- long_data_2t %>% 
  group_by(source_code) %>% 
  mutate(
    source_forest_type_ES = paste(
      unique(
        Filter(Negate(is.null), na.omit(c(source_vegetation_class_ES.x, source_vegetation_class_ES.y, source_vegetation_class_ES)))
      ),
      collapse = "/"
    ),
    source_forest_type = paste(
      unique(
        Filter(Negate(is.null), na.omit(c(source_vegetation_class.x, source_vegetation_class.y, source_vegetation_class)))
      ),
      collapse = "/"
    ),
    source_land_ownership = paste(
      unique(
        Filter(Negate(is.null), na.omit(c(land_ownership.x, land_ownership.y, land_ownership)))
      ),
      collapse = "/"
    )
  )


write.csv(view,"checks.Mexico10.csv",row.names=F)
