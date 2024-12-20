########### Mexico National Inventory census 1 ###################
#set local working directory#
setwd("/Users/kmelgaco/Documents/Rfile")#set working directory

### loading packages #####
library(tidyverse)
library(dplyr)
library(BIOMASS)
library(stringi)
library(stringr)

#### Read files ####
mexico_c1 <- read.csv("INFyS_c1_2004_2007.csv",h=T)
mexico_c1_fire<-read.csv('fire_census1.csv',h=T)
mexico_c1_impactos<-read.csv('impactos_c1.csv',h=T)
mexico_c1_metadata <- read.csv('metadata_conglomerados_census1.csv',h=T)
mexico_c3 <- read.csv("INFyS_c3_2015_2020.csv")
disturbances_c1<-read.csv("Disturbances_census1.csv",h=T)

### Select relevant columns ### 
mexico_census1 <- mexico_c1 %>% 
  select( 
    Conglomerado:Estado,
    Formato:Tipo_veg_SV,
    Sitio,
    Arbol,
    Familia_APG:Condicion, 
    X, 
    Y,
    Edad, 
    NombreCientifico_APG,
    Diametro_normal,
    Edad, 
    Altura_total
  ) %>% 
  #create convert diameter from cm to mm and create source code
  mutate(
    #DBH_original = Diametro_normal * 10,
    std_Estado = Estado %>% stri_trans_general("Latin-ASCII") %>%  str_replace_all(" ", "_") %>% tolower,
    source_code = paste(Conglomerado,state_id$Cve_Estado_C3 [match (std_Estado, state_id$std_State_name)],sep="_"),
    Height1 = ifelse(Altura_total > 100, NA, Altura_total),
    stand_age = ifelse (Edad = 0, NA, Edad)
  ) %>%
  #rename columns to standardised names 
  rename(
    condition1 = Condicion,
    long = X,
    lat = Y,
    tag_number = Arbol)

#### standardizing species names ###########
std_species <- mexico_census1 %>%
  separate(NombreCientifico_APG, into = c("Genus", "Species"), remove = FALSE) %>%
  mutate(Species = ifelse(is.na(Species), "sp.", Species),
         Full_Species = paste (Genus, Species, sep = " "), # to remove var. determinations from the full_species list
         Full_Species = sub("Aa", "Indet sp.", Full_Species))

###### Assign wood density values according to genus, species or Family identification #######################
## Use the BIOMASS package to obtain the wood density from the Zanne et al wood density database
Census_wd <- getWoodDensity(
  genus = std_species$Genus,
  species = std_species$Species,
  family = std_species$Familia_APG,
  region = "Mexico",
  verbose = TRUE)

Census_wd$full_species<-paste(Census_wd$genus,Census_wd$species,sep=" ")

# Create new columns by matching the Full species name
std_species$wood_density_dryad <- Census_wd$meanWD[match(std_species$Full_Species, Census_wd$full_species)]
std_species$level_wood_density_dryad <- Census_wd$levelWD[match(std_species$Full_Species, Census_wd$full_species)]

std_species$level_wood_density_dryad <- ifelse(
  is.na(std_species$level_wood_density_dryad),
  "mean_mexico",
  std_species$level_wood_density_dryad
)

std_species$wood_density_dryad <- ifelse(is.na(std_species$wood_density_dryad),0.48,
  std_species$wood_density_dryad)

# Assign the wood density values using values from the Ricker et al. 2024 (wd_paper object)
# Calculate the average wood density for each combination of UPMID and Species levels in wd_paper
wd_paper<- wd_ppr %>% 
  mutate(
    state_id_c1 = state_id$Cve_Estado_C3 [match (wd_ppr$std_Estado, state_id$std_State_name)],
    source_code = upmid_plotid$source_code [match (wd_ppr$UPMID.SITE.CODE, upmid_plotid$UPMID)] 
  ) %>% 
  rename(
    wd = BASIC.WOOD.DENSITY..g.cm3.,
    Full_Species = SPECIES,
    Genus = GENUS,
    Familia_APG = FAMILY,
  ) %>% 
  filter (
    !is.na(source_code)
  )

species_mean_density <- wd_paper %>%
  group_by(
    source_code,  Full_Species
  ) %>%
  summarize(
    mean_wood_density = mean( wd, na.rm = TRUE), .groups = 'drop'
  )

# Join species-level mean wood density with mexico_census1_tree
updated_data <- std_species %>%
  left_join(
    species_mean_density, by = c("source_code", "Full_Species")
  )

# Calculate genus-level matching where species match failed
genus_mean_density <- wd_paper %>%
  group_by(
    source_code, Genus
  ) %>%
  summarize(
    mean_wood_density = mean(wd, na.rm = TRUE), .groups = 'drop')

updated_data <- updated_data %>%
  left_join(
    genus_mean_density, by = c("source_code", "Genus")
  ) %>%
  mutate(
    wood_density = ifelse(is.na(mean_wood_density.x), mean_wood_density.y, mean_wood_density.x)
  ) %>%
  select(-mean_wood_density.x, -mean_wood_density.y)

# calculate family-level matching where both species and genus match failed
family_mean_density <- wd_paper %>%
  group_by(
    source_code, Familia_APG) %>%
  summarize(
    mean_wood_density = mean(wd, na.rm = TRUE), .groups = 'drop'
  )

updated_data <- updated_data %>%
  left_join(
    family_mean_density, by = c("source_code", "Familia_APG")
  ) %>%
  mutate(
    wood_density = ifelse(is.na(wood_density), mean_wood_density, wood_density)
  ) %>%
  select(-mean_wood_density)

# The 'updated_data' now has the wood densities from wd_paper and is assigned to mexico_census1_tree
std_species<-updated_data %>% 
  mutate(
    wood_density_merged = ifelse(!is.na(updated_data$wood_density), 
                                 updated_data$wood_density,
                                 updated_data$wood_density_dryad),
    level_WD_merged=ifelse(is.na(wood_density),
                           updated_data$level_wood_density_dryad,
                           "mexico_wd_db")
  )



std_species$agb_tree_kg<- round(as.numeric(EQ_016(std_species$Diametro_normal,
    std_species$wood_density_merged,
    std_species$Height1)),2)

std_species$AGB_eq1<-"EQ_016"

mexico_census1_tree<-std_species %>% 
  select(-c(Conglomerado,
            wood_density,
            level_wood_density_dryad,
            cgl_sit_arb,
            Cve_veg_SV,
            wood_density_dryad,
            NombreCientifico_APG,
            Genus, 
            Species)) %>% 
  rename(
    census_date=Anio,
    subplot1=Sitio,
    Family=Familia_APG,
    wood_density=wood_density_merged,
    level_WD=level_WD_merged,
    Species=Full_Species
  ) %>% 
  relocate(
    source_code,
    lat,
    long,
    tag_number,
    Family,
    Species,
    agb_tree_kg,
    AGB_eq1
  )

####### Metadata census 1 ###########################
mexico_census1_fire<-mexico_c1_fire %>% 
  select(Conglomerado, 
         Anio, 
         Estado, 
         Anio_Incendio,
         TipoIncendio
  ) %>% 
  mutate(
    std_Estado = Estado %>% stri_trans_general("Latin-ASCII") %>%  str_replace_all(" ", "_") %>% tolower,
    source_code = paste(Conglomerado, state_id$Cve_Estado_C3 [match (std_Estado, state_id$std_State_name)],sep = "_"))

mexico_census1_impactos<- mexico_c1_impactos %>% 
  mutate(
    std_Estado = Estado %>% stri_trans_general("Latin-ASCII") %>%  str_replace_all(" ", "_") %>% tolower,
    source_code = paste(Conglomerado, state_id$Cve_Estado_C3 [match (std_Estado, state_id$std_State_name)],sep = "_")) %>% 
  full_join(mexico_census1_fire,by = "source_code") 

mexico_census1_meta <- mexico_c1_metadata %>% 
  mutate(
    std_Estado = Estado %>% stri_trans_general("Latin-ASCII") %>%  str_replace_all(" ", "_") %>% tolower,
    source_code = paste(Conglomerado, state_id$Cve_Estado_C3 [match (std_Estado, state_id$std_State_name)],sep = "_"),
    forest_type = tolower (Tipo_veg_SV)
  ) %>% 
  select(Conglomerado:Formato,
         Monitoreo:Tenencia,
         Tipo_veg_SV,
         X,
         Y,
         source_code,
         std_Estado
  ) %>% 
  full_join(mexico_census1_impactos,by="source_code")


mexico_census1_metadata <- mexico_census1_meta %>%
  mutate(
    source_veg_fire = ifelse (Causa == 'Incendios', paste(Causa, TipoIncendio, sep="_"),Causa),
    Causa_EN = disturbances_c1$Distubances_en[match(source_veg_fire,disturbances_c1$Impactos_ambientales)],
         ) %>% 
  group_by(source_code, Tenencia,Anio_Incendio,Anio) %>% 
  summarise(
    # concatenate unique Causa values with a ";" separator
    source_vegetation_status_ES = paste(unique(source_veg_fire), collapse = ";"),
    source_vegetation_status = paste (unique(Causa_EN), collapse = ";") ,
    .groups = "drop"
  ) %>% 
  ungroup()

###### Handle tree level data and metatadata #####################

mexico_c1 <- mexico_census1_tree %>% 
  mutate(std_age = ifelse(stand_age == 'NULL', NA, stand_age),  # Replace NULL with NA
         std_age = as.numeric(std_age)) %>%  # Convert to numeric, NA remains NA
  group_by(source_code, AGB_eq1, census_date, Tipo_veg_SV, Estado, std_Estado) %>% 
  summarise(
    subplot_count = n_distinct(subplot1),
    agb_plot = round(sum(agb_tree_kg, na.rm = TRUE) / 1000, 2),
    stand_age = round(mean (std_age, na.rm = TRUE),2),
    Height = round(mean (Height1, na.rm = TRUE),2),
    .groups = "drop" 
  ) %>% 
  mutate(
    plot_area_ha = case_when(
      subplot_count == 1 ~ round(area_per_hectare_sqm / 4, 2),
      subplot_count == 2 ~ round(area_per_hectare_sqm / 2, 2),
      subplot_count == 3 ~ round(area_per_hectare_sqm / 1.3, 2),
      TRUE ~ area_per_hectare_sqm
    ),
    agb_Mgha_1 = round(agb_plot / plot_area_ha, 2),
    source_veg_class = tolower(Tipo_veg_SV)
  )

## Merge with plots metadata #####
##Obtain unique coordinates

mexico_1_coordinates <- mexico_census1_tree %>%
  group_by(source_code) %>%
  filter(subplot1 == min(subplot1, na.rm = TRUE)) %>% #selects the min subplot to obtain the coordinates from
  slice(1) %>%  # ensures one row per source_code
  ungroup() %>%
  select(source_code, lat, long) %>%
  distinct(source_code, .keep_all = TRUE)

merged_files_census1 <- mexico_c1 %>% 
  left_join(mexico_census1_metadata, by = "source_code") %>%
  left_join(mexico_1_coordinates, by = "source_code") %>% 
  mutate(
    source_vegetation_class = source_veg_class_table$source_veg_class_EN[match(mexico_c1$source_veg_class, source_veg_class_table$source_veg_class_std_ES)],
    source_vegetation_status_year = ifelse(
      Anio_Incendio == "Año actual", Anio, 
      ifelse(Anio_Incendio == "Años anteriores", NA, Anio_Incendio)),
    land_ownership = land_owner$land_owner_EN[match(Tenencia, land_owner$land_owner_ES)],
    Cve_Estado_C3 = state_id$Cve_Estado_C3[match(mexico_c1$std_Estado, state_id$std_State_name)]
  ) %>% 
  select(-c(Anio_Incendio,Anio,Tipo_veg_SV, agb_plot))


### Create the collection code column for each State### 
#Loop through each State to create the collection column, considering State 1 as COL211 ####

results <- numeric(length(merged_files_census1$Cve_Estado_C3))

for (i in 1:length(merged_files_census1$Cve_Estado_C3)) {
  if (is.na(merged_files_census1$Cve_Estado_C3[i])) {
    results[i] <- NA  # Assign NA if the value is missing
  } else if (merged_files_census1$Cve_Estado_C3[i] == 1) {
    results[i] <- paste("COL", as.numeric(212), sep = "")
  } else if (merged_files_census1$Cve_Estado_C3[i] >= 2 & merged_files_census1$Cve_Estado_C3[i] < 33) {
    # Calculate the additional value based on the index (statecd - 1)
    additional_value <- paste("COL", (213 + (merged_files_census1$Cve_Estado_C3[i] - 1)), sep = "")
    results[i] <- additional_value
  } else {
    results[i] <- NA
  }
}

merged_files_census1$collection_code <- as.character(results)  
mexico_census1_plot <- merged_files_census1 %>%  
  group_by(collection_code) %>% 
  mutate(source_group_num = dense_rank(source_code)
  ) %>%  # Create a grouping number for each source_code within collection_code
  group_by(collection_code, source_code) %>%  
  mutate ( source_code=paste("INFYS",source_code,sep="_") )%>% 
  select(-c(Cve_Estado_C3,
            source_group_num,
            Estado,
            std_Estado,
            Tenencia,
            subplot_count)
  ) %>% 
  relocate(
    collection_code,
    source_code,
    lat,
    long,
    census_date, 
    agb_Mgha_1,
    AGB_eq1, 
    source_vegetation_class,
    source_veg_class
  ) %>% 
  rename(source_vegetation_class_ES = source_veg_class)

## Create final output ####
write.csv(mexico_census1_plot,"Mexico_Census1_PLOT.csv",row.names=F)

