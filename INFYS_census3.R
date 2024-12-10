########### Mexico National Inventory census 3 ###################
#set local working directory#
setwd("/Users/kmelgaco/Documents/Rfile")#set working directory

### loading packages #####
library(tidyverse)
library(dplyr)
library(BIOMASS)
library(stringi)

#### Read files ####
mexico_c3 <- read.csv("INFyS_c3_2015_2020.csv", h = T)
mexico_c3_metadata<-read.csv("metadata_INFyS_Secciones_2015-2020_fRBymGR.csv",h=T)
disturbances_c3<-read.csv("Disturbances_census3.csv",h=T)

#### Select relevant columns and create new ones ###
mexico_census3 <- mexico_c3 %>% 
  select("Cve_Estado_C3",
         "Estado_C3",
         "UPMID":"Anio_C3",
         "DESECON4_C3",
         "X_C3":"FORM_S7_C3",
         "Sitio_C3","NoIndividuo_C3",
         'NombreCientifico_APG_C3',
         "NoRama_C3",
         "Familia_APG_C3",
         "Genero_APG_C3",
         "Especie_APG_C3",
         "DiametroNormal_C3", 
         "AlturaTotal_C3",
         "biomasa_kg_C3",
         "carbono_kg_C3",
         'Condicion_C3','MuertoPie_C3',
         'X_C3',"Y_C3","Edad_C3",
    starts_with("AgenteDanio")
  ) %>%  
 mutate(DBH_original=DiametroNormal_C3*10,
        std_Estado = Estado_C3 %>% stri_trans_general("Latin-ASCII") %>%  str_replace_all(" ", "_") %>% tolower,
        source_code = paste(IdConglomerado,state_id$Cve_Estado_C3 [match (std_Estado, state_id$std_State_name)], sep = "_")
        ) %>%                              
  group_by(source_code) %>% 
  mutate(subplot_count = n_distinct(Sitio_C3)) %>%
  filter(!is.na(DiametroNormal_C3)) %>% 
  ungroup()

#### standardise species 
std_species <- mexico_census3 %>%
  separate(NombreCientifico_APG_C3, into = c("Genus", "Species"), remove = FALSE) %>%
  mutate(Species = ifelse(is.na(Species), "sp.", Species),
         Species = sub ('x', "sp.", Species),
         Full_Species = paste (Genus, Species, sep = " "), # to remove var. determinations from the full_species list
         Full_Species = sub("Aa", "Indet sp.", Full_Species),
         Full_Species = sub("ZZ", "Indet sp.", Full_Species)
         )

########### Obtaining wood density ###############################
Census_wd <- getWoodDensity(genus = std_species$Genus,
                            species = std_species$Especie_APG_C3,
                            family =  std_species$Familia_APG_C3,
                            region = "Mexico",
                            verbose = TRUE) 

Census_wd$full_species <- paste(Census_wd$genus, Census_wd$species, sep = " ")
std_species$wood_density_dryad <- Census_wd$meanWD[match(std_species$Full_Species, Census_wd$full_species)]
std_species$level_WD <- Census_wd$levelWD
std_species$level_WD<- ifelse(is.na(std_species$wood_density_dryad),"mean_mexico",std_species$level_WD)
std_species$wood_density_dryad<-ifelse(is.na(std_species$wood_density_dryad),
                                       0.48,
                                       std_species$wood_density_dryad)

std_species$wood_density_dryad <- Census_wd$meanWD[match(std_species$Full_Species, Census_wd$full_species)]
std_species$level_wood_density_dryad <- Census_wd$levelWD[match(std_species$Full_Species, Census_wd$full_species)]

std_species$level_wood_density_dryad <- ifelse(
  is.na(std_species$level_wood_density_dryad),
  "mean_mexico",
  std_species$level_wood_density_dryad
)

std_species$wood_density_dryad <- ifelse(
  is.na(std_species$wood_density_dryad),
  0.48,
  std_species$wood_density_dryad
)

wd_paper<- wd_ppr %>% 
  mutate(
    state_id_c1 = state_id$Cve_Estado_C3 [match (wd_ppr$std_Estado, state_id$Estado_C3)],
    source_code = upmid_plotid$source_code [match (wd_ppr$UPMID.SITE.CODE, upmid_plotid$UPMID)] 
  ) %>% 
  rename(
    wd = BASIC.WOOD.DENSITY..g.cm3.,
    Full_Species = SPECIES,
    Genus = GENUS,
    Familia_APG_C3 = FAMILY,
  ) %>% 
  filter (
    !is.na(source_code)
  )

# First, calculate the average wood density for each combination of UPMID and Species in wd_paper
species_mean_density <- wd_paper %>%
  group_by(source_code, Full_Species) %>%
  summarize(mean_wood_density = mean(wd, na.rm = TRUE), .groups = 'drop')

# Join species-level mean wood density with mexico_census3_tree
updated_data <- std_species %>%
  left_join(species_mean_density, 
            by = c("source_code", "Full_Species"))

# Now handle genus-level matching where species match failed
genus_mean_density <- wd_paper %>%
  group_by(source_code, Genus) %>%
  summarize(mean_wood_density = mean(wd, na.rm = TRUE), .groups = 'drop')

updated_data <- updated_data %>%
  left_join(genus_mean_density, by = c("source_code", "Genus")) %>%
  mutate(wood_density = ifelse(is.na(mean_wood_density.x), mean_wood_density.y, mean_wood_density.x)) %>%
  select(-mean_wood_density.x, -mean_wood_density.y)

# Handle family-level matching where both species and genus match failed
family_mean_density <- wd_paper %>%
  group_by(source_code, Familia_APG_C3) %>%
  summarize(mean_wood_density = mean(wd, na.rm = TRUE), .groups = 'drop')

updated_data <- updated_data %>%
  left_join(family_mean_density, by = c("source_code", "Familia_APG_C3")) %>%
  mutate(wood_density = ifelse(is.na(wood_density), mean_wood_density, wood_density)) %>%
  select(-mean_wood_density)


# The updated_data dataframe now has the wood densities from wd_paper and is assigned to mexico_census3_tree
mexico_census3<-updated_data %>% 
  mutate(wood_density_merged = ifelse(!is.na(updated_data$wood_density), 
                                      updated_data$wood_density,
                                      updated_data$wood_density_dryad),
         level_WD_merged=ifelse(is.na(wood_density),updated_data$level_WD,"mexico_wd_db")
         )

# Calculating AGB but what to do with trees with dbh =9999993 OR height

mexico_census3$agb_tree_kg<-(ifelse(mexico_census3$DiametroNormal_C3 > 1000 | mexico_census3$AlturaTotal_C3 > 100,
                                    NA,
                                    round(
                                      as.numeric(EQ_016(mexico_census3$DiametroNormal_C3,
                                                        mexico_census3$wood_density_merged,mexico_census3$AlturaTotal_C3)),2)
)
)

mexico_census3$agb2_tree_kg<-as.numeric(ifelse(mexico_census3$biomasa_kg_C3=="NULL",NA,mexico_census3$biomasa_kg_C3))


mexico_census3$agb3_tree_kg<-(ifelse(mexico_census3$DiametroNormal_C3 > 1000 | mexico_census3$AlturaTotal_C3 > 500,
                                     NA,
                                     round(
                                        as.numeric(EQ_005(mexico_census3$DiametroNormal_C3,
                                            mexico_census3$wood_density_merged,mexico_census3$AlturaTotal_C3)),2)
                                     )
                              )


mexico_census3$AGB_eq1<-"EQ_016"
mexico_census3$AGB_eq2<-"NI_eq"
mexico_census3$AGB_eq3<-"EQ_005"

mexico_census3_tree<- mexico_census3 %>% 
  #filter(agb2_tree_kg<12000 & Edad_C3<90000) %>% 
  rename(census_date=Anio_C3,
         subplot1=Sitio_C3,
         tag_number=ArboladoID_C3,
         Family=Familia_APG_C3,
         long=X_C3,
         lat=Y_C3,
         #Species=Full_Species,
         ecoregion=DESECON4_C3,
         source_vegetation_class=DESCRIP_S7_C3,
         condition1=Condicion_C3,
         #DBH_original=DiametroNormal_C3,
         Height1=AlturaTotal_C3) %>% 
  relocate(source_code,
           subplot1,
           tag_number,
           census_date,lat,long,
           ecoregion,
           Family, 
           Species,
           wood_density_merged,
           level_WD_merged,
           DBH_original,Height1,
           condition1,
           agb_tree_kg, 
           agb2_tree_kg, 
           agb3_tree_kg, 
           AGB_eq1,AGB_eq2) 

### Create the collection code column for each State### 
#Loop through each State to create the collection column, considering State 1 as COL211 ####

results <- numeric(length(mexico_census3_tree$Cve_Estado_C3))

for (i in 1:length(mexico_census3_tree$Cve_Estado_C3)) {
  if (is.na(mexico_census3_tree$Cve_Estado_C3[i])) {
    results[i] <- NA  # Assign NA if the value is missing
  } else if (mexico_census3_tree$Cve_Estado_C3[i] == 1) {
    results[i] <- paste("COL", as.numeric(212), sep = "")
  } else if (mexico_census3_tree$Cve_Estado_C3[i] >= 2 & mexico_census3_tree$Cve_Estado_C3[i] < 33) {
    # Calculate the additional value based on the index (statecd - 1)
    additional_value <- paste("COL", (213 + (mexico_census3_tree$Cve_Estado_C3[i] - 1)), sep = "")
    results[i] <- additional_value
  } else {
    results[i] <- NA
  }
}

mexico_census3_tree$collection_code<-as.character(results)   
mexico_census3_tree_COLLECTION<-mexico_census3_tree %>%  
  group_by(collection_code) %>% 
  mutate(source_code = paste ("INFYS",source_code, sep = "_")) %>% 
  rename(State=std_Estado,
         stand_age=Edad_C3) %>% 
  relocate(collection_code, source_code) %>% 
  select(-c(wood_density_dryad,
            biomasa_kg_C3,
            AgenteDanio1_C3,
            AgenteDanio2_C3,
            AgenteDanio1_EST_C3,
            AgenteDanio2_EST_C3,
            MuertoPie_C3,
            Estado_C3,
            NoIndividuo_C3,
            NoRama_C3,
            FORM_S7_C3,
            DiametroNormal_C3,
            wood_density,
            Genero_APG_C3,	
            Especie_APG_C3))


##################################################################
#### Summarising at plot level ##################################
m_c3 <- mexico_census3_tree_COLLECTION %>% 
  group_by(collection_code,source_code,lat,long,census_date,subplot_count, AGB_eq1,AGB_eq2) %>% 
  summarise(agb_plot = sum(as.numeric(agb_tree_kg)/1000, na.rm = TRUE),
            agb2_plot= sum ( as.numeric(agb2_tree_kg)/1000, na.rm = TRUE),
            #agb3_plot= sum ( as.numeric(agb3_tree_kg)/1000, na.rm = TRUE),
            stand_age= round(mean(stand_age),2),
            Height= round(mean(Height1,na.rm=T),2),
            #source_vegetation_status = paste(unique(na.omit(c(source_vegetation_status_EN, source_vegetation_status2_EN))), collapse = "/")
            ) %>% 
  mutate(plot_area_ha=ifelse(subplot_count==1,round(area_per_hectare_sqm/4,2),
                             ifelse(subplot_count==2,round(area_per_hectare_sqm/2,2),
                                    ifelse(subplot_count==3,round(area_per_hectare_sqm/1.3,2),area_per_hectare_sqm))),
         country="Mexico",
         usage_policy=0,
         stem_level_available=1,
         agb_Mgha_1=round(agb_plot/plot_area_ha,3),
         agb2_Mgha_1=round(agb2_plot/plot_area_ha,3),
         #agb3_Mgha_1=round(agb3_plot/plot_area_ha,3)
         )%>% 
  ungroup()
 
view<-m_c3 %>% 
  arrange(source_code) %>% 
  mutate(source_group_num = dense_rank(source_code)) %>%  # Create a grouping number for each source_code within collection_code
  group_by(collection_code, source_code) %>%  
  mutate(plot_code = paste("PL", sprintf('%04d', source_group_num), sep = ""),
         plot_code = paste(collection_code,plot_code,sep="_")) %>% 
  select(-c(source_group_num)) %>% #,agb_PLOT)) %>% 
  relocate(collection_code,plot_code,source_code,lat,long) %>% 
  ungroup()


metadata<-mexico_c3_metadata %>% 
  select(
    Cve_Estado_C3,
    UPMID:Y_C3,
    Estado_C3,
    #CLAVE_UMAF_C3,
    UMAFOR_C3,
    DESCRIP_S7_C3,
    FAO_S7_C3,
    ECO_S7_C3:TIP_NUC_C3,
    ANP_CAT_MANEJO_C3,
    Altitud_C3,
    Tenencia_C3,
    FORM_S7_C3,
    Estado_C3
  ) %>% 
  mutate(source_code=paste("INFYS", IDConglomerado, Cve_Estado_C3, sep = "_"),
         minimum_dbh_plot=75,
         land_ownership=ifelse(Tenencia_C3=="No capturado" | Tenencia_C3=="No aplica", TIP_NUC_C3,
                               Tenencia_C3),
         land_ownership=ifelse(land_ownership=="","unkown",land_ownership),
         source_vegetation_class=tolower(stri_trans_general(DESCRIP_S7_C3, "Latin-ASCII"))
         ) %>% 
  rename(ecoregion= DESECON4_C3, #
         UMAFOR=UMAFOR_C3) %>% 
         select(-c(FORM_S7_C3,CVEECON1_C3,CVEECON2_C3,
                   starts_with("DESE"),CVEECON3_C3,CVEECON4_C3,
                   FAO_S7_C3,TIP_PROP_C3,ANP_CAT_MANEJO_C3,ECO_S7_C3)
                )  

  metadata$land_ownership<-sub("EJIDO","ejido",metadata$land_ownership)
  metadata$land_ownership<-sub("Ejido","ejido",metadata$land_ownership)
  metadata$land_ownership<-sub("Ejidal","ejido",metadata$land_ownership)
  metadata$land_ownership<-sub("Comunal","community",metadata$land_ownership)
  metadata$land_ownership<-sub("Comunidad","community",metadata$land_ownership)
  metadata$land_ownership<-sub("COMUNIDAD","community",metadata$land_ownership)
  metadata$land_ownership<-sub("Propiedad particular","private",metadata$land_ownership)
  metadata$land_ownership<-sub("Propiedad federal","federal",metadata$land_ownership)
  

view_metadata<-view %>% 
  left_join(metadata,by="source_code") %>% 
  select(-c(agb2_plot,
            X_C3,
            Y_C3,
            IDConglomerado,
            agb_plot)
         ) %>% 
  rename(source_vegetation_class_ES = source_vegetation_class) %>% 
  relocate(collection_code,
           plot_code,
           source_code,
           lat,
           long,
           agb_Mgha_1,
           agb2_Mgha_1,
           AGB_eq1,
           AGB_eq2) %>%
    filter(!is.na(collection_code)) %>%  
    select(-c(TIP_NUC_C3,Tenencia_C3))

view_metadata $ source_vegetation_class_ES <-  tolower( str_replace_all(view_metadata $ source_vegetation_class_ES, "\u00A0", " "))

Mexico_c3_plot<-view_metadata %>%  
  rename(State=Estado_C3) %>% 
  mutate(
    source_vegetation_class = source_veg_class_table$std_source_veg_class[
        match(source_vegetation_class_ES, source_veg_class_table$std_source_veg_class_ES)],
    status_sec = ifelse(grepl("vegetacion secundaria", source_vegetation_class_ES), "secondary vegetation", NA),
  )  %>%
  left_join(results_disturbances, by="source_code") %>% 
   mutate(source_vegetation_status = case_when (
    !is.na(status_sec) & !is.na(disturbance_en) ~ paste(disturbance_en, status_sec, sep="/"),
    !is.na(status_sec) ~ status_sec,
    !is.na(disturbance_en) ~ disturbance_en,
    TRUE ~ ""
    )
  ) %>% 
  select(-c(#total_trees, 
            #n_trees, 
            #proportion, 
            disturbance_en, 
            status_sec, 
            #status, 
            #Cve_Estado_C3,
            #DESCRIP_S7_C3.y,
            #DESCRIP_S7_C3.x,
            #Altitud_C3,
            State,
            #subplot_count.x,
            #subplot_count.y
            )) 



write.csv(Mexico_c3_plot,"Mexico_Census3_PLOT.csv",row.names=F)

############## PLOTTING #########################################################
#Pivoting the data to a long format
mexico_census3_long <- mexico_census3_tree_COLLECTION %>%
  #filter(mexico_census3_tree_COLLECTION$agb_tree_kg != mexico_census3_tree$agb1_tree_kg) %>% 
  pivot_longer(
    cols = c(agb_tree_kg, agb2_tree_kg, agb3_tree_kg),  # Include all AGB columns
    names_to = "AGB_type",
    values_to = "AGB_value"
  ) %>%
  filter(!is.na(AGB_value))   # Remove NA values for plotting


#Creating the box plot
ggplot(mexico_census3_long, aes(x = AGB_type, y = AGB_value, fill = AGB_type)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 1.5) +  # Adding outliers
  #geom_boxplot(outlier.shape = NA) + 
  labs(title = "Comparison of AGB Estimates",
       x = "AGB Type",
       y = "AGB Value (kg)") +
  theme_minimal() +
  theme(legend.position = "none") +  # Hide the legend if not needed
  scale_fill_manual(values = c("red", "green", "purple"))#+
#ylim(0, 200) 

# Create the scatter plot with three lines
ggplot(mexico_census3_tree_COLLECTION, aes(x = agb_tree_kg)) +
  geom_point(aes(y = agb2_tree_kg), color = "lightgreen", alpha = 0.4) +
  geom_point(aes(y = agb3_tree_kg), color = "lightgray", alpha = 0.4) +
  geom_smooth(aes(y = agb2_tree_kg), method = "lm", color = "lightgreen", se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = agb3_tree_kg), method = "lm", color = "lightgray", se = FALSE, linetype = "dotted") +
  labs(
    x = "AGB (Mgha_1)",
    y = "AGB2, AGB3 (Mgha_1)",
    title = "Scatter Plot with Multiple Lines"
  ) +
  theme_minimal() 



