### Constant ####

source_veg_class_table<-read.csv("source_veg_class.csv",h=T) 
wd_ppr <- read.csv("wood_densities_2024.csv",h=T) %>% as.data.frame
mexico_c3 <- read.csv("INFyS_c3_2015_2020.csv", h = T)
wd_paper<-read.csv("wood_density_mexico.csv",h=T)

## Total plot hectare area##
area_per_hectare_sqm<-0.16

## Create land ownership table and translation ###

land_owner<-tibble(
  land_owner_ES=c("Propiedad Particular","Ejidal","Comunal","Propiedad Federal"),
  land_owner_EN=c("private","ejido","community","federal")
  )

## Create a table including State and State ID ##

state_id <- mexico_c3 %>% 
  select(Cve_Estado_C3, Estado_C3) %>% 
  filter(!is.na(Cve_Estado_C3)) %>% 
  mutate(
    std_State_name = Estado_C3 %>% 
      stri_trans_general("Latin-ASCII") %>%  # Remove accents
      str_replace_all(" ", "_") %>%         # Replace spaces with underscores
      tolower() %>%                         # Convert to lowercase
      gsub("ciudad_de_mexico", "distrito_federal", .) # Replace string
  ) %>% 
  unique() %>% 
  as_tibble() %>% 
  add_row(
    Cve_Estado_C3 = c(9, 9, 22),
    Estado_C3 = c("Distrito Federal", "Ciudad de Mexico", "Queretaro de arteaga"),
    std_State_name = c("distrito_federal","ciudad_de_mexico", "queretaro_de_arteaga")
  )

### Create table with UPMID and IdConglomerate ###

upmid_plotid<-mexico_c3 %>% 
  distinct(UPMID, IdConglomerado, Cve_Estado_C3
  ) %>% 
  mutate(source_code = paste(IdConglomerado,Cve_Estado_C3,sep = "_")
  )

###### Standardise source_veg_class table #####

source_veg_class_table$std_source_veg_class_ES <- tolower( str_replace_all(source_veg_class_table$Forest.Type..Spanish., "\u00A0", " "))
source_veg_class_table$std_source_veg_class <- tolower( str_replace_all(source_veg_class_table$Forest.Type..English., "\u00A0", " "))

### standardize wd_ppr states ###

wd_ppr$std_Estado <- wd_ppr$STATE %>% 
  stri_trans_general("Latin-ASCII") %>%  
  str_replace_all(" ", "_")   %>% tolower  

#Chave et al allometric equations #

EQ_016<- function (D,WD, H){
  0.0673 * (WD * ((D/10)^2) * H)^0.976 # needs to convert the Diameter back to cm
}

EQ_005<-function (D, WD, H) {
  0.0559*(WD*(D/10^2)*H)
}



