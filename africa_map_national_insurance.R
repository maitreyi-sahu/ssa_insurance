# MSahu
# Oct 17, 2023

# Map of health insurance programs across Africa

# TO DO: 
# Western Sahara??
# Add margins around title?


library(readxl)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(RColorBrewer)
library(ggnewscale)
library(ggpattern)
library(maps)
library(mapproj)

rm(list=ls())

# ------------------------------------------------------------------------------

# Read data

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/IHME/FGH/Health insurance/"

insurance_coverage <- read_xlsx(paste0(dir, "lit review africa health insurance coverage_revised.xlsx"),
                                sheet = "Data") %>% 
  rename(iso_a3 = ISO3) %>% 
  
  mutate(`National (public) health insurance?` = factor(`National (public) health insurance?`,
         levels = c("Yes, coverage >40%", "Yes, coverage <40%", "Yes, coverage unknown",
                    "No, bill passed in 2023 but not yet implemented",
                    "No, planned but not implemented",
                    "No"))) %>% 
  
  mutate(`Any national insurance?` = factor(
    ifelse(substr(`National (public) health insurance?`,1 ,1)=="Y", "Yes", "No"), 
                                          levels = c("Yes", "No"))) %>% 
  
  mutate(`Any community insurance?` = factor(
    ifelse(substr(`Community-based health insurance?`,1 ,1)=="Y", "Yes", "No"), 
    levels = c("Yes", "No"))) %>% 
  
  mutate(`Any public insurance?` = factor(
    ifelse(`Any national insurance?` == "Yes" | `Any community insurance?` == "Yes", "Yes", "No"),
    levels = c("Yes", "No"))) %>% 
  
  # read the levels
  
  mutate(`Status of claims data` = factor(case_when(
    `Publicly available health insurance claims data?` == "Y" ~ "Publically available claims data",
    `Has health insurance claims data been used in a research article?` == "Y" & `Publicly available health insurance claims data?` != "Y" ~ "Claims used in research but \n are not publically available",
    `Any public insurance?` == "Yes" & `Has health insurance claims data been used in a research article?` != "Y" ~ "National public health insurance exists \n but no analysis of claims available",
    T ~ "No national public health insurance currently exists"),
    levels = c("Publically available claims data", 
               "Claims used in research but \n are not publically available", 
               "National public health insurance exists \n but no analysis of claims available", 
               "No national public health insurance currently exists"))) %>% 
  
  mutate(published_claims_in_research = ifelse(`Status of claims data` == "Claims used in research but \n are not publically available", "Yes", "No"))

# ------------------------------------------------------------------------------

# Maps!

# Yes has 3 levels; No has 3 levels
# 
# african_countries <- ne_countries(
#   continent = "Africa",
#   returnclass = "sf",
#   type = "countries"
# ) 
# 
# merged_data <- african_countries %>% left_join(insurance_coverage, by = "iso_a3") %>% 
#   filter(iso_a3!="SAH" & iso_a3!="ESH") # remove Western Sahara and Somaliland
# 
# pdf(paste0(dir,"insurance_map_africa.pdf"), width = 10, height = 12, onefile = TRUE)
# 
# ggplot(data = merged_data) +
#   geom_sf(aes(fill = `National (public) health insurance?`)) +
#   scale_fill_manual(values = c("#2E7D32", "#43A047", "#81C784", "#FF8A65","#FF7043", "#E64A19")) + #https://www.r-bloggers.com/2018/12/having-bit-of-party-with-material-colour-palette/
#   labs(title = "Is there a national public insurance program, has there been analysis of claims data, \nand are claims data publically available?") +
#   new_scale_fill() +
#   geom_map_pattern( aes(pattern_fill = "published_claims_in_research"), map = african_countries)+
#   #geom_sf(aes(fill= "published_claims_in_research")) +
#   #scale_fill_manual(values = c("#00000000", "#6D4C4180"),
#   #                  na.value="blank", name = "Published research using claims data?") +
#   theme_void()
# 
# dev.off()

# ------------------------------------------------------------------------------


# World borders downloaded from: http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip

library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(dir,"/world_borders/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

africa <- my_spdf[my_spdf@data$REGION==2 , ]
rm(my_spdf)

africa_merged <- st_as_sf(africa)  %>%  mutate(iso_a3 = ISO3) %>% 
  left_join(insurance_coverage, by = "iso_a3") %>% 
  mutate(id = ISO3, x = LON, y = LAT) %>%
  # Clean country names to match those from world borders
  mutate(NAME = ifelse(NAME == "United Republic of Tanzania", "Tanzania", NAME),
         NAME = ifelse(NAME == "Libyan Arab Jamahiriya", "Libya", NAME),
         NAME = ifelse(NAME == "Congo", "Republic of Congo", NAME)
         ) %>% 
  mutate(country = tolower(NAME)) %>% 
  mutate(`Published research using claims data?` = ifelse(published_claims_in_research == "Yes", "Yes", "No"))  %>% 
  # filter out values with NA's (Western Sahara / Somaliland) - otherwise gives error
  filter(!is.na(ISO3) & !is.na(`Published research using claims data?`))  %>% 
  #filter(!NAME %in% islands) %>% 
  mutate(health_insurance = `National (public) health insurance?`)
  
world_map <- map_data("world")
africa_map <- world_map %>% filter(region %in% africa_merged$NAME)

# PLOT

pdf(paste0(dir,"insurance_map_africa_pattern.pdf"), width = 10, height = 12, onefile = TRUE)

ggplot(data = africa_merged, aes(map_id = Country, x = x, y = y, 
                                 fill = `National (public) health insurance?`)) +
  geom_sf(linewidth = 0.2) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  geom_map_pattern(size = .2, 
                   aes(pattern = `Published research using claims data?`),
                       fill = "#00000000",
                       color = '#00000000',
                       pattern_density = 0.125,
                       pattern_spacing = .008,
                       pattern_colour  = 'black',
                       pattern_fill = 'black',
                       map = africa_map) +
  scale_pattern_manual(values = c("none", "stripe")) +
  theme_void() +
  expand_limits(x = africa_map$long, y = africa_map$lat) +
  coord_sf() +
  ggtitle("Does a national public insurance program exist, and has there been published research\nusing claims data?") 

dev.off()


