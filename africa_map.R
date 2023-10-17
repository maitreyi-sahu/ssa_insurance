# MSahu
# Oct 17, 2023

library(readxl)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(RColorBrewer)

rm(list=ls())

# ------------------------------------------------------------------------------

# Read data

dir <- "C:/Users/msahu/OneDrive - UW/Documents/Research/IHME/FGH/Health insurance/"

insurance_coverage <- read_xlsx(paste0(dir, "lit review africa health insurance coverage.xlsx"),
                                sheet = "Data") %>% 
  rename(iso_a3 = ISO3) %>% 
  
  mutate(`Any national insurance?` = factor(
    ifelse(substr(`National (public) health insurance?`,1 ,1)=="Y", "Yes", "No"), 
                                          levels = c("Yes", "No"))) %>% 
  
  mutate(`Any community insurance?` = factor(
    ifelse(substr(`Community-based health insurance?`,1 ,1)=="Y", "Yes", "No"), 
    levels = c("Yes", "No"))) %>% 
  
  mutate(`Any public insurance?` = factor(
    ifelse(`Any national insurance?` == "Yes" | `Any community insurance?` == "Yes", "Yes", "No"),
    levels = c("Yes", "No"))) %>% 
  
  mutate(`Status of claims data` = factor(case_when(
    `Publicly available health insurance claims data?` == "Y" ~ "Publically available claims data",
    `Has health insurance claims data been used in a research article?` == "Y" & `Publicly available health insurance claims data?` != "Y" ~ "Claims used in research but \n are not publically available",
    `Any public insurance?` == "Yes" & `Has health insurance claims data been used in a research article?` != "Y" ~ "National or community health insurance exists \n but no analysis of claims available",
    T ~ "No public (national or community) insurance currently exists"),
    levels = c("Publically available claims data", 
               "Claims used in research but \n are not publically available", 
               "National or community health insurance exists \n but no analysis of claims available", 
               "No public (national or community) insurance currently exists")))

# ------------------------------------------------------------------------------

# Maps!

# 3 colors: has National OR community based insurance, has it been used in research, is it publically available

african_countries <- ne_countries(
  continent = "Africa",
  returnclass = "sf",
  type = "countries"
) 

merged_data <- african_countries %>% left_join(insurance_coverage, by = "iso_a3")

pdf(paste0(dir,"insurance_maps_africa.pdf"), width = 10, height = 12, onefile = TRUE)

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Status of claims data`)) +
  scale_fill_viridis_d() +  
  labs(title = "Is there a public (national or community) insurance program, has there been analysis of claims data, \nand are claims data publically available?") +
  theme_void()

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Any public insurance?`)) +
  scale_fill_viridis_d() +  
  labs(title = "Any national or community health insurance?") +
  theme_void()

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Any national insurance?`)) +
  scale_fill_viridis_d() +  
  labs(title = "Any national health insurance? (including programs for civil servants)") +
  theme_void()

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Any community insurance?`)) +
  scale_fill_viridis_d() +  
  labs(title = "Any community-based health insurance?") +
  theme_void()

ggplot(data = merged_data) +
  geom_sf(aes(fill = `National (public) health insurance?`)) + 
  scale_fill_viridis_d() +  
  labs(title = "Any national health insurance? [Detailed]") +
  theme_void()

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Community-based health insurance?`)) +
  scale_fill_viridis_d() +  
  labs(title = "Any community-based health insurance? [Detailed]") +
  theme_void()

dev.off()

