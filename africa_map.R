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
  mutate(`Any public insurance?` = factor(
    ifelse(substr(`National (public) health insurance?`,1 ,1)=="Y", "Yes", "No"), 
                                          levels = c("Yes", "No")))

# ------------------------------------------------------------------------------

# Maps!

# 3 colors: has National OR community based insurance, has it been used in research, is it publically available

african_countries <- ne_countries(
  continent = "Africa",
  returnclass = "sf",
  type = "countries"
) 

merged_data <- african_countries %>% left_join(insurance_coverage, by = "iso_a3")

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Any public insurance?`)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +  # Change the color scale as desired
  labs(title = "Any national or community health insurance?")

ggplot(data = merged_data) +
  geom_sf(aes(fill = `National (public) health insurance?`)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +  # Change the color scale as desired
  labs(title = "Any national health insurance? (including programs for civil servants)")

ggplot(data = merged_data) +
  geom_sf(aes(fill = `Community-based health insurance?`)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +  # Change the color scale as desired
  labs(title = "Any community-based health insurance")


