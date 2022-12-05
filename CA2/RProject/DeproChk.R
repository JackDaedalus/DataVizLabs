# load required libraries
library(rgeos)
library(maptools)
library(curl)
library(readr)
library(sf)
library(rmapshaper)
library(tidyverse)
library(ggmap)


temp_1 <- tempfile()
temp_2 <- tempfile()
source <- "http://data-osi.opendata.arcgis.com/datasets/68b14cef8cf247b191ee2737e7e6993d_1.zip"
temp_1 <- curl_download(url = source, destfile = temp_1, quiet = FALSE)
unzip(temp_1, exdir = temp_2)
sf_Ireland <- read_sf(file.path(temp_2,"0d80d6a5-6314-4a4b-ac2f-09f3767f054b2020329-1-1rx3r8i.iy91.shp"))
sf_Ireland <- sf_Ireland %>% # ms_simplify(sf_Ireland)  %>% 
  select(-NUTS1, -NUTS1NAME, -NUTS2, -NUTS3,
         -SMALL_AREA, -CSOED, -OSIED, - COUNTY) %>% 
  rename(sa_code = GEOGID, county = COUNTYNAME, ed_name = EDNAME,
         nuts2name = NUTS2NAME, nuts3name = NUTS3NAME)
sf_Ireland