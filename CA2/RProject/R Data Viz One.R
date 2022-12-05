# ------------------------------------------------ #
#
# Data Visualisation Assignment 2
# Visualisations in R
#
# Student No. d21124026
# Name : Ciaran Finnegan
# TU060 Data Science MSc
#
# December 2022
# ------------------------------------------------ #


# load required libraries
library(rgeos)
library(maptools)
library(curl)
library(readr)
library(sf)
library(rmapshaper)
library(tidyverse)
library(ggmap)

#
#
#
#library(viridis)


#

temp_1 <- tempfile()
temp_2 <- tempfile()
source <- "https://www.cso.ie/en/media/csoie/census/census2011boundaryfiles/26counties/26_Counties.zip"
temp_1 <- curl_download(url = source, destfile = temp_1, quiet = FALSE)
unzip(temp_1, exdir = temp_2)

#sf_Ireland <- read_sf(file.path(temp_2,"0d80d6a5-6314-4a4b-ac2f-09f3767f054b2020329-1-1rx3r8i.iy91.shp"))
#sf_Ireland <- read_sf(file.path(temp_2,"26 counties/Census2011_Admin_Counties_generalised20m.shp"))
#sf_Ireland <- read_sf("Census2011_Admin_Counties_generalised20m.shp")
#sf_Ireland <- read_sf("Census2011_Counties_Modified.shp")
#sf_Ireland <- read_sf("Counties/Counties___OSi_National_Statutory_Boundaries___Generalised_20m.shp")
#Census2011_Counties_Modified


# read the shape file
spdf <- readShapePoly("Counties/Counties___OSi_National_Statutory_Boundaries___Generalised_20m.shp")

#spdf <- readShapePoly(file.path(temp_2,"Census2011_Admin_Counties_generalised20m.shp"))

# make it ggplot-friendly
spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")


# plot it
ggplot(counties) + geom_polygon(colour="black", fill=NA, aes(x=long, y=lat, group=group)) + coord_fixed()




