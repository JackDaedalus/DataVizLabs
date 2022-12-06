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
library(ggplot2)
library(sqldf)
library(tidyverse)
library(viridis)
library(dplyr)




sGitHub_Datasource1 <-"https://github.com/JackDaedalus/DataVizLabs/raw/"
sGitHub_Datasource2 <- paste(sGitHub_Datasource1,"dfa3d486a5ea74a588e9768141b35f570eff3c57/CA2/", sep = "", collapse=NULL)
sGitHub_Datafile <- "Counties_-_OSi_National_Statutory_Boundaries_-_2019_-_Generalised_20m.zip"
sGitHub_Datasource <- paste(sGitHub_Datasource2,sGitHub_Datafile, sep = "", collapse=NULL)

county_map_source <- sGitHub_Datasource


temp_1 <- tempfile()
temp_2 <- tempfile()
source <- county_map_source
temp_1 <- curl_download(url = source, destfile = temp_1, quiet = FALSE)
unzip(temp_1, exdir = temp_2)




# read the shape file
spdf <- readShapePoly(file.path(temp_2,"Counties___OSi_National_Statutory_Boundaries___Generalised_20m.shp"))

# make it ggplot-friendly
spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")






#"https://github.com/JackDaedalus/DataVizLabs/raw/main/CA2/SAPS2016_CTY31.zip"

sGitHub_Datasource1_2016 <-"https://github.com/JackDaedalus/DataVizLabs/raw/"
sGitHub_Datasource2_2016 <- paste(sGitHub_Datasource1_2016,"main/CA2/", sep = "", collapse=NULL)
sGitHub_Datafile_2016 <- "SAPS2016_CTY31.zip"
sGitHub_Datasource_2016 <- paste(sGitHub_Datasource2_2016,sGitHub_Datafile_2016, sep = "", collapse=NULL)

f2016CTY_data <- sGitHub_Datasource_2016


temp_3 <- tempfile()
temp_4 <- tempfile()
source <- f2016CTY_data
temp_3 <- curl_download(url = source, destfile = temp_3, quiet = FALSE)
unzip(temp_3, exdir = temp_4)




# Load Census Theme Data for 2016 for Irish counties
# Select only the required unemployment data 
# Rename the columns to increase understanding of the data
# read the CSV file
df2016CountyThemes <- read_delim(file.path(temp_4,"SAPS2016_CTY31.csv"),show_col_types = FALSE) %>%
  select(GEOGID, GEOGDESC, T8_1_LFFJT, T8_1_ULGUPJT, T8_1_TT) %>%
  rename(Looking_for_Work = T8_1_LFFJT, Unemployed = T8_1_ULGUPJT, Total_Workforce = T8_1_TT)



view(df2016CountyThemes)

# The Census theme data breaks down the counties in certain cases. This data needs to be merge to match 
# the county boundaries in the OSI dataframe

# Start with Dublin...
df2016DublinThemes <- df2016CountyThemes %>%
  filter(GEOGID %in% c("CTY31_DC","CTY31_DR","CTY31_FL","CTY31_SD"))

head(df2016DublinThemes)

# Group the country regions and sum all unemployment data for Dublin overall
df2016DublinThemesTotal <- sqldf("Select 'CTY32_DC' as GEOGID,
                                  'Dublin' as Dublin,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2016DublinThemes
                                  group by Dublin")

head(df2016DublinThemesTotal)
str(df2016DublinThemesTotal)



# The Cork areas are combined next...
df2016CorkThemes <- df2016CountyThemes %>%
  filter(GEOGID %in% c("CTY31_CC","CTY31_CK"))

head(df2016CorkThemes)

# Group the country regions and sum all unemployment data for Dublin overall
df2016CorkThemesTotal <- sqldf("Select 'CTY33_CC' as GEOGID,
                                  'Cork' as Cork,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2016CorkThemes
                                  group by Cork")

head(df2016CorkThemesTotal)
str(df2016CorkThemesTotal)






# The Galway areas are combined next...
df2016GalwayThemes <- df2016CountyThemes %>%
  filter(GEOGID %in% c("CTY31_GC","CTY31_GY"))

head(df2016GalwayThemes)

# Group the country regions and sum all unemployment data for Galway overall
df2016GalwayThemesTotal <- sqldf("Select 'CTY34_GC' as GEOGID,
                                  'Galway' as Galway,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2016GalwayThemes
                                  group by Galway")

head(df2016GalwayThemesTotal)
str(df2016GalwayThemesTotal)








# Add Collated County data to revised county theme data
df2016CountyThemes <- data.frame(rbind(as.matrix(df2016CountyThemes), as.matrix(df2016DublinThemesTotal)))    # Dublin
df2016CountyThemes <- data.frame(rbind(as.matrix(df2016CountyThemes), as.matrix(df2016CorkThemesTotal)))      # Cork
df2016CountyThemes <- data.frame(rbind(as.matrix(df2016CountyThemes), as.matrix(df2016GalwayThemesTotal)))    # Galway





# Reconvert County unemployment data columns back to numeric
df2016CountyThemes$Looking_for_Work = as.numeric(as.character(df2016CountyThemes$Looking_for_Work))
df2016CountyThemes$Unemployed = as.numeric(as.character(df2016CountyThemes$Unemployed))
df2016CountyThemes$Total_Workforce = as.numeric(as.character(df2016CountyThemes$Total_Workforce))


head(df2016CountyThemes)
tail(df2016CountyThemes)
str(df2016CountyThemes)


# Remove the redundant county sub-breakdowns for unemployment data
df2016CountyThemes <- df2016CountyThemes %>%
  filter(!GEOGID %in% c("CTY31_CC","CTY31_CK","CTY31_DC","CTY31_DR","CTY31_FL","CTY31_SD","CTY31_GC","CTY31_GY")) %>%
  rename(COUNTY = GEOGDESC)



# Calculate Unemployment rate by County and add to Dataframe
df2016CountyThemes$Unemploy_Rate <- ((df2016CountyThemes$Looking_for_Work + df2016CountyThemes$Unemployed) / df2016CountyThemes$Total_Workforce) * 100


# Convert county names to upper case to match map dataframe
df2016CountyThemes <- mutate_all(df2016CountyThemes, .funs=toupper)


# Reconvert County unemployment data columns back to numeric
df2016CountyThemes$Looking_for_Work = as.numeric(as.character(df2016CountyThemes$Looking_for_Work))
df2016CountyThemes$Unemployed = as.numeric(as.character(df2016CountyThemes$Unemployed))
df2016CountyThemes$Total_Workforce = as.numeric(as.character(df2016CountyThemes$Total_Workforce))
df2016CountyThemes$Unemploy_Rate = as.numeric(as.character(df2016CountyThemes$Unemploy_Rate))


head(df2016CountyThemes)
tail(df2016CountyThemes)
str(df2016CountyThemes)


head(counties)
head(df2016CountyThemes)
#view(counties)
view(df2016CountyThemes)


# Using cut to create categorical bands for rates of unemployment
df2016CountyThemes$Unemploy_Pct <- cut(df2016CountyThemes$Unemploy_Rate, 
                                       breaks = c(0, 6.99, 9.99, 10.99, 11.99, 12.99, 13.99, 14.99, 99), 
                                       labels = c("<7%", "7-10%", "10-11%", "11-12%", "12-13%", "13-14%", "14-15%", "15%+"))



str(df2016CountyThemes)
view(df2016CountyThemes)


# Change name for Limerick and Waterford to allow dataframe sot join on County Name
#myDataFrame["rowName", "columnName"] <- value
df2016CountyThemes[10, "COUNTY"] <- "LIMERICK"
df2016CountyThemes[20, "COUNTY"] <- "WATERFORD"


# Join dataframe on county names
dfCountyMap <- left_join(counties, df2016CountyThemes, by = "COUNTY")

dfCountyMap$COUNTY <- factor(dfCountyMap$COUNTY)

head(dfCountyMap)
tail(dfCountyMap)

view(df2016CountyThemes)



# plot it
ggplot(dfCountyMap) + 
  geom_polygon(colour="black", aes(x=long, y=lat, group=group, fill=Unemploy_Pct)) + 
  labs(x = NULL, y = NULL,                                                          
       title = "Unemployment Rate by County (2016) for Ireland",
       subtitle = "Sources: Census 2016", 
       caption = "Plot by C.Finegan d21124026") +  
  theme(axis.line=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid = element_blank(),
        plot.caption.position = 'plot',
        plot.title.position = 'plot',
        legend.position = "bottom")+ 
  #scale_fill_brewer(palette="OrRd") + 
  labs(fill = "Unemployment Rate (%)")+
  scale_fill_brewer(palette="YlOrRd")


