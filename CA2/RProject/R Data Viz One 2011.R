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




#"https://github.com/JackDaedalus/DataVizLabs/raw/dfa3d486a5ea74a588e9768141b35f570eff3c57/CA2/Counties_-_OSi_National_Statutory_Boundaries_-_2019_-_Generalised_20m.zip"

sGitHub_Datasource1 <-"https://github.com/JackDaedalus/DataVizLabs/raw/"
sGitHub_Datasource2 <- paste(sGitHub_Datasource1,"dfa3d486a5ea74a588e9768141b35f570eff3c57/CA2/", sep = "", collapse=NULL)
sGitHub_Datafile <- "Counties_-_OSi_National_Statutory_Boundaries_-_2019_-_Generalised_20m.zip"
sGitHub_Datasource <- paste(sGitHub_Datasource2,sGitHub_Datafile, sep = "", collapse=NULL)



county_map_source <- sGitHub_Datasource

#county_map_source <- "https://github.com/JackDaedalus/DataVizLabs/raw/dfa3d486a5ea74a588e9768141b35f570eff3c57/CA2/Counties_-_OSi_National_Statutory_Boundaries_-_2019_-_Generalised_20m.zip"

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



# Load Census Theme Data for 2011 for Irish counties
# Select only the required unemployment data 
# Rename the columns to increase understanding of the data
df2011CountyThemes <- read_delim("https://www.cso.ie/en/media/csoie/census/documents/saps2011files/AllThemesTablesCTY.csv",show_col_types = FALSE) %>%
  select(GEOGID, GEOGDESC, T8_1_LFFJT, T8_1_ULGUPJT, T8_1_TT) %>%
  rename(Looking_for_Work = T8_1_LFFJT, Unemployed = T8_1_ULGUPJT, Total_Workforce = T8_1_TT)


# The Census theme data breaks down the counties in certain cases. This data needs to be merge to match 
# the county boundaries in the OSI dataframe


# Start with Dublin...
df2011DublinThemes <- df2011CountyThemes %>%
  filter(GEOGID %in% c("C02","C03","C04","C05"))

head(df2011DublinThemes)

# Group the country regions and sum all unemployment data for Dublin overall
df2011DublinThemesTotal <- sqldf("Select 'C35' as GEOGID,
                                  'Dublin' as Dublin,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2011DublinThemes
                                  group by Dublin")

head(df2011DublinThemesTotal)
str(df2011DublinThemesTotal)



# The Cork areas are combined next...
df2011CorkThemes <- df2011CountyThemes %>%
  filter(GEOGID %in% c("C17","C18"))

head(df2011CorkThemes)

# Group the country regions and sum all unemployment data for Dublin overall
df2011CorkThemesTotal <- sqldf("Select 'C36' as GEOGID,
                                  'Cork' as Cork,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2011CorkThemes
                                  group by Cork")

head(df2011CorkThemesTotal)
str(df2011CorkThemesTotal)






# The Limerick areas are combined next...
df2011LimerickThemes <- df2011CountyThemes %>%
  filter(GEOGID %in% c("C20","C21"))

head(df2011LimerickThemes)

# Group the country regions and sum all unemployment data for Limerick overall
df2011LimerickThemesTotal <- sqldf("Select 'C36' as GEOGID,
                                  'Limerick' as Limerick,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2011LimerickThemes
                                  group by Limerick")

head(df2011LimerickThemesTotal)
str(df2011LimerickThemesTotal)





# The Tipperary areas are combined next...
df2011TipperaryThemes <- df2011CountyThemes %>%
  filter(GEOGID %in% c("C22","C23"))

head(df2011TipperaryThemes)

# Group the country regions and sum all unemployment data for Tipperary overall
df2011TipperaryThemesTotal <- sqldf("Select 'C37' as GEOGID,
                                  'Tipperary' as Tipperary,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2011TipperaryThemes
                                  group by Tipperary")

head(df2011TipperaryThemesTotal)
str(df2011TipperaryThemesTotal)







# The Waterford areas are combined next...
df2011WaterfordThemes <- df2011CountyThemes %>%
  filter(GEOGID %in% c("C24","C25"))

head(df2011WaterfordThemes)

# Group the country regions and sum all unemployment data for Waterford overall
df2011WaterfordThemesTotal <- sqldf("Select 'C38' as GEOGID,
                                  'Waterford' as Waterford,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2011WaterfordThemes
                                  group by Waterford")

head(df2011WaterfordThemesTotal)
str(df2011WaterfordThemesTotal)








# The Galway areas are combined next...
df2011GalwayThemes <- df2011CountyThemes %>%
  filter(GEOGID %in% c("C26","C27"))

head(df2011GalwayThemes)

# Group the country regions and sum all unemployment data for Galway overall
df2011GalwayThemesTotal <- sqldf("Select 'C39' as GEOGID,
                                  'Galway' as Galway,
                                  sum(Looking_for_Work),
                                  sum(Unemployed),
                                  sum(Total_Workforce)
                                  from df2011GalwayThemes
                                  group by Galway")

head(df2011GalwayThemesTotal)
str(df2011GalwayThemesTotal)








# Add Collated County data to revised county theme data
df2011CountyThemes <- data.frame(rbind(as.matrix(df2011CountyThemes), as.matrix(df2011DublinThemesTotal)))    # Dublin
df2011CountyThemes <- data.frame(rbind(as.matrix(df2011CountyThemes), as.matrix(df2011CorkThemesTotal)))      # Cork
df2011CountyThemes <- data.frame(rbind(as.matrix(df2011CountyThemes), as.matrix(df2011LimerickThemesTotal)))  # Limerick
df2011CountyThemes <- data.frame(rbind(as.matrix(df2011CountyThemes), as.matrix(df2011TipperaryThemesTotal))) # Tipperary
df2011CountyThemes <- data.frame(rbind(as.matrix(df2011CountyThemes), as.matrix(df2011WaterfordThemesTotal))) # Waterford
df2011CountyThemes <- data.frame(rbind(as.matrix(df2011CountyThemes), as.matrix(df2011GalwayThemesTotal)))    # Galway


# Convert county names to upper case to match map dataframe
df2011CountyThemes <- mutate_all(df2011CountyThemes, .funs=toupper)


# Reconvert County unemployment data columns back to numeric
df2011CountyThemes$Looking_for_Work = as.numeric(as.character(df2011CountyThemes$Looking_for_Work))
df2011CountyThemes$Unemployed = as.numeric(as.character(df2011CountyThemes$Unemployed))
df2011CountyThemes$Total_Workforce = as.numeric(as.character(df2011CountyThemes$Total_Workforce))


head(df2011CountyThemes)
tail(df2011CountyThemes)
str(df2011CountyThemes)


# Remove the redundant county sub-breakdowns for unemployment data
df2011CountyThemes <- df2011CountyThemes %>%
  filter(!GEOGID %in% c("C02","C03","C04","C05","C17","C18","C20","C21","C22","C23","C24","C25","C26","C27")) %>%
  rename(COUNTY = GEOGDESC)



# Calculate Unemployment rate by County and add to Dataframe
df2011CountyThemes$Unemploy_Rate <- ((df2011CountyThemes$Looking_for_Work + df2011CountyThemes$Unemployed) / df2011CountyThemes$Total_Workforce) * 100


head(df2011CountyThemes)
tail(df2011CountyThemes)
str(df2011CountyThemes)


head(counties)
head(df2011CountyThemes)



# Using cut to create categorical bands for rates of unemployment
df2011CountyThemes$Unemploy_Pct <- cut(df2011CountyThemes$Unemploy_Rate, 
                                       breaks = c(0, 9.99, 10.99, 11.99, 12.99, 13.99, 14.99, 99), 
                                       labels = c("<10%", "10-11%", "11-12%", "12-13%", "13-14%", "14-15%", "15%+"))


str(df2011CountyThemes)
view(df2011CountyThemes)



# Join dataframe on county names
dfCountyMap <- left_join(counties, df2011CountyThemes, by = "COUNTY")

dfCountyMap$COUNTY <- factor(dfCountyMap$COUNTY)

head(dfCountyMap)
tail(dfCountyMap)

view(df2011CountyThemes)


# define a base map layer
IRLCounty_BaseLayer <- ggplot(dfCountyMap) + 
  aes(long, lat, group=group) +
  geom_polygon(colour="grey")

# plot a data column 
IRLCounty_BaseLayer + aes(fill=Unemploy_Pct)



# plot it
ggplot(dfCountyMap) + 
  geom_polygon(colour="black", aes(x=long, y=lat, group=group, fill=Unemploy_Pct)) + 
  labs(x = NULL, y = NULL,                                                          
       title = "Unemployment Rate by County (2011) for Ireland",
       subtitle = "Sources: Census 2011", 
       caption = "Plot by C.Finegan d21124026") +  
  theme(axis.line=element_blank(), 
        axis.ticks=element_blank(), 
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid = element_blank(),
        plot.caption.position = 'plot',
        plot.title.position = 'plot',
        legend.position = "bottom")+ 
  scale_fill_discrete(name = "Unemployment Rate (%)") +
  #scale_fill_brewer(palette="YlOrRd")
  scale_fill_brewer(palette="OrRd")


