# -------------------------------------------------- #
#
# Data Visualisation Assignment 2
# Visualisations in R
#
# Student No. d21124026
# Name : Ciaran Finnegan
# TU060 Data Science MSc
#
# December 2022


# Visualisation Two - Socio-economic group comparison
# -------------------------------------------------- #


# load required libraries
library(curl)
library(readr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(viridis)
library(dplyr)




# Load Census Theme Data for 2011 for Irish counties
# Select only the required Socio-economic data 
# Rename the columns to increase understanding of the data
# df2011CountyThemes <- read_delim("https://www.cso.ie/en/media/csoie/census/documents/saps2011files/AllThemesTablesCTY.csv",show_col_types = FALSE) %>%
#  select(GEOGID, GEOGDESC, T8_1_LFFJT, T8_1_ULGUPJT, T8_1_TT) %>%
#  rename(Looking_for_Work = T8_1_LFFJT, Unemployed = T8_1_ULGUPJT, Total_Workforce = T8_1_TT)


df2011CountyThemes <- read_delim("https://www.cso.ie/en/media/csoie/census/documents/saps2011files/AllThemesTablesCTY.csv",
                                 show_col_types = FALSE) %>%
  select(GEOGID, GEOGDESC, 
          T9_2_HA,
          T9_2_HB,
          T9_2_HC,
          T9_2_HD,
          T9_2_HE,
          T9_2_HF,
          T9_2_HG,
          T9_2_HH,
          T9_2_HI,
          T9_2_HJ,
          T9_2_HZ) %>%
  rename(GroupA = T9_2_HA,
         GroupB = T9_2_HB,
         GroupC = T9_2_HC,
         GroupD = T9_2_HD,
         GroupE = T9_2_HE,
         GroupF = T9_2_HF,
         GroupG = T9_2_HG,
         GroupH = T9_2_HH,
         GroupI = T9_2_HI,
         GroupJ = T9_2_HJ,
         GroupZ = T9_2_HZ)

View(df2011CountyThemes)




# Sum County Data into a single row



# Pivot County Data for numbers in Each Socio-economic Group



# Add column to identify numbers in each socio-economic group




# Plot 2011 data in isolation - temporary graph
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
        legend.position = "bottom") + 
  #scale_fill_brewer(palette="OrRd") + 
  labs(fill = "Unemployment Rate (%)")+
  scale_fill_brewer(palette="YlOrRd")


