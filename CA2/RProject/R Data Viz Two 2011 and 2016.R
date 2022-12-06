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
library(scales)




# Load Census Theme Data for 2011 for Irish counties
# Select only the required Socio-economic data 
# Rename the columns to increase understanding of the data
df2011CountySocioThemes <- read_delim("https://www.cso.ie/en/media/csoie/census/documents/saps2011files/AllThemesTablesCTY.csv",
                                 show_col_types = FALSE) %>%   # Read Census 2011 data from CSO website
  select(GEOGID, GEOGDESC, # Only select the county identifier and the numbers of people in each
          T9_2_PA,         # Socio-economic group
          T9_2_PB,
          T9_2_PC,
          T9_2_PD,
          T9_2_PE,
          T9_2_PF,
          T9_2_PG,
          T9_2_PH,
          T9_2_PI,
          T9_2_PJ,
          T9_2_PZ) %>%
  rename(GroupA = T9_2_PA,  # Rename Columns to improve readability
         GroupB = T9_2_PB,
         GroupC = T9_2_PC,
         GroupD = T9_2_PD,
         GroupE = T9_2_PE,
         GroupF = T9_2_PF,
         GroupG = T9_2_PG,
         GroupH = T9_2_PH,
         GroupI = T9_2_PI,
         GroupJ = T9_2_PJ,
         GroupZ = T9_2_PZ)

View(df2011CountySocioThemes)




# Sum County Data into a single row

# Group the counties and sum all socio-economic data for Ireland overall
df2011CtyThemesSocioTotals <- sqldf("Select 'CTT' as GEOGID,
                                    '2011' as Year,
                                     sum(GroupA) as GrpA,
                                     sum(GroupB) as GrpB,
                                     sum(GroupC) as GrpC,
                                     sum(GroupD) as GrpD,
                                     sum(GroupE) as GrpE,
                                     sum(GroupF) as GrpF,
                                     sum(GroupG) as GrpG,
                                     sum(GroupH) as GrpH,
                                     sum(GroupI) as GrpI,
                                     sum(GroupJ) as GrpJ,
                                     sum(GroupZ) as GrpZ
                                     from df2011CountySocioThemes
                                     group by Year")

head(df2011CtyThemesSocioTotals)
str(df2011CtyThemesSocioTotals)



# Pivot County Data for numbers in Each Socio-economic Group
df2011CtyThemesSocioTotals_Reshape <- df2011CtyThemesSocioTotals %>% 
  pivot_longer(c(GrpA,
                 GrpB,
                 GrpC,
                 GrpD,
                 GrpE,
                 GrpF,
                 GrpG,
                 GrpH,
                 GrpI,
                 GrpJ,
                 GrpZ), # values to pivot or reshape
               names_to = "SocioEcon_Group", # Rename column for exam type (written or oral)
               values_to = "Numbers_in_Group") # Re-name column containing the exam scores

view(df2011CtyThemesSocioTotals_Reshape)
str(df2011CtyThemesSocioTotals_Reshape)



# Plot socio-economic groups

#sort x-axis variable in alphabetical order - top down from Group A to Z
level_order <- c('GrpZ',
                 'GrpJ',
                 'GrpI',
                 'GrpH',
                 'GrpG',
                 'GrpF',
                 'GrpE',
                 'GrpD',
                 'GrpC',
                 'GrpB',
                 'GrpA')


gg1 <- ggplot(data=df2011CtyThemesSocioTotals_Reshape, aes(x = factor(SocioEcon_Group, level = level_order), 
                                                           y=Numbers_in_Group, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  labs(x = "Socio-Economic Groups", y = "Population (numbers)",                                                          
       title = "Breakdown of Socio-Economic Groups (2011) for Ireland",
       subtitle = "Sources: Census 2011", 
       caption = "Plot by C.Finegan d21124026") +  
  theme(legend.position = "right",
        plot.title = element_text(size = 22),
        axis.title = element_text(size = 19),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=12, face="bold", colour = "black")) + 
  scale_fill_discrete(labels=c('2011', '2016'),name = "Year") +
  scale_y_continuous(labels = comma) +
  coord_flip()
print(gg1)


