# -------------------------------------------------- #
# -------------------------------------------------- #

# Data Visualisation Assignment 2
# Visualisations in R
#
# Student No. d21124026
# Name : Ciaran Finnegan
# TU060 Data Science MSc
#
# December 2022


# Visualisation Three - comparison of Population 
# Health against Socio-economic groups in Census 2011 
# and Census 2016 data for Irish counties
# -------------------------------------------------- #
# -------------------------------------------------- #

#install.packages("gghighlight")
#install.packages("ggthemes") # Install 

# load required libraries
library(curl)
library(readr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(viridis)
library(dplyr)
library(scales)
library(gghighlight)
library(ggthemes)
library(stringr)



# ---------------------------------------------------- #
# Download Census Theme Data for 2016 for Irish counties
# ---------------------------------------------------- #

# Prepare URL string with location on GitHub of ZIP file with Census 2016 'Theme' data
sGitHub_Datasource1_2016 <-"https://github.com/JackDaedalus/DataVizLabs/raw/"
sGitHub_Datasource2_2016 <- paste(sGitHub_Datasource1_2016,"main/CA2/", sep = "", collapse=NULL)
sGitHub_Datafile_2016 <- "SAPS2016_CTY31.zip"
sGitHub_Datasource_2016 <- paste(sGitHub_Datasource2_2016,sGitHub_Datafile_2016, sep = "", collapse=NULL)
f2016CTY_data <- sGitHub_Datasource_2016


# Download zip file from from GitHub and extract 2016 Theme data for Irish counties
temp_3 <- tempfile()
temp_4 <- tempfile()
source <- f2016CTY_data
temp_3 <- curl_download(url = source, destfile = temp_3, quiet = FALSE)
unzip(temp_3, exdir = temp_4)

# Prepare location string of downloaded 2016 Census data CSV file 
f2016CensusData <- "\\SAPS2016_CTY31.csv"
f2016CensusData <- paste(temp_4,f2016CensusData, sep = "", collapse=NULL)



# ---------------------------------------------------- #
# Prepare URL for download of Census Theme Data for 2011 
# for Irish counties
# ---------------------------------------------------- #

# Prepare URL string for Census Theme Data for 2011 for Irish counties
f2011CensusData <- "https://www.cso.ie/en/media/csoie/census/documents/saps2011files/AllThemesTablesCTY.csv"





# -----------------------------------------------------#
# Prepare sequence of Census data to be downloaded and 
# read into dataframes for processing
# -----------------------------------------------------#

# Set up array of Census file names to be loaded in sequence
arrCensusThemeFiles <- c(f2011CensusData, f2016CensusData)
arrCensusThemeYears <- c('2011','2016') # 2011 data is downloaded first for manipulation



# Set up dataframe array to hold the 2011 and 2016 Census Theme data
# Both sets of data will undergo the same transformation before
# being merged in advance of graph generation
arrDFYrCountySocioThemes          <- list()  # start with empty array for data 
                                             # loaded from files
arrDFYrCountySocioThemes_Modified <- list()  # array to store dataframes after 
                                             # the data wrangling process
dfCensusGraph <- list()


# -----------------------------------------------------#
# Iterate through the 2011 and 2016 files and manipulate 
# the socio-economic data for visualisation
# -----------------------------------------------------#
#i <- 1
for (i in 1:(length(arrCensusThemeFiles))) {

  
  if (arrCensusThemeYears[i] == '2011') { # The 2011 Census Theme columns for 
                                          # Health data have a different format
    
    # Select only the required Socio-economic and Health data 
    # Rename the columns to increase understanding of the data
    
    # Read Census 2011 data from CSO website
    arrDFYrCountySocioThemes[[i]] <- read_delim(arrCensusThemeFiles[i], 
                                          show_col_types = FALSE) %>%   
      
      select(GEOGID, GEOGDESC, # Select the county identifier and the following... 
              T9_2_PA,         # Socio-economic Group A
              T9_2_PB,         # Socio-economic Group B
              T9_2_PT,         # Total population in Socio-economic Groups
              T12_3VGT,       # Population in 'Very Good Health'
              T12_3GT,        # Population in 'Good Health'
              T12_3TT         # Total Population in health census
                      ) %>%
      rename(GroupA         = T9_2_PA,  # Rename Columns to improve readability
             GroupB         = T9_2_PB,
             GroupsTotal    = T9_2_PT,
             VeryGoodHealth = T12_3VGT,
             GoodHealth     = T12_3GT,
             HealthTotal    = T12_3TT)
    

    
  } else {
    
    # Read 2016 file - formats of Theme 12 column headings are different than those in 2011
    arrDFYrCountySocioThemes[[i]] <- read_delim(arrCensusThemeFiles[i], 
                                                show_col_types = FALSE) %>%   # Read Census 2011 data from CSO website
      select(GEOGID, GEOGDESC, # Select the county identifier and the following... 
             T9_2_PA,          # Socio-economic Group A
             T9_2_PB,          # Socio-economic Group B
             T9_2_PT,          # Total population in Socio-economic Groups
             T12_3_VGT,        # Population in 'Very Good Health'
             T12_3_GT,         # Population in 'Good Health'
             T12_3_TT          # Total Population in health census
      ) %>%
      rename(GroupA         = T9_2_PA,  # Rename Columns to improve readability
             GroupB         = T9_2_PB,
             GroupsTotal    = T9_2_PT,
             VeryGoodHealth = T12_3_VGT,
             GoodHealth     = T12_3_GT,
             HealthTotal    = T12_3_TT)
    

    # Correct for fada in Dun-Laoighaire thowing a text error
    arrDFYrCountySocioThemes[[i]][8, "GEOGDESC"] <- "Dun Laoghaire-Rathdown"
    
  }

  # Create dataframe for year in array - 2011 or 2016  
  dfThisYrCountySocioThemes <- arrDFYrCountySocioThemes[[i]]
  
  # Add Year Value as label to dataframe
  dfThisYrCountySocioThemes$Year <- arrCensusThemeYears[i]
  
  
  # Calculate Ratio by County of Population in Socio-economic groups A and B.
  dfThisYrCountySocioThemes$GroupAB_Pct  <- ((dfThisYrCountySocioThemes$GroupA 
                                                + dfThisYrCountySocioThemes$GroupB) 
                                               / dfThisYrCountySocioThemes$GroupsTotal) * 100
  
  
  # Calculate Ratio by County of Population in 'Very Good' health.
  dfThisYrCountySocioThemes$VGHealth_Pct  <- (dfThisYrCountySocioThemes$VeryGoodHealth
                                             / dfThisYrCountySocioThemes$HealthTotal) * 100


  
  # Set up dataframe in array after data manipulation complete
  arrDFYrCountySocioThemes_Modified[[i]] <- dfThisYrCountySocioThemes
  
  
  
  # -----------------------------------------------------#
  # Merge the 2011 and 2016 dataframes and plot the 
  # comparisons in a horizontal bar chart
  # -----------------------------------------------------#
  
  # Copy data to a dataframe with a shorter name to simplify
  # the code generating the graphs
  dfCensusGraph[[i]] <- arrDFYrCountySocioThemes_Modified[[i]]
  
  # Format graph titles - avoids distortion when rendering the graph
  sGraphTitle <- "Relationship of Higher Socio-Economic Groups to Health in Ireland - "
  sGraphSubTitle <- "Sources: Census "
  # Add 'Year' for given dataframe in this loop
  sGraphTitle <- paste(sGraphTitle, arrCensusThemeYears[i], 
                       sep = "", collapse=NULL)
  sGraphSubTitle <- paste(sGraphSubTitle, arrCensusThemeYears[i], 
                          sep = "", collapse=NULL)
  
  
  # These parameter works best for highlighting specific data points 
  # in the 2011/2016 Scatterplot graph
  UpperGroupAB_Pct  <- 27
  LowerGroupAB_Pct  <- 16 
  LowerVGHealth_Pct <- 56

  
  # Generate Horizontal Bar Chart
  gg1 <- ggplot(data=dfCensusGraph[[i]], aes(x = GroupAB_Pct, y=VGHealth_Pct)) +
    geom_point(colour="black", size=3, shape=21, fill="red") +
    # Highlight on Scatter plot the county area at the upper and lower end of 
    # the socio-economic groups and health rating.
    gghighlight(((GroupAB_Pct>=UpperGroupAB_Pct|GroupAB_Pct<=LowerGroupAB_Pct) 
                  | VGHealth_Pct<=LowerVGHealth_Pct), 
                label_key = GEOGDESC,
                unhighlighted_params 
                = list(colour = "blue", 
                       fill="black",
                       size=1.5),
                label_params = list(fill="red", 
                                    colour="white",
                                    size=3.75))+
    # Label the graph
    labs(x = "% County Population in Socio-Economic Groups A and B", 
         y = "% County Population in 'Very Good Health'", 
         # Format title to avoid distortion of the graph
         title = str_wrap(sGraphTitle, 60),
         subtitle = sGraphSubTitle, 
         caption = "Plot by C.Finegan d21124026") +  
    # Add a dashed regression line and a confidence interval for relationship  
    geom_smooth(method='lm', se=TRUE, linetype="dashed",
                color="darkred", fullrange=TRUE, fill="light grey") +
    # Use GDocs theme from ggplot themes library
    theme_gdocs()
  
  
  print(gg1)
  

}











