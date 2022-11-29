

# Load libraries
library(readr)
library(ggplot2)
library(sqldf)
#library(viridis)
#library(hrbrthemes)
#library(tidyr) # tidy data

# Read CSV file with Olympoic Data
olympicdata<- read.csv( 'OlympicGames.csv' , sep= ',' , header=T)


# Build dataset with gold medals per country
resultsmedalsgold<-sqldf("select country, 
                          count(Medal) as gold 
                          from olympicdata where 
                          Medal=='gold' 
                          group by country")

# Build dataset with silver medals per country
resultsmedalssilver<-sqldf("select country, 
                            count(Medal) as silver 
                            from olympicdata 
                            where Medal=='silver' 
                            group by country") 

# Build dataset with bronze medals per country
resultsmedalsbronze<-sqldf("select country, 
                            count(Medal) as bronze 
                            from olympicdata 
                            where Medal=='bronze' 
                            group by country")

# Combine medals dataframes
resultsmedals<-merge(resultsmedalsgold,resultsmedalssilver,by="Country",all=TRUE) 
resultsmedals2<-merge(resultsmedals,resultsmedalsbronze,by="Country",all=TRUE)


# Checking for NA Values - using North Korea as an example
resultsmedalsNK2<-sqldf("select * from
                          resultsmedals2 
                          where Country=='North Korea'")
resultsmedalsNK2

# Replace NA values with zero
resultsmedals2[is.na(resultsmedals2)] <- 0
# Check NA values converted to zero - using North Korea as an example
resultsmedalsNK2<-sqldf("select * from
                          resultsmedals2 
                          where Country=='North Korea'")
resultsmedalsNK2


# Visualization One - What country has won most Silver medals since 2000? (2 marks)

dfMostSilverMedalsCountry<-sqldf("select country, 
                            count(Medal) as Silver 
                            from olympicdata 
                            where Medal=='silver' 
                            and Year >= 2000
                            group by country
                            order by Silver desc
                            limit 7") 


dfMostSilverMedalsCountry

# Visualization 1 - Horizontal Bar Chart of top performing silver medalist countries
# since 2000 (Top 7)
gg1 <- ggplot(data=dfMostSilverMedalsCountry, aes(x=reorder(Country, +Silver), y=Silver)) +
  geom_bar(stat="identity", colour="black", fill="blue") +
  labs(y = "Count of Silver Medals", x = "Country Name") + 
  ggtitle('Countries With Most Silver Medals Since 2000 (Top 7)') +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size=12, face="bold", colour = "black")) +
  coord_flip() + 
  geom_label(
    aes(label = round(Silver)), 
    #hjust=1.15,
    size = 2.5, fontface = "bold",
    ## turn into white box without outline
    fill = "white", label.size = 0.5)
print(gg1)




















