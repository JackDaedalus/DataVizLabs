
#Variables
x<-2
y<-c(2,3,4,5)
y

#Accessing values...........Find differences with Python
y[1]
y[1:3]
y[-3]
class(x)    ###data type x

#Creating a dataframe
#setting up the components
x<-1:10
y<-4:13
comments<-c("good", "basic", "Excellent","good", "basic","good", "basic","Excellent","good","Excellent")
#Creating the dataframe
mydf<-data.frame(x,y,comments)
#displaying the dataframe
mydf
#displaying the first few lines of the dataframe

#Reading data into a data frame
adult<-read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data',header=FALSE,sep=',')

#check the size of the data frame
dim(adult)

# View of initial rows in dataframe
head(adult)

#add the column names 
colnames(adult) <- c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hours_per_week","native_country","income_bracket")

#Have a look at the first few lines of the data frame
head(adult)

#checking for missing values
apply(is.na(adult),2,sum)

### Create a dataframe with the information from the table : http://datasets.flowingdata.com/hot-dog-contest-winners.csv
hotdogs<-read.table('http://datasets.flowingdata.com/hot-dog-contest-winners.csv',header=FALSE,sep=',')


### Inspect the dataframe (size, columns, names etc..)
# Check layout of dataframe
dim(hotdogs)
head(hotdogs)


#checking for missing values
#head(hotdogs,15)
apply(is.na(hotdogs),2,sum)
#head(hotdogs,15)



#filtering information by year
hotdogs[which(hotdogs[,1]>1990),]

#Inspect if there are any changes in the orginial dataframe
head(hotdogs,1)

#create a new dataframe that contains information about the hotdogs competition 
#only for cases when more than 40 hotdogs were eaten
#df_big_hotdogs = hotdogs[which(hotdogs[,3]>45),]
#head(df_big_hotdogs)
#df_big_hotdogs

#Scatter plot with years on the x axis and hotdogs eaten on the y axis   ###Uncomment next line
colnames(hotdogs)[3] ="Dogseaten"
head(hotdogs,1)


#convert column 'a' from character to numeric
hotdogs$Dogseaten <- as.numeric(hotdogs$Dogseaten)
hotdogs = na.omit(hotdogs)
head(hotdogs)

plot(hotdogs$Dogseaten~as.integer(hotdogs$Year), data = hotdogs)

plot(hotdogs$Dogseaten~as.integer,(hotdogs$Year), data = hotdogs)

plot(hotdogs$Dogseaten,(hotdogs$Year), data = hotdogs)

plot(hotdogs$Dogseaten,(hotdogs$Year))
#Replace the axis titles

### create a histogram of the amount of hotdogs eaten   .... use hist(....)


##Add main title and axis titles

#create a boxplot for hotdogs eaten   ... use boxplot(....)


##Add main title and axis titles

### Barplot hotdogs eaten per year   ... use barplot(....)




library(ggplot2)

#scatter plot ggplot2
#ggplot(hotdogs,aes(x=Year,y=Dogs.eaten))+geom_point()

###Customize the labels


#small multiples using facet_wrap  scatter plot ggplot2



