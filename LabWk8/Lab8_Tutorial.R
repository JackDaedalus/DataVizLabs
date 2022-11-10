# install.packages('ggplot2')

# library(ggplot2)
library(sqldf)

# Ex1: What are the average results in written exams across all subjects and all years per student?

markswritten <- sqldf('select Name, avg(Mark_Written) as Written_marks from studentresult group by Name')

View(markswritten)

markswritten2<-aggregate(data=studentresult, Mark_Written~ Name,mean)
markswritten2


# Ex2: Impute missing values

# Version 1 with sqldf
avgmark <- sqldf("select AVG(Mark_Oral) from studentresult where Name = 'Mary Healy' AND Mark_Oral is not 'NA'")
avgmark

studentresult$Mark_Oral <- ifelse(is.na(studentresult$Mark_Oral), as.numeric(avgmark), studentresult$Mark_Oral)

# Graphing Exercises
# Ex1. What are the average results in written exams across all subjects and all years per student?

namesAvgmarkplot<-ggplot(data=markswritten, aes(x= Name, y=Written_marks)) + geom_bar(stat="identity",aes(fill=Name), width = 0.25)
namesAvgmarkplot+ scale_fill_manual(values=c('Hercule Poirot'='steelblue', "Joe O'Neil"='firebrick', 'Mary Healy'='darkgreen'))


# Ex3: What are the average results in the written exams per student and year?
mby <- sqldf("select year, name, sum(Mark_Written)/5 as mark from studentresult group by year, name")
View(mby)
ggplot(data=mby, aes(x= Name, y=mark,fill=as.factor(Year))) + geom_bar(stat="identity")
  
  
  
  
  
  