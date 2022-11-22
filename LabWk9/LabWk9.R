# Lab - Week 0

# Recreate given graphs using RStudio and ggploy

# Load libraries
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(sqldf)
library(tidyr) # tidy data


# Impute missing values in Student Record
avgmark <- sqldf("select AVG(Mark_Oral) from studentresult where Name = 'Mary Healy' AND Mark_Oral is not 'NA'")
avgmark

studentresult$Mark_Oral <- ifelse(is.na(studentresult$Mark_Oral), as.numeric(avgmark), studentresult$Mark_Oral)



# Exercise 1: Looking at last week’s studentresults file, 
# plot the results for the written and oral exams in two bars per student. (4 marks)


# Build SQL for all students average marks
avgOralScore <- sqldf("select Name,
                       sum(Mark_Written)/count(Mark_Written) as markw,
                       sum(Mark_Oral)/count(Mark_Oral) as marko
                       from studentresult
                       group by Name")

# Review aggregations of average score for students
View(avgOralScore)

# Pivot table to prepare data for bar chart
studentMarkComps_Reshape <- avgOralScore %>% 
  pivot_longer(c(markw, marko), # values to pivot or reshape
               names_to = "Exam_Type", # Rename column for exam type (written or oral)
               values_to = "Mark_Score") # Re-name column containing the exam scores


# Change data order to adjust graph legend
studentMarkComps_Reshape$Exam_Type <- factor(studentMarkComps_Reshape$Exam_Type, levels = c("markw","marko"))

# Display dataset
studentMarkComps_Reshape

# Question 1 Bar Graphs - Compare Average Student Results
# Order by ascending marks values
gg1 <- ggplot(data=studentMarkComps_Reshape, aes(x=reorder(Name, +Mark_Score), y=Mark_Score, fill=Exam_Type)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  labs(y = "Student Marks (Average)", x = "Student Name") + 
  ggtitle('Average Student Performance Across Exam Types') +
  theme(legend.position = "right",
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size=12, face="bold", colour = "black")) + 
  scale_fill_discrete(labels=c('Marks - Written', 'Marks - Oral'),name = "Exam Type") 
print(gg1)



###############################################################################################################
# Exercise 2: Plot one variable counting the instances that fall in each bin (Histogram). 
# Bin size can be adjusted with the command binwidth. (1 mark)
###############################################################################################################

histMarksWrittenData <- sqldf("select Name, subject, Year,
                             Mark_Written
                             from studentresult
                             order by year asc")


View(histMarksWrittenData)

ggplot(histMarksWrittenData, aes(x=Mark_Written)) + 
  geom_histogram(color="red", fill="white", binwidth=17)


