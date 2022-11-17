# Lab - Week 8

# Use a visualisation to support the answer for each question. 
# Experiment with the different types of charts and options ggplot offers. 
# Please place the visualisations underneath each question.

# 1. What are the total marks (oral plus written divided by two) for each student for each subject? (2 marks)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(sqldf)


# Impute missing values
avgmark <- sqldf("select AVG(Mark_Oral) from studentresult where Name = 'Mary Healy' AND Mark_Oral is not 'NA'")
avgmark

studentresult$Mark_Oral <- ifelse(is.na(studentresult$Mark_Oral), as.numeric(avgmark), studentresult$Mark_Oral)


avgTotalResults <- sqldf('select Name, subject, 
                                      sum(Mark_Written) as sum_Written, 
                                      sum(Mark_Oral) as sum_Oral,
                                      count(year) as Num_Years,
                                      (((sum(Mark_Written)+sum(Mark_Oral))/2)/count(year)) as Overall_Avg
                                      from studentresult group by Name, subject')

View(avgTotalResults)


gg <- ggplot(avgTotalResults,aes(x = Name, y = Overall_Avg, fill = Subject, label = round(Overall_Avg, digits = 0)))
gg <- gg + geom_bar(position = "stack", stat = "identity",width=0.4)
gg <- gg + scale_fill_viridis(discrete = T, option = "E")
gg <- gg + ggtitle("Student Body Marks") 
gg <- gg + facet_wrap(~Subject)
gg <- gg + theme_ipsum()
gg <- gg + labs(y = "Average Marks")
#gg <- gg + geom_text(aes(label = round(Overall_Avg, digits = 0), vjust=-0.3)) 
#gg <- gg + geom_text(size = 3, position = position_stack(vjust = 0.5))
gg <- gg + geom_label(
  aes(label = round(Overall_Avg, digits = 0)), 
  #vjust=-0.3,
  size = 3, fontface = "bold", family = "Fira Sans",
  ## turn into white box without outline
  fill = "white", label.size = 0)
gg <- gg + theme(legend.position = "bottom")
print(gg)


# 2. What is the relationship between age and mark? (2 marks)



# 3. Did any students do better on their written compared with their oral (or vice versa)? (2 marks)



# 4. What subject obtained the best results on average? (2 marks)




# 5. What are the average results in oral exams across all subjects and all years per student? (2 marks)