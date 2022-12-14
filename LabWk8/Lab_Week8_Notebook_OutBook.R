---
title: "R Notebook"
output: html_notebook
---

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

```{r}
# Lab - Week 8

# Use a visualisation to support the answer for each question. 
# Experiment with the different types of charts and options ggplot offers. 
# Please place the visualisations underneath each question.

# Set Up Libraries
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(sqldf)
library(tidyr) # tidy data

```

```{r}

# 1. What are the total marks (oral plus written divided by two) for each student for each subject? (2 marks)



# Impute missing values
avgmark <- sqldf("select AVG(Mark_Oral) from studentresult where Name = 'Mary Healy' AND Mark_Oral is not 'NA'")
avgmark

studentresult$Mark_Oral <- ifelse(is.na(studentresult$Mark_Oral), as.numeric(avgmark), studentresult$Mark_Oral)


# SQL to Set Up Exercise 1 + Exercise 2

# Convert date to a "real date" - Exercise 2
studentresult$DOB <- as.Date(studentresult$DOB, '%d-%m-%Y')

# Add today's date, also as a "real date" - Exercise 2
studentresult$today <- as.Date(Sys.Date())


# Average Result - Exercise 1
avgTotalResults <- sqldf('select Name, subject, 
                                      sum(Mark_Written) as Sum_Written_Mrks, 
                                      sum(Mark_Oral) as Sum_Oral_Mks,
                                      count(year) as Num_Years,
                                      (((sum(Mark_Written)+sum(Mark_Oral))/2)/count(year)) as Overall_Avg,
                                      floor((today-DOB)/365.25) as Age
                                      from studentresult group by Name, subject')

View(avgTotalResults)

# Question 1 Bar Graphs

gg <- ggplot(avgTotalResults,aes(x = Name, y = Overall_Avg, fill = Subject, 
                                 label = round(Overall_Avg, digits = 0)))
gg <- gg + geom_bar(position = "stack", stat = "identity", width=0.35)
gg <- gg + scale_fill_viridis(discrete = T, option = "E")
gg <- gg + ggtitle("Student Body Marks") 
gg <- gg + facet_wrap(~Subject, nrow = 1, scales = "free")
gg <- gg + theme_ipsum()
gg <- gg + labs(y = "Average Marks For Each Student")
gg <- gg + geom_label(
  aes(label = round(Overall_Avg, digits = 0)), 
  vjust=1.15,
  size = 3, fontface = "bold", family = "Fira Sans",
  ## turn into white box without outline
  fill = "white", label.size = 0)
gg <- gg + theme(legend.position = "none")
print(gg)

```




