library(caret)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(VIM)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(eeptools)

#loading all the data into R with the read.csv function.

#basic approach is to combine each dataset by year. then clean and update the columns for continous
#unchanging columns such as highschool diploma, while dropping the old columns. this will further
#reduce the missing na values while condensing our table's column length. 
#in the master progress data set, all 13k students should have all their previous info, cummulative gpa
#and term gpa upon graduation or dismissal. as well as other 

#split the data by each academic year

progfall11 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Fall 2011_SP.csv")
as.data.frame(colnames(progfall11))
progfall11 <- progfall11[-c(2,3,4,5)]
progspring12 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Spring 2012_SP.csv")
as.data.frame(colnames(progspring12))
progspring12 <- progspring12[-c(2,3,4,5)]
prog1 <- full_join(progfall11, progspring12, by = 'StudentID')

#update missing values
prog1$CompleteDevMath.y <- ifelse(is.na(prog1$CompleteDevMath.y),prog1$CompleteDevMath.x,prog1$CompleteDevMath.y)
prog1$CompleteDevEnglish.y <- ifelse(is.na(prog1$CompleteDevEnglish.y),prog1$CompleteDevEnglish.x,prog1$CompleteDevEnglish.y)
prog1$Major1.y <- ifelse(is.na(prog1$Major1.y),prog1$Major1.x,prog1$Major1.y)
prog1$Major2.y <- ifelse(is.na(prog1$Major2.y),prog1$Major2.x,prog1$Major2.y)
prog1$Complete1.y <- ifelse(is.na(prog1$Complete1.y),prog1$Complete1.x,prog1$Complete1.y)
prog1$Complete2.y <- ifelse(is.na(prog1$Complete2.y),prog1$Complete2.x,prog1$Complete2.y)
prog1$CompleteCIP1.y <- ifelse(is.na(prog1$CompleteCIP1.y),prog1$CompleteCIP1.x, prog1$CompleteCIP1.y)
prog1$CompleteCIP2.y <- ifelse(is.na(prog1$CompleteCIP2.y),prog1$CompleteCIP2.x, prog1$CompleteCIP2.y)
prog1$TransferIntent.y <- ifelse(is.na(prog1$TransferIntent.y),prog1$TransferIntent.x,prog1$TransferIntent.y)
prog1$DegreeTypeSought.y <- ifelse(is.na(prog1$DegreeTypeSought.y),prog1$DegreeTypeSought.x,prog1$DegreeTypeSought.y)
prog1$TermGPA.y <- ifelse(is.na(prog1$TermGPA.y),prog1$TermGPA.x,prog1$TermGPA.y)
prog1$CumGPA.y <- ifelse(is.na(prog1$CumGPA.y),prog1$CumGPA.x,prog1$CumGPA.y)

#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(prog1))
prog1 <- prog1[-c(2:13)]

#repeat the process for every academic year. 
progfall12 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Fall 2012_SP.csv")
as.data.frame(colnames(progfall12))
progfall12 <- progfall12[-c(2,3,4,5)]
progspring13 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Spring 2013_SP.csv")
as.data.frame(colnames(progspring13))
progspring13 <- progspring13[-c(2,3,4,5)]
prog2 <- full_join(progfall12, progspring13, by = 'StudentID')

#update the missing values
prog2$CompleteDevMath.y <- ifelse(is.na(prog2$CompleteDevMath.y),prog2$CompleteDevMath.x,prog2$CompleteDevMath.y)
prog2$CompleteDevEnglish.y <- ifelse(is.na(prog2$CompleteDevEnglish.y),prog2$CompleteDevEnglish.x,prog2$CompleteDevEnglish.y)
prog2$Major1.y <- ifelse(is.na(prog2$Major1.y),prog2$Major1.x,prog2$Major1.y)
prog2$Major2.y <- ifelse(is.na(prog2$Major2.y),prog2$Major2.x,prog2$Major2.y)
prog2$Complete1.y <- ifelse(is.na(prog2$Complete1.y),prog2$Complete1.x,prog2$Complete1.y)
prog2$Complete2.y <- ifelse(is.na(prog2$Complete2.y),prog2$Complete2.x,prog2$Complete2.y)
prog2$CompleteCIP1.y <- ifelse(is.na(prog2$CompleteCIP1.y),prog2$CompleteCIP1.x, prog2$CompleteCIP1.y)
prog2$CompleteCIP2.y <- ifelse(is.na(prog2$CompleteCIP2.y),prog2$CompleteCIP2.x, prog2$CompleteCIP2.y)
prog2$TransferIntent.y <- ifelse(is.na(prog2$TransferIntent.y),prog2$TransferIntent.x,prog2$TransferIntent.y)
prog2$DegreeTypeSought.y <- ifelse(is.na(prog2$DegreeTypeSought.y),prog2$DegreeTypeSought.x,prog2$DegreeTypeSought.y)
prog2$TermGPA.y <- ifelse(is.na(prog2$TermGPA.y),prog2$TermGPA.x,prog2$TermGPA.y)
prog2$CumGPA.y <- ifelse(is.na(prog2$CumGPA.y),prog2$CumGPA.x,prog2$CumGPA.y)

#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(prog2))
prog2 <- prog2[-c(2:13)]




progfall13 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Fall 2013_SP.csv")
as.data.frame(colnames(progfall13))
progfall13 <- progfall13[-c(2,3,4,5)]
progspring14 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Spring 2014_SP.csv")
as.data.frame(colnames(progspring14))
progspring14 <- progspring14[-c(2,3,4,5)]
prog3 <- full_join(progfall13, progspring14, by = 'StudentID')

#update the missing values
prog3$CompleteDevMath.y <- ifelse(is.na(prog3$CompleteDevMath.y),prog3$CompleteDevMath.x,prog3$CompleteDevMath.y)
prog3$CompleteDevEnglish.y <- ifelse(is.na(prog3$CompleteDevEnglish.y),prog3$CompleteDevEnglish.x,prog3$CompleteDevEnglish.y)
prog3$Major1.y <- ifelse(is.na(prog3$Major1.y),prog3$Major1.x,prog3$Major1.y)
prog3$Major2.y <- ifelse(is.na(prog3$Major2.y),prog3$Major2.x,prog3$Major2.y)
prog3$Complete1.y <- ifelse(is.na(prog3$Complete1.y),prog3$Complete1.x,prog3$Complete1.y)
prog3$Complete2.y <- ifelse(is.na(prog3$Complete2.y),prog3$Complete2.x,prog3$Complete2.y)
prog3$CompleteCIP1.y <- ifelse(is.na(prog3$CompleteCIP1.y),prog3$CompleteCIP1.x, prog3$CompleteCIP1.y)
prog3$CompleteCIP2.y <- ifelse(is.na(prog3$CompleteCIP2.y),prog3$CompleteCIP2.x, prog3$CompleteCIP2.y)
prog3$TransferIntent.y <- ifelse(is.na(prog3$TransferIntent.y),prog3$TransferIntent.x,prog3$TransferIntent.y)
prog3$DegreeTypeSought.y <- ifelse(is.na(prog3$DegreeTypeSought.y),prog3$DegreeTypeSought.x,prog3$DegreeTypeSought.y)
prog3$TermGPA.y <- ifelse(is.na(prog3$TermGPA.y),prog3$TermGPA.x,prog3$TermGPA.y)
prog3$CumGPA.y <- ifelse(is.na(prog3$CumGPA.y),prog3$CumGPA.x,prog3$CumGPA.y)

#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(prog3))
prog3 <- prog3[-c(2:13)]


progfall14 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Fall 2014_SP.csv")
as.data.frame(colnames(progfall14))
progfall14 <- progfall14[-c(2,3,4,5)]
progspring15 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Spring 2015_SP.csv")
as.data.frame(colnames(progspring15))
progspring15 <- progspring15[-c(2,3,4,5)]
prog4 <- full_join(progfall14, progspring15, by = 'StudentID')

#update the missing values
prog4$CompleteDevMath.y <- ifelse(is.na(prog4$CompleteDevMath.y),prog4$CompleteDevMath.x,prog4$CompleteDevMath.y)
prog4$CompleteDevEnglish.y <- ifelse(is.na(prog4$CompleteDevEnglish.y),prog4$CompleteDevEnglish.x,prog4$CompleteDevEnglish.y)
prog4$Major1.y <- ifelse(is.na(prog4$Major1.y),prog4$Major1.x,prog4$Major1.y)
prog4$Major2.y <- ifelse(is.na(prog4$Major2.y),prog4$Major2.x,prog4$Major2.y)
prog4$Complete1.y <- ifelse(is.na(prog4$Complete1.y),prog4$Complete1.x,prog4$Complete1.y)
prog4$Complete2.y <- ifelse(is.na(prog4$Complete2.y),prog4$Complete2.x,prog4$Complete2.y)
prog4$CompleteCIP1.y <- ifelse(is.na(prog4$CompleteCIP1.y),prog4$CompleteCIP1.x, prog4$CompleteCIP1.y)
prog4$CompleteCIP2.y <- ifelse(is.na(prog4$CompleteCIP2.y),prog4$CompleteCIP2.x, prog4$CompleteCIP2.y)
prog4$TransferIntent.y <- ifelse(is.na(prog4$TransferIntent.y),prog4$TransferIntent.x,prog4$TransferIntent.y)
prog4$DegreeTypeSought.y <- ifelse(is.na(prog4$DegreeTypeSought.y),prog4$DegreeTypeSought.x,prog4$DegreeTypeSought.y)
prog4$TermGPA.y <- ifelse(is.na(prog4$TermGPA.y),prog4$TermGPA.x,prog4$TermGPA.y)
prog4$CumGPA.y <- ifelse(is.na(prog4$CumGPA.y),prog4$CumGPA.x,prog4$CumGPA.y)

#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(prog4))
prog4 <- prog4[-c(2:13)]

progfall15 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Fall 2015_SP.csv")
as.data.frame(colnames(progfall15))
progfall15 <- progfall15[-c(2,3,4,5)]
progspring16 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Spring 2016_SP.csv")
as.data.frame(colnames(progspring16))
progspring16 <- progspring16[-c(2,3,4,5)]
prog5 <- full_join(progfall15, progspring16, by = 'StudentID')

#update the missing values
prog5$CompleteDevMath.y <- ifelse(is.na(prog5$CompleteDevMath.y),prog5$CompleteDevMath.x,prog5$CompleteDevMath.y)
prog5$CompleteDevEnglish.y <- ifelse(is.na(prog5$CompleteDevEnglish.y),prog5$CompleteDevEnglish.x,prog5$CompleteDevEnglish.y)
prog5$Major1.y <- ifelse(is.na(prog5$Major1.y),prog5$Major1.x,prog5$Major1.y)
prog5$Major2.y <- ifelse(is.na(prog5$Major2.y),prog5$Major2.x,prog5$Major2.y)
prog5$Complete1.y <- ifelse(is.na(prog5$Complete1.y),prog5$Complete1.x,prog5$Complete1.y)
prog5$Complete2.y <- ifelse(is.na(prog5$Complete2.y),prog5$Complete2.x,prog5$Complete2.y)
prog5$CompleteCIP1.y <- ifelse(is.na(prog5$CompleteCIP1.y),prog5$CompleteCIP1.x, prog5$CompleteCIP1.y)
prog5$CompleteCIP2.y <- ifelse(is.na(prog5$CompleteCIP2.y),prog5$CompleteCIP2.x, prog5$CompleteCIP2.y)
prog5$TransferIntent.y <- ifelse(is.na(prog5$TransferIntent.y),prog5$TransferIntent.x,prog5$TransferIntent.y)
prog5$DegreeTypeSought.y <- ifelse(is.na(prog5$DegreeTypeSought.y),prog5$DegreeTypeSought.x,prog5$DegreeTypeSought.y)
prog5$TermGPA.y <- ifelse(is.na(prog5$TermGPA.y),prog5$TermGPA.x,prog5$TermGPA.y)
prog5$CumGPA.y <- ifelse(is.na(prog5$CumGPA.y),prog5$CumGPA.x,prog5$CumGPA.y)

#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(prog5))
prog5 <- prog5[-c(2:13)]


progfall16 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Fall 2016_SP.csv")
as.data.frame(colnames(progfall16))
progfall16 <- progfall16[-c(2,3,4,5)]
progspring17 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Progress Data/Spring 2017_SP.csv")
as.data.frame(colnames(progspring17))
progspring17 <- progspring17[-c(2,3,4,5)]
prog6 <- full_join(progfall16, progspring17, by = 'StudentID')

#update the missing values
prog6$CompleteDevMath.y <- ifelse(is.na(prog6$CompleteDevMath.y),prog6$CompleteDevMath.x,prog6$CompleteDevMath.y)
prog6$CompleteDevEnglish.y <- ifelse(is.na(prog6$CompleteDevEnglish.y),prog6$CompleteDevEnglish.x,prog6$CompleteDevEnglish.y)
prog6$Major1.y <- ifelse(is.na(prog6$Major1.y),prog6$Major1.x,prog6$Major1.y)
prog6$Major2.y <- ifelse(is.na(prog6$Major2.y),prog6$Major2.x,prog6$Major2.y)
prog6$Complete1.y <- ifelse(is.na(prog6$Complete1.y),prog6$Complete1.x,prog6$Complete1.y)
prog6$Complete2.y <- ifelse(is.na(prog6$Complete2.y),prog6$Complete2.x,prog6$Complete2.y)
prog6$CompleteCIP1.y <- ifelse(is.na(prog6$CompleteCIP1.y),prog6$CompleteCIP1.x, prog6$CompleteCIP1.y)
prog6$CompleteCIP2.y <- ifelse(is.na(prog6$CompleteCIP2.y),prog6$CompleteCIP2.x, prog6$CompleteCIP2.y)
prog6$TransferIntent.y <- ifelse(is.na(prog6$TransferIntent.y),prog6$TransferIntent.x,prog6$TransferIntent.y)
prog6$DegreeTypeSought.y <- ifelse(is.na(prog6$DegreeTypeSought.y),prog6$DegreeTypeSought.x,prog6$DegreeTypeSought.y)
prog6$TermGPA.y <- ifelse(is.na(prog6$TermGPA.y),prog6$TermGPA.x,prog6$TermGPA.y)
prog6$CumGPA.y <- ifelse(is.na(prog6$CumGPA.y),prog6$CumGPA.x,prog6$CumGPA.y)

#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(prog6))
prog6 <- prog6[-c(2:13)]


#now combine the progress year per academic year to produce one dataset for all 6 acadeic years
progtotal1 <- full_join(prog1, prog2, by = 'StudentID')
#update the missing values
progtotal1$CompleteDevMath.y.y <- ifelse(is.na(progtotal1$CompleteDevMath.y.y),progtotal1$CompleteDevMath.y.x,progtotal1$CompleteDevMath.y.y)
progtotal1$CompleteDevEnglish.y.y <- ifelse(is.na(progtotal1$CompleteDevEnglish.y.y),progtotal1$CompleteDevEnglish.y.x,progtotal1$CompleteDevEnglish.y.y)
progtotal1$Major1.y.y <- ifelse(is.na(progtotal1$Major1.y.y),progtotal1$Major1.y.x,progtotal1$Major1.y.y)
progtotal1$Major2.y.y <- ifelse(is.na(progtotal1$Major2.y.y),progtotal1$Major2.y.x,progtotal1$Major2.y.y)
progtotal1$Complete1.y.y <- ifelse(is.na(progtotal1$Complete1.y.y),progtotal1$Complete1.y.x,progtotal1$Complete1.y.y)
progtotal1$Complete2.y.y <- ifelse(is.na(progtotal1$Complete2.y.y),progtotal1$Complete2.y.x,progtotal1$Complete2.y.y)
progtotal1$CompleteCIP1.y.y <- ifelse(is.na(progtotal1$CompleteCIP1.y.y),progtotal1$CompleteCIP1.y.x, progtotal1$CompleteCIP1.y.y)
progtotal1$CompleteCIP2.y.y <- ifelse(is.na(progtotal1$CompleteCIP2.y.y),progtotal1$CompleteCIP2.y.x, progtotal1$CompleteCIP2.y.y)
progtotal1$TransferIntent.y.y <- ifelse(is.na(progtotal1$TransferIntent.y.y),progtotal1$TransferIntent.y.x,progtotal1$TransferIntent.y.y)
progtotal1$DegreeTypeSought.y.y <- ifelse(is.na(progtotal1$DegreeTypeSought.y.y),progtotal1$DegreeTypeSought.y.x,progtotal1$DegreeTypeSought.y.y)
progtotal1$TermGPA.y.y <- ifelse(is.na(progtotal1$TermGPA.y.y),progtotal1$TermGPA.y.x,progtotal1$TermGPA.y.y)
progtotal1$CumGPA.y.y <- ifelse(is.na(progtotal1$CumGPA.y.y),progtotal1$CumGPA.y.x,progtotal1$CumGPA.y.y)
#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(progtotal1))
progtotal1 <- progtotal1[-c(2:13)]


progtotal2 <- full_join(prog3,prog4, by = 'StudentID')
#update the missing values
progtotal2$CompleteDevMath.y.y <- ifelse(is.na(progtotal2$CompleteDevMath.y.y),progtotal2$CompleteDevMath.y.x,progtotal2$CompleteDevMath.y.y)
progtotal2$CompleteDevEnglish.y.y <- ifelse(is.na(progtotal2$CompleteDevEnglish.y.y),progtotal2$CompleteDevEnglish.y.x,progtotal2$CompleteDevEnglish.y.y)
progtotal2$Major1.y.y <- ifelse(is.na(progtotal2$Major1.y.y),progtotal2$Major1.y.x,progtotal2$Major1.y.y)
progtotal2$Major2.y.y <- ifelse(is.na(progtotal2$Major2.y.y),progtotal2$Major2.y.x,progtotal2$Major2.y.y)
progtotal2$Complete1.y.y <- ifelse(is.na(progtotal2$Complete1.y.y),progtotal2$Complete1.y.x,progtotal2$Complete1.y.y)
progtotal2$Complete2.y.y <- ifelse(is.na(progtotal2$Complete2.y.y),progtotal2$Complete2.y.x,progtotal2$Complete2.y.y)
progtotal2$CompleteCIP1.y.y <- ifelse(is.na(progtotal2$CompleteCIP1.y.y),progtotal2$CompleteCIP1.y.x, progtotal2$CompleteCIP1.y.y)
progtotal2$CompleteCIP2.y.y <- ifelse(is.na(progtotal2$CompleteCIP2.y.y),progtotal2$CompleteCIP2.y.x, progtotal2$CompleteCIP2.y.y)
progtotal2$TransferIntent.y.y <- ifelse(is.na(progtotal2$TransferIntent.y.y),progtotal2$TransferIntent.y.x,progtotal2$TransferIntent.y.y)
progtotal2$DegreeTypeSought.y.y <- ifelse(is.na(progtotal2$DegreeTypeSought.y.y),progtotal2$DegreeTypeSought.y.x,progtotal2$DegreeTypeSought.y.y)
progtotal2$TermGPA.y.y <- ifelse(is.na(progtotal2$TermGPA.y.y),progtotal2$TermGPA.y.x,progtotal2$TermGPA.y.y)
progtotal2$CumGPA.y.y <- ifelse(is.na(progtotal2$CumGPA.y.y),progtotal2$CumGPA.y.x,progtotal2$CumGPA.y.y)
#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(progtotal2))
progtotal2 <- progtotal2[-c(2:13)]





progtotal3 <- full_join(prog5, prog6, by = 'StudentID')
#update the missing values
progtotal3$CompleteDevMath.y.y <- ifelse(is.na(progtotal3$CompleteDevMath.y.y),progtotal3$CompleteDevMath.y.x,progtotal3$CompleteDevMath.y.y)
progtotal3$CompleteDevEnglish.y.y <- ifelse(is.na(progtotal3$CompleteDevEnglish.y.y),progtotal3$CompleteDevEnglish.y.x,progtotal3$CompleteDevEnglish.y.y)
progtotal3$Major1.y.y <- ifelse(is.na(progtotal3$Major1.y.y),progtotal3$Major1.y.x,progtotal3$Major1.y.y)
progtotal3$Major2.y.y <- ifelse(is.na(progtotal3$Major2.y.y),progtotal3$Major2.y.x,progtotal3$Major2.y.y)
progtotal3$Complete1.y.y <- ifelse(is.na(progtotal3$Complete1.y.y),progtotal3$Complete1.y.x,progtotal3$Complete1.y.y)
progtotal3$Complete2.y.y <- ifelse(is.na(progtotal3$Complete2.y.y),progtotal3$Complete2.y.x,progtotal3$Complete2.y.y)
progtotal3$CompleteCIP1.y.y <- ifelse(is.na(progtotal3$CompleteCIP1.y.y),progtotal3$CompleteCIP1.y.x, progtotal3$CompleteCIP1.y.y)
progtotal3$CompleteCIP2.y.y <- ifelse(is.na(progtotal3$CompleteCIP2.y.y),progtotal3$CompleteCIP2.y.x, progtotal3$CompleteCIP2.y.y)
progtotal3$TransferIntent.y.y <- ifelse(is.na(progtotal3$TransferIntent.y.y),progtotal3$TransferIntent.y.x,progtotal3$TransferIntent.y.y)
progtotal3$DegreeTypeSought.y.y <- ifelse(is.na(progtotal3$DegreeTypeSought.y.y),progtotal3$DegreeTypeSought.y.x,progtotal3$DegreeTypeSought.y.y)
progtotal3$TermGPA.y.y <- ifelse(is.na(progtotal3$TermGPA.y.y),progtotal3$TermGPA.y.x,progtotal3$TermGPA.y.y)
progtotal3$CumGPA.y.y <- ifelse(is.na(progtotal3$CumGPA.y.y),progtotal3$CumGPA.y.x,progtotal3$CumGPA.y.y)
#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(progtotal3))
progtotal3 <- progtotal3[-c(2:13)]

progfinal1 <- full_join(progtotal1, progtotal2, by = 'StudentID')
#update the missing values
progfinal1$CompleteDevMath.y.y.y <- ifelse(is.na(progfinal1$CompleteDevMath.y.y.y),progfinal1$CompleteDevMath.y.y.x,progfinal1$CompleteDevMath.y.y.y)
progfinal1$CompleteDevEnglish.y.y.y <- ifelse(is.na(progfinal1$CompleteDevEnglish.y.y.y),progfinal1$CompleteDevEnglish.y.y.x,progfinal1$CompleteDevEnglish.y.y.y)
progfinal1$Major1.y.y.y <- ifelse(is.na(progfinal1$Major1.y.y.y),progfinal1$Major1.y.y.x,progfinal1$Major1.y.y.y)
progfinal1$Major2.y.y.y <- ifelse(is.na(progfinal1$Major2.y.y.y),progfinal1$Major2.y.y.x,progfinal1$Major2.y.y.y)
progfinal1$Complete1.y.y.y <- ifelse(is.na(progfinal1$Complete1.y.y.y),progfinal1$Complete1.y.y.x,progfinal1$Complete1.y.y.y)
progfinal1$Complete2.y.y.y <- ifelse(is.na(progfinal1$Complete2.y.y.y),progfinal1$Complete2.y.y.x,progfinal1$Complete2.y.y.y)
progfinal1$CompleteCIP1.y.y.y <- ifelse(is.na(progfinal1$CompleteCIP1.y.y.y),progfinal1$CompleteCIP1.y.y.x, progfinal1$CompleteCIP1.y.y.y)
progfinal1$CompleteCIP2.y.y.y <- ifelse(is.na(progfinal1$CompleteCIP2.y.y.y),progfinal1$CompleteCIP2.y.y.x, progfinal1$CompleteCIP2.y.y.y)
progfinal1$TransferIntent.y.y.y <- ifelse(is.na(progfinal1$TransferIntent.y.y.y),progfinal1$TransferIntent.y.y.x,progfinal1$TransferIntent.y.y.y)
progfinal1$DegreeTypeSought.y.y.y <- ifelse(is.na(progfinal1$DegreeTypeSought.y.y.y),progfinal1$DegreeTypeSought.y.y.x,progfinal1$DegreeTypeSought.y.y.y)
progfinal1$TermGPA.y.y.y <- ifelse(is.na(progfinal1$TermGPA.y.y.y),progfinal1$TermGPA.y.y.x,progfinal1$TermGPA.y.y.y)
progfinal1$CumGPA.y.y.y <- ifelse(is.na(progfinal1$CumGPA.y.y.y),progfinal1$CumGPA.y.y.x,progfinal1$CumGPA.y.y.y)
#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(progfinal1))
progfinal1 <- progfinal1[-c(2:13)]

progfinal <- full_join(progfinal1, progtotal3, by = 'StudentID')
#update the missing values
progfinal$CompleteDevMath.y.y <- ifelse(is.na(progfinal$CompleteDevMath.y.y),progfinal$CompleteDevMath.y.y.y,progfinal$CompleteDevMath.y.y)
progfinal$CompleteDevEnglish.y.y <- ifelse(is.na(progfinal$CompleteDevEnglish.y.y),progfinal$CompleteDevEnglish.y.y.y,progfinal$CompleteDevEnglish.y.y)
progfinal$Major1.y.y <- ifelse(is.na(progfinal$Major1.y.y),progfinal1$Major1.y.y.y,progfinal1$Major1.y.y)
progfinal$Major2.y.y <- ifelse(is.na(progfinal$Major2.y.y),progfinal$Major2.y.y.y,progfinal1$Major2.y.y)
progfinal$Complete1.y.y <- ifelse(is.na(progfinal$Complete1.y.y),progfinal$Complete1.y.y.y,progfinal$Complete1.y.y)
progfinal$Complete2.y.y <- ifelse(is.na(progfinal$Complete2.y.y),progfinal$Complete2.y.y.y,progfinal$Complete2.y.y)
progfinal$CompleteCIP1.y.y <- ifelse(is.na(progfinal$CompleteCIP1.y.y),progfinal$CompleteCIP1.y.y.y, progfinal$CompleteCIP1.y.y)
progfinal$CompleteCIP2.y.y <- ifelse(is.na(progfinal$CompleteCIP2.y.y),progfinal$CompleteCIP2.y.y.y, progfinal$CompleteCIP2.y.y)
progfinal$TransferIntent.y.y <- ifelse(is.na(progfinal$TransferIntent.y.y),progfinal$TransferIntent.y.y.y,progfinal$TransferIntent.y.y)
progfinal$DegreeTypeSought.y.y <- ifelse(is.na(progfinal$DegreeTypeSought.y.y),progfinal$DegreeTypeSought.y.y.y,progfinal$DegreeTypeSought.y.y)
progfinal$TermGPA.y.y <- ifelse(is.na(progfinal$TermGPA.y.y),progfinal$TermGPA.y.y.y,progfinal$TermGPA.y.y)
progfinal$CumGPA.y.y <- ifelse(is.na(progfinal$CumGPA.y.y),progfinal$CumGPA.y.y.y,progfinal$CumGPA.y.y)
#drop useless columns, mainly ones with repeating values or missing values.
as.data.frame(colnames(progfinal))
#producted one final dataset, with all the columns combined and feature engineering done.
#all finalized datables have the keywork final in the data frame.
progressfinal <- progfinal[-c(2:13)]


#step one, load all static data provided.

#static data by fall semester
Fallstat11 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Fall 2011_ST.csv")
Fallstat12 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Fall 2012.csv")
Fallstat13 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Fall 2013.csv")
Fallstat14 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Fall 2014.csv")
Fallstat15 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Fall 2015.csv")
Fallstat16 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Fall 2016.csv")

#static data by spring semester
Springstat12 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Spring 2012_ST.csv")
Springstat13 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Spring 2013.csv")
Springstat14 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Spring 2014.csv")
Springstat15 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Spring 2015.csv")
Springstat16 <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/Midterm/Midterm_data/Student Retention Challenge Data/Student Static Data/Spring 2016.csv")

#static data by year
#using rowbind keeps the same 13k number of rows while joining all the columns on one another.

statcomp <- rbind(Fallstat11,Fallstat12,Fallstat13,Fallstat14,Fallstat15,Fallstat16,Springstat12,Springstat13,Springstat14,Springstat15,Springstat16)
#now we combine all the race cloumns into one, reducing the columns of our master static dataset
#by 6 variables. 
statcomp$Race[statcomp$Hispanic == "1"]<-"Hispanic"
statcomp$Race[statcomp$AmericanIndian == "1"]<-"AmericanIndian"
statcomp$Race[statcomp$Asian == "1"]<-"Asian"
statcomp$Race[statcomp$Black == "1"]<-"Black"
statcomp$Race[statcomp$NativeHawaiian == "1"]<-"NativeHawaiian"
statcomp$Race[statcomp$White == "1"]<-"White"
statcomp$Race[statcomp$TwoOrMoreRace=="1"]<-"MixRace"
as.data.frame(colnames(statcomp))
#complete stat data highlighted by the word final. 
#we proceed to drop the race columns as well as the address 2, and some other columns that provide 
#us no information.
staticfinal <-statcomp[-c(2,3,4,6,10,14,15,16,17,18,19,20)]



#student financial data
#basic strategy is to combing all the student finanal aid info from 5 years to one. it should 
#reduce our columns by 15 columns. 
studfindata <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/midterm/Midterm_data/Student Retention Challenge Data/Student Financial Aid Data/2011-2017_Cohorts_Financial_Aid_and_Fafsa_Data.csv")
#change the student id column, to proper name. StudentID is the key word used to combine all the tables
names(studfindata)[1]<-paste("StudentID")

#must change all numeric values that are missing to 0 so that we can concatenate all the student
#financial aid columns.
studfindata$X2012.Scholarship[is.na(studfindata$X2012.Scholarship)] <- 0
studfindata$X2013.Scholarship[is.na(studfindata$X2013.Scholarship)] <- 0
studfindata$X2014.Scholarship[is.na(studfindata$X2014.Scholarship)] <- 0
studfindata$X2015.Scholarship[is.na(studfindata$X2015.Scholarship)] <- 0
studfindata$X2016.Scholarship[is.na(studfindata$X2016.Scholarship)] <- 0
studfindata$X2017.Scholarship[is.na(studfindata$X2017.Scholarship)] <- 0

studfindata$X2012.Grant[is.na(studfindata$X2012.Grant)] <- 0
studfindata$X2013.Grant[is.na(studfindata$X2013.Grant)] <- 0
studfindata$X2014.Grant[is.na(studfindata$X2014.Grant)] <- 0
studfindata$X2015.Grant[is.na(studfindata$X2015.Grant)] <- 0
studfindata$X2016.Grant[is.na(studfindata$X2016.Grant)] <- 0
studfindata$X2017.Grant[is.na(studfindata$X2017.Grant)] <- 0

studfindata$X2012.Loan[is.na(studfindata$X2012.Loan)] <- 0
studfindata$X2013.Loan[is.na(studfindata$X2013.Loan)] <- 0
studfindata$X2014.Loan[is.na(studfindata$X2014.Loan)] <- 0
studfindata$X2015.Loan[is.na(studfindata$X2015.Loan)] <- 0
studfindata$X2016.Loan[is.na(studfindata$X2016.Loan)] <- 0
studfindata$X2017.Loan[is.na(studfindata$X2017.Loan)] <- 0

#we segregate the financial aid by scholarship, loans, and grants. 

#add the respective columns
studfindata$totalScholarship <- studfindata$X2012.Scholarship+
  studfindata$X2013.Scholarship+
  studfindata$X2014.Scholarship+
  studfindata$X2015.Scholarship+
  studfindata$X2016.Scholarship+
  studfindata$X2017.Scholarship



studfindata$totalGrant <- studfindata$X2012.Grant+
  studfindata$X2013.Grant+
  studfindata$X2014.Grant+
  studfindata$X2015.Grant+
  studfindata$X2016.Grant+
  studfindata$X2017.Grant


studfindata$totalLoan <- studfindata$X2012.Loan+
  studfindata$X2013.Loan+
  studfindata$X2014.Loan+
  studfindata$X2015.Loan+
  studfindata$X2016.Loan+
  studfindata$X2017.Loan


colnames(studfindata)
sum(is.na(studfindata$X2012.Work.Study))
#work study columns are mostly missing and do not provide much insight into the data. 
#sum.is.na function helps us see how many missing values per column

#master financial data set is labeled with key work final
financialfinal <- studfindata[-c(2,3,10:33)]

#final merging of all the data
Studentdata1 <- full_join(financialfinal, staticfinal,by = 'StudentID')

Studentdata <- full_join(Studentdata1, progressfinal, by = 'StudentID')


#Our master dataset, with all the static, financial, and progress data for all 13 + thousand student
#is written onto an excel file. This makes further computation onto the data easier and cleaner.
write.csv(Studentdata, file = "C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/midterm/Studentdata.csv",row.names=F)

#My project was split into two different parts. The first part was to combine the massive amounts of data and condense and consolidate the columns. 
#In addition to that, I also wanted to eliminate the na values but by consolidation not by dropping values. 
#The goal was to create a master dataset and write it onto a separate excel sheet. I did not use any dropping methods what so ever in the first part. 
#The only columns that were dropped were completely blank columns. Feature engineering was done with the intend of combine, not deriving. 
#For example combining all the race columns into one or combining all the scholarship, loan and grants column into one. My final dataset had 45 columns from 598. And it still had every student present. 
#Again the updating of the continuous variables provided quite a challenge for me. 


#load master dataset
studentdata <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/midterm/Studentdata.csv")
as.data.frame(colnames(studentdata))
#further checking and dropping of columns
#any non categorial variables that have repeating values are dropped
colnames(studentdata)
#stuent address is dropped because it does not help us predict the outcome of dropping out 
#or staying in school
studentdata <- studentdata[-c(11:14)]

as.data.frame(colnames(studentdata))
#table function is great for viewing values in an individual column. 
table(studentdata$HSDip) 
table(studentdata$HSDipYr)
table(studentdata$HSGPAUnwtd) 
table(studentdata$HSGPAWtd)
table(studentdata$EnrollmentStatus)
table(studentdata$NumColCredAttemptTransfer)
table(studentdata$NumColCredAcceptTransfer)
table(studentdata$HighDeg)
table(studentdata$MathPlacement)
table(studentdata$EngPlacement)
table(studentdata$GatewayMathStatus)
table(studentdata$GatewayEnglishStatus)
table(studentdata$CompleteDevMath.y.y)
table(studentdata$CompleteDevEnglish.y.y)
table(studentdata$Major1.y.y)
table(studentdata$Major2.y.y)
table(studentdata$Complete1.y.y)
table(studentdata$CompleteCIP1.y.y)
table(studentdata$TermGPA.y.y)
table(studentdata$CumGPA.y.y)
(studentdata$HSGPAWtd) #drop this one
table(studentdata$FirstGen) #drop this one
table(studentdata$DualHSSummerEnroll) #drop this one
table(studentdata$Complete2.y.y) #drop this one
table(studentdata$CompleteCIP2.y.y)#drop this one
table(studentdata$TransferIntent.y.y) #drop this one
table(studentdata$DegreeTypeSought.y.y) #drop this one
table(studentdata$CumLoanAtEntry) #drop this one
as.data.frame(colnames(studentdata))
studentdata <- studentdata[-c(17,18,19,33,35,36,37,38,39)]
colnames(studentdata)

#change everything to Na values, blank spaces are not detected by na function however cannot
#be run.
studentdata$Marital.Status[studentdata$Marital.Status==""]<-NA
studentdata$Mother.s.Highest.Grade.Level[studentdata$Mother.s.Highest.Grade.Level==""]<-NA
studentdata$Father.s.Highest.Grade.Level[studentdata$Father.s.Highest.Grade.Level==""]<-NA
studentdata$Housing[studentdata$Housing==""]<-NA

#for all non numeric missing values I decided to use kNN, which selected the 6 nearnest neighbors
#and chose the most common value. KNN works much better at randomizing for classical modeling.
studentdata <- kNN(studentdata, variable = 'Marital.Status', k = 6, imp_var = FALSE)
studentdata <- kNN(studentdata, variable = 'Mother.s.Highest.Grade.Level', k = 6, imp_var = FALSE)
studentdata <- kNN(studentdata, variable = 'Father.s.Highest.Grade.Level', k = 6, imp_var = FALSE)
studentdata <- kNN(studentdata, variable = 'Housing', k = 6, imp_var = FALSE)
studentdata <- kNN(studentdata, variable = 'Race', k = 6, imp_var = FALSE)

#for all missing values in the income, gross income and financial aid I choose the mean because 
#the number of entries i was missing wasn't a whole lot so mean made the most sense in order to
#hold on to the randomness of my data while not losing to many entries. 
studentdata$Adjusted.Gross.Income[is.na(studentdata$Adjusted.Gross.Income)] <- mean(studentdata$Adjusted.Gross.Income,na.rm = T)
studentdata$Parent.Adjusted.Gross.Income[is.na(studentdata$Parent.Adjusted.Gross.Income)] <- mean(studentdata$Parent.Adjusted.Gross.Income,na.rm = T)
studentdata$totalLoan[is.na(studentdata$totalLoan)] <- mean(studentdata$totalLoan,na.rm = T)
studentdata$totalGrant[is.na(studentdata$totalGrant)] <- mean(studentdata$totalGrant,na.rm = T)
studentdata$totalScholarship[is.na(studentdata$totalScholarship)] <- mean(studentdata$totalScholarship,na.rm = T)

summary(studentdata)
#I realized after examining the data that I was missing exactly 508 entries in gender, birthyear, birthMonth
#highschool diploma, year, unweighted gpa, enrollmentstatus, college credits transferred, attempted
#credits accepted, type of highschool degree, math placement, english placement, gateway math status
#and english gateway status. compared to 13k enteries, losing 500 would save me much time so i
#decided to use a simple na.omit function to clean the rest of the table. 

studentdata <- na.omit(studentdata)
#ultimately I ended up losing only those 500 entries so it was well worth it. 

summary(studentdata)
#the more data you can consolidate and fit into your model the better the accuracy. so i decided 
#to combine the scholarship, loans, and grants into one column called total student aid. 

studentdata$totalAid <- studentdata$totalGrant + studentdata$totalLoan + studentdata$totalScholarship
colnames(studentdata)

colnames(studentdata)
studentdata <- studentdata[-c(13,8,9,10)]
colnames(studentdata)
#added the train dataset to begin testing. 

dropouttrain <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/midterm/Midterm_data/DropoutTrainLabels.csv")
testID <- read.csv("C:/Users/sandh/OneDrive/Desktop/data science njcu/intro to machine learning/midterm/Midterm_data/Student Retention Challenge Data/Test Data/TestIDs.csv")

#all numeric that are not continous have to change to factor because this is a classification model. 
train <- merge(x=dropouttrain, y=studentdata, by = 'StudentID', all.x = TRUE)
summary(train)
train<-na.omit(train)
train$Dropout <- as.factor(train$Dropout)
train$Dropout <- as.factor(train$Dropout)
train$Gender <- as.factor(train$Gender)
train$HSDip <- as.factor(train$HSDip)
train$EnrollmentStatus <- as.factor(train$EnrollmentStatus)
train$HighDeg <- as.factor(train$HighDeg)
train$MathPlacement <- as.factor(train$MathPlacement)
train$EngPlacement <- as.factor(train$EngPlacement)
train$GatewayMathStatus <- as.factor(train$GatewayMathStatus)
train$GatewayEnglishStatus <- as.factor(train$GatewayEnglishStatus)
train$CompleteDevMath.y.y <- as.factor(train$CompleteDevMath.y.y)
train$CompleteDevEnglish.y.y <- as.factor(train$CompleteDevEnglish.y.y)
train$Complete1.y.y <- as.factor(train$Complete1.y.y)

#as per professor's instructions we create a test column which we will use to test our models 
#accuracy

kaggletest <- merge(x=testID, y=studentdata, by = 'StudentID', all.x = TRUE)
summary(kaggletest)
kaggletest$Gender <- as.factor(kaggletest$Gender)
kaggletest$HSDip <- as.factor(kaggletest$HSDip)
kaggletest$EnrollmentStatus <- as.factor(kaggletest$EnrollmentStatus)
kaggletest$HighDeg <- as.factor(kaggletest$HighDeg)
kaggletest$MathPlacement <- as.factor(kaggletest$MathPlacement)
kaggletest$EngPlacement <- as.factor(kaggletest$EngPlacement)
kaggletest$GatewayMathStatus <- as.factor(kaggletest$GatewayMathStatus)
kaggletest$GatewayEnglishStatus <- as.factor(kaggletest$GatewayEnglishStatus)
kaggletest$CompleteDevMath.y.y <- as.factor(kaggletest$CompleteDevMath.y.y)
kaggletest$CompleteDevEnglish.y.y <- as.factor(kaggletest$CompleteDevEnglish.y.y)
kaggletest$Complete1.y.y <- as.factor(kaggletest$Complete1.y.y)
sum(is.na(kaggletest))

#lets try the testing....
#split our train model into train and test model
intrain <- createDataPartition(train$Dropout,p=0.75,list = FALSE)
train1 <- train[intrain,]
test1 <- train[-intrain,]
#10 fold cross validation to make sure our results are randomized and valid
trctrl <- trainControl(method = "cv", number = 10)

#fitting classification tree

tree_fit <- train(Dropout ~ ., data = train1, method = "rpart",
                  trControl=trctrl)
tree_fit$bestTune
tree_fit$finalModel
#Plot complexity parameter tuning runs
plot(tree_fit)
#Plot the tree
fancyRpartPlot(tree_fit$finalModel)
#Predict
predictions <- predict(tree_fit, newdata = test1)
#Performance metrics

confusionMatrix(predictions,test1$Dropout)
#To see the importance of the variables
treeImp <- varImp(tree_fit, scale = TRUE)
treeImp
#imp variables include studentid, cum gpa, term gpa, total aid, highest award in highschook
#birthyear, highschool diploma and gateway english courses required. the other variables have no
#impact on this model and i continue to get above 70 percent accurace. 




#Second method we use to model is random forest
forest_fit <- train(Dropout ~., data = train1, method = "rf",importance = T,
                    trControl=trctrl)
#To see model details. we see that .85 was our highest accuracy. 
forest_fit
#To see the tuned mtry parameter.  Mtry is the number of randomly selected predictors
forest_fit$bestTune
#To see the the % variance explained
forest_fit$finalModel
#Plot complexity parameter tuning runs
plot(forest_fit)



#To see the importance of the variables. it seems that all my variables become a factor in
#random forest. 
forestImp <- varImp(forest_fit)
forestImp

#Bagging
bag_fit <- train(Dropout ~., data = train1, method = "treebag",
                 trControl=trctrl)

#similar accuracy and results as the random forest method. .85 percent accuracy.
bag_fit




#To see the importance of the variables
bagImp <- varImp(bag_fit, scale=TRUE)
bagImp

#fit the SVM model
#svmRadial uses the Radial Kernel.  
modSVMFit <- train(Dropout ~ .,train1="svmRadial",sigma =.2,trControl=trctrl,data=train1)
#See model fit details
modSVMFit$finalModel
#See the tuning parametrs used (cost C, and sigma of the radial kernel function)
modSVMFit$bestTune
#See the results details by each optimization run
modSVMFit$results




#The second part of my project was to actually do feature engineering, further drop unneeded columns and then experiment with modeling. 
#I dropped columns I could not think of using such as address, zip code, birth month and high school weighted gpa. 
#I did feature engineering on all the missing columns such as marital status, race, housing, and parents highest education. 
#I created synthetic data because as we went over it in class, synthetic data is quite handy when making predictions. 
#My basic strategy was to use K nearest neighbor for all the non-numeric variables. 
#Then I preceded to impute all my numeric column via the mean because there was a good bit of data provided in the columns compared to the missing values.
#Overall I used 4 models, random forest, classification tree, bagging and svm. All gave me above 80 accuracy, besides the classification tree which gave me above 70.
#I did want to further experiment with the age column and the income column, but I ran out of time. Overall gpa, financial aid and income played a huge part in all my models. 
#One surprising variable that made a huge impact was the high school award column. But it showed a correlation between successful high school students to college students.       


