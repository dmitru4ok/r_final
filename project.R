install.packages(c("shiny", "treemap", "stringr", "dplyr", "ggplot2", "bslib"))
library(shiny)
library(treemap)
library(stringr)
library(dplyr)
library(ggplot2)
library(bslib)

setwd("/Users/dmitru4ok/Desktop/R_final")

dataset=read.csv("Pazymejimas.csv", na.strings="")
refined = dataset[,c(5,9,11,12,13,15,17,18)]
refined$gender = as.factor(refined$gender)
refined$country_of_birth = as.factor(refined$country_of_birth)
refined$birth_date = as.Date(refined$birth_date)
refined$license_valid_from = as.Date(refined$license_valid_from)
dr_categories = c("A", "A1", "A2", "AM", "B", "B1", "BE", "C", "C1", "C1E", "CE", "D", "D1", "D1E", "DE", "T")
str(refined)




runApp("Shiny_App")
