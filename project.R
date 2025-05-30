install.packages(c("shiny", "treemap", "stringr", "dplyr", "ggplot2", "bslib"))
library(shiny)
library(treemap)
library(stringr)
library(dplyr)
library(ggplot2)
library(bslib)

dataset=read.csv("Pazymejimas.csv", na.strings="")
refined=dataset[,c(5,9,11,12,13,15,17,18)]
refined$gender = as.factor(refined$gender)
refined$country_of_birth = as.factor(refined$country_of_birth)
refined$birth_date = as.Date(refined$birth_date)
refined$license_valid_from = as.Date(refined$license_valid_from)

str(refined)


all_cats_list = str_extract_all(refined$categories, "\\w+")
all_cats_unlist = unlist(all_cats_list)

categories_table = table(all_cats_unlist)
df = data.frame(categories_table)
colnames(df) = c("category", "count")

df$label = paste0(df$category, "\n", round(df$count / 1000), "K")


#plot 1


# plot 2
runApp("Shiny_App")

