install.packages(c("shiny", "treemap", "stringr", "dplyr", "ggplot2", "bslib"))
library(shiny)
library(treemap)
library(stringr)
library(dplyr)
library(ggplot2)
library(bslib)
library(lubridate)

refined = read.csv("dataset.csv", na.strings="")
refined = na.omit(refined)

refined$gender = as.factor(refined$gender)
refined$birth_date = as.Date(refined$birth_date)
refined$license_valid_from = as.Date(refined$license_valid_from)
refined$age_when_got_licence = as.numeric(
  interval(refined$birth_date, refined$license_valid_from) / years(1))
refined = refined[refined$age_when_got_licence >= 18, ] # only "legal licences :)"
str(refined)

# plot 1

age_density_plot <- ggplot(refined, aes(x = refined$age_when_got_licence)) +
    geom_density(fill = "skyblue", alpha = 0.7, color = "blue") +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "lightblue", alpha = 0.5, color = "grey50") +
    labs(
      title = "Driver age distribution in the moment of obtaining the licence",
      subtitle = "Plot and density histogram",
      x = "Age (years)",
      y = "Density"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11)
    ) + 
    scale_x_continuous(breaks = seq(
      floor(min(refined$age_when_got_licence)/10)*10,
      ceiling(max(refined$age_when_got_licence)/10)*10, by = 5))

print(age_density_plot)

# plot 2
all_cats_list = str_extract_all(refined$categories, "\\w+")
all_cats_unlist = unlist(all_cats_list)

categories_table = table(all_cats_unlist)
df = data.frame(categories_table)
colnames(df) = c("category", "count")

df$label = paste0(df$category, "\n", round(df$count / 1000), "K")

runApp("Shiny_App")

