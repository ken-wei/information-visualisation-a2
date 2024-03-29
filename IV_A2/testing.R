library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
install.packages("reshape2")
library(reshape)
library(reshape2)
library(dplyr)


breed_rank_data <- read.csv("./breed_rank.csv")
breed_rank_data <- breed_rank_data[,-c(10:11)]
breeds <- breed_rank_data$Breed
years <- c("2013", "2014", "2015", "2016",
           "2017", "2018", "2019", "2020")
print(breeds)
print(names(breed_rank_data))
names(breed_rank_data) <- c("Breed", "2013", "2014", "2015", "2016",
                            "2017", "2018", "2019", "2020")

print(names(breed_rank_data))
breed_rank_data <- as.data.frame(t(breed_rank_data))
# Update the column names 
colnames(breed_rank_data) <- breeds
# Removes the first row of repeated breed names
breed_rank_data <- breed_rank_data[-1,]
testing <- colnames(breed_rank_data)
print("Hello1")
print(head(breed_rank_data))
breed_rank_data$year <- c("2013",  "2014",  "2015",  "2016",  "2017",  "2018",
                          "2019",  "2020")
print("Hello2")
print(breed_rank_data$year)
breed_rank_data <- as.data.frame(melt(breed_rank_data, id=c("year")))
breed_filter <- c("Retrievers (Labrador)", "French Bulldogs",
                  "German Shepherd Dogs", "Retrievers (Golden)", "Bulldogs")
p <- ggplot(filter(breed_rank_data, breed_rank_data$variable %in% breed_filter), aes(x=year, y=value, group=variable, color=variable)) + 
  geom_line() 
# theme(legend.position = "None")
print(p)
