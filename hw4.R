#Devon Lee
#3/7/2021

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
# hint: consider using a list, and also new vectors for regression variables
versicolor <- iris[iris$Species == "versicolor",]
sepal_length <- versicolor$Sepal.Length
sepal_width <- versicolor$Sepal.Width
petal_length <- versicolor$Petal.Length
petal_width <- versicolor$Petal.Width
var_pairs_list <- list(list(sepal_length, sepal_width), list(petal_length, petal_width), 
          list(sepal_length, petal_length))
for (sublist in var_pairs_list){
  fit <- lm(sublist[[2]]~sublist[[1]], na.action=na.exclude)
  print(summary(fit))
}

#####################################
##### Part 2: data in dplyr     #####
#####################################
#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
iris_plus_height <- left_join(iris, height, by="Species")


#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
sepal_plot <- ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) + 
                geom_point(shape = 1, size = 2.5) +
                scale_x_continuous(breaks = seq(0, 8, by = 0.5))

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
no_lines_sepal <- sepal_plot +
                    theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
final_sepal_plot <- ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width, color=Species, size=Petal.Length)) + 
                      geom_point() +
                      scale_x_continuous(breaks = seq(0, 8, by = 0.5)) +
                      theme_classic() +   #remove grid lines
                      labs(x = "Sepal Length (cm)", y = "Sepal Width (cm)", title = "Iris Sepal Dimensions") +
                      theme(plot.title = element_text(hjust = 0.5)) #center title
  
#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		