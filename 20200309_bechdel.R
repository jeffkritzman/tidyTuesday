library(tidyverse)
library(shiny)
library(shinydashboard)

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
bechdel <- tuesdata$raw_bechdel
ratings <- tuesdata$movies

#investigate data, possible joins
bechdel[1:10, ] #super old movies first
bechdel[8820:8829, ] #more recent
ratings[1:10, ]
ratings[1:5, "imdb"]
bechdel[8820:8824, "imdb_id"] #more recent
bechdel[bechdel[ , "imdb_id"] == '1343727', ]
ratings[ratings[ , "imdb"] == 'tt1343727',  c("year", "imdb", "title") ]

#convert to data frames
ratingsDF <- data.frame(ratings)
bechdelDF <- data.frame(bechdel)

#match ids better
ratingsDF$imdb_id = str_replace(ratingsDF$imdb, 'tt', '')
ratingsDF$domgross_2013_num = as.numeric(ratingsDF$domgross_2013)
#as.numeric(str_replace(ratingsDF$domgross_2013, '#N/A', '0')) #old version
ratingsDF$intgross_2013_num = as.numeric(ratingsDF$intgross_2013)
ratingsDF$gross2013 = ratingsDF$domgross_2013_num + ratingsDF$intgross_2013_num
ratingsDF$profit2013 = ratingsDF$gross2013 - ratingsDF$budget_2013

#join
comboFull <- full_join(ratings, bechdel, by = "imdb_id")
comboInner <- inner_join(ratings, bechdel, by = "imdb_id")

#summary stats
g_bechdel <- bechdelDF %>% count(year, rating) #number of movies per year per rating
g_bechdel[1:10,]

#small mult
plotb1 <- ggplot(data = g_bechdel, aes(x=year, y=n, fill=rating)) +
  geom_col() + 
  facet_grid(cols = vars(rating))
plotb1

#stacked bars
plotb2 <- ggplot(data = g_bechdel, aes(x=year, y=n, fill=rating)) +
  geom_col() 
plotb2

#stacked bars %
plotb3 <- ggplot(data = g_bechdel, aes(x=year, y=n, fill=rating)) +
  geom_col(position = "fill") 
plotb3

####INTERESTING VARIABLES#####
#budget_2013
#rated
#metascore
#imdb_rating
#imdb_votes
#intgross_2013_num
#domgross_2013_num
#gross2013
#profit2013
#rating #bechdel score

#run some regressions

#start by comparing budget to bechdel
m1a <- lm(budget_2013 ~ 1, data=comboInner) #to obtain total SS
m1b <- lm(budget_2013 ~ year.x, data=comboInner) #compare to year
m1c <- lm(budget_2013 ~ year.x + rating, data=comboInner) #year + Bechdel score

anova(m1a) ##Total SS = 5.4076e+18
anova(m1a, m1b, m1c)
anova(m1b, m1c) #https://data.library.virginia.edu/hierarchical-linear-regression/

