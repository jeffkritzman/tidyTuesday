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
ratingsDF$intgross_2013_num = as.numeric(ratingsDF$intgross_2013)
#be careful of unfair comparisons between movies with dom vs intl vs both
ratingsDF$gross2013 = ratingsDF$domgross_2013_num + ratingsDF$intgross_2013_num
ratingsDF$profit2013 = ratingsDF$gross2013 - ratingsDF$budget_2013

#join
comboFull <- full_join(ratings, bechdel, by = "imdb_id")
comboInner <- inner_join(ratings, bechdel, by = "imdb_id")

#summary stats
g_bechdel <- bechdelDF %>% count(year, rating) #number of movies per year per rating
g_bechdel[1:10,]

#small mult, # per year, by rating
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
#rated #i.e. PG13
#metascore
#imdb_rating
#imdb_votes
#intgross_2013_num
#domgross_2013_num
#gross2013
#profit2013
#rating #bechdel score
#language 
#country

# try some regression!

# comparing budget to bechdel

# block 1, control variables, what are you controlling for?
m1a <- lm(budget_2013 ~ 1, data=comboInner) #to obtain total SS 
# block 2, impact of year
m1b <- lm(budget_2013 ~ year.x, data=comboInner) #compare to year
# block 3, impact of bechdel score
m1c <- lm(budget_2013 ~ year.x + rating, data=comboInner) #year + Bechdel score
summary(m1c)
#would be nice to scale this down a little

max(comboInner$budget_2013)
min(comboInner$budget_2013)

dataUS <- comboInner[comboInner[, "country"] == "USA", ]
dataUS <- dataUS[complete.cases(dataUS[ , "budget_2013"]), ]
max(dataUS$budget_2013)
min(dataUS$budget_2013)

ggplot(data = dataUS, aes(x=budget_2013, y=budget_2013, fill=rating)) +
  geom_point()

#scale down millions
dataUS$budget_millions <- dataUS$budget_2013 / 1000000

m1a2 <- lm(budget_millions ~ 1, data=dataUS) #to obtain total SS 
m1b2 <- lm(budget_millions ~ year.x, data=dataUS) #compare to year
m1c2 <- lm(budget_millions ~ year.x + rating, data=dataUS) #year + Bechdel score
summary(m1c2) 
# $.67M budget increase per year
# $7.1M budget DECREASE per additional point on the bechdel score!
# both significant to p < 0.001


# hierarchical regression
# https://data.library.virginia.edu/hierarchical-linear-regression/

anova(m1a2)  ##Total SS = 2,915,906
anova(m1a2, m1b2, m1c2) 
# explanation of SS due to year: 32,447 
# additional explanation of SS due to bechdel score: 53,678  
# both significant to p < 0.001
