# tidyTuesday 2018 04 02 us avg tuition

#install.packages("usmap")

library(tidyverse)
library(readxl)

setwd("~/R/tidyTuesday")

#read in raw data
data <- read_excel("20180402_us_avg_tuition.xlsx") 
data

#pivot it to be tidy
tidyData <- data %>% 
  pivot_longer(-State, names_to = "years", values_to = "avg_tuition")
tidyData
#vs pivot_wider()

#convert to data frame
tidyDataDF <- data.frame(tidyData)

#but the years ain't great...
tidyDataDF$startYear <- str_sub(tidyDataDF$years, 1, 4)
tidyDataDF$endYear <- str_c("20", str_sub(tidyDataDF$years, 6, 7))
tidyDataDF[1:10, ]

tidyDataDF$startYear <- as.numeric(tidyDataDF$startYear)
tidyDataDF$endYear <- as.numeric(tidyDataDF$endYear)
tidyDataDF[1:20, ]

#initial small multiple
ggplot(data = tidyDataDF, aes(x=startYear, y=avg_tuition)) +
  geom_col() +
  facet_wrap(vars(State)) + 
  scale_x_continuous(breaks = seq(2004, 2016, by = 12)) +
  labs(x = "Year", y = "Avg Tuition",
       title = "Avg Tuition by Year, by State (2004-2016)")
