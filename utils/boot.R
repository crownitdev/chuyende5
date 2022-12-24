library("dplyr")
library("tidyverse")
data <- read.csv("Data/region25_en.csv")
### Starting cleansing ###
data <- data %>% rename(parseDate = parse_date)
dataClean <- data %>% na.omit()
visualData <- dataClean[1:1000, ]

getCompare <- function(t1, t2) {
  ggplot(visualData, aes_string(x=t1, y=t2)) +
    geom_point(colour = "#0c4c8a") +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}
getEngine <- function(powerInput){
  visualData %>% mutate(Color = ifelse(power > powerInput, "True", "False")) %>%
    ggplot(aes(x = price, y= power, color = Color))+
    geom_point() +
    stat_ellipse(type="norm") +
    theme_minimal()
}