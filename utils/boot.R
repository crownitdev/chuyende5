library("dplyr")
library("tidyverse")
region25Data <- read.csv("Data/region25_en.csv")
region41Data <- read.csv("Data/region41_en.csv")
### Starting cleansing ###
region25Data <- region25Data %>% rename(parseDate = parse_date)
data25Clean <- region25Data %>% na.omit()
visual25Data <- data25Clean[1:1000, ]

region41Data <- region41Data %>% rename(parseDate = parse_date)
data41Clean <- region41Data %>% na.omit()
visual41Data <- data41Clean[1:1000, ]


getCompare <- function(visualData, t1, t2) {
  ggplot(visualData, aes_string(x=t1, y=t2)) +
    geom_point(colour = "#0c4c8a") +
    geom_smooth(method = lm, se = FALSE) +
    theme_minimal()
}
getEngine <- function(visualData, powerInput){
  visualData %>% mutate(Color = ifelse(power > powerInput, "True", "False")) %>%
    ggplot(aes(x = price, y= power, color = Color))+
    geom_point() +
    stat_ellipse(type="norm") +
    theme_minimal()
}
car25Prices <- visual25Data$price
car25Powers <- visual25Data$power
linear25ModelPricePerPower <- lm(car25Prices ~ car25Powers)

car41Prices <- visual41Data$price
car41Powers <- visual41Data$power
linear41ModelPricePerPower <- lm(car41Prices ~ car41Powers)

plot(car25Powers, car25Prices)
abline(linear25ModelPricePerPower, lwd=3)
plotPredict25 <- recordPlot()

plot(car41Powers, car41Prices)
abline(linear41ModelPricePerPower, lwd=3)
plotPredict41 <- recordPlot()

predictData25 <- function(powerInput) {
  confidence <- predict(linear25ModelPricePerPower, data.frame(car25Powers=c(powerInput)), interval="confidence")
  prediction <- predict(linear25ModelPricePerPower, data.frame(car25Powers=c(powerInput)), interval="prediction")
  c(
    confidence[1],
    prediction[1]
  )
}

predictData41 <- function(powerInput) {
  confidence <- predict(linear41ModelPricePerPower, data.frame(car41Powers=c(powerInput)), interval="confidence")
  prediction <- predict(linear41ModelPricePerPower, data.frame(car41Powers=c(powerInput)), interval="prediction")
  c(
    confidence[1],
    prediction[1]
  )
}
