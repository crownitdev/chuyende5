# install tidyverse, inside it includes dplyr package
#install.packages("tidyverse")
# Alternative, install just dplyr:
#install.packages("dplyr")

library("dplyr")
library("tidyverse")

# get working directory
print(getwd())

data <- read.csv("Data/region25_en.csv")
#print(head(data, n = 10))

# list all column names
print(colnames(data))
#print(names(data)) # same colnames() func

# check this variable is a data frame object
print(is.data.frame(data))
# print number of columns
print(ncol(data))
# print number of rows
print(nrow(data))

# use dim() to find the dimension of the data set
print(dim(data))

# analyzing the data (use summary())
summary(data)

# summary with descriptive statistic by group (use by())
by(data, data$brand, summary)

# for showing a summary table
# using vtable library for make statistical table
library(vtable)
# create a summary table, show it and save on car_marketplace_summary file
#st(dataClean, file="car_marketplace_summary") # or sumtable()
'
N: number of valid cases
Mean: the mean value
Min: the minium value
Max: the maxium value
...
'

# initial look at the data frame
# show column names, their types, with some sample individual data
str(data)
# 1513200 obs (rows) and 17 variables (columns)

# show info of each column
print(str(data$brand))
print(str(data$name))
print(str(data$bodyType))
print(str(data$color))
print(str(data$fuelType))
print(str(data$year))
print(str(data$mileage))
print(str(data$transmission))
print(str(data$power))
print(str(data$price))
print(str(data$vehicleConfiguration))
print(str(data$engineName))
print(str(data$engineDisplacement))
print(str(data$date))
print(str(data$location))
print(str(data$parse_date))
print(str(data$link))

# head first 5 values for each rows
cat("first 5 values of brand:\n", head(data$brand), "\n")
cat("first 5 values of name:\n", head(data$name), "\n")
cat("first 5 values of body type:\n", head(data$bodyType), "\n")
cat("first 5 values of color:\n", head(data$color), "\n")
cat("first 5 values of fuel type:\n", head(data$fuelType), "\n")
cat("first 5 values of year:\n", head(data$year), "\n")
cat("first 5 values of mileage:\n", head(data$mileage), "\n")
cat("first 5 values of transmission:\n", head(data$transmission), "\n")
cat("first 5 values of power:\n", head(data$power), "\n")
cat("first 5 values of price:\n", head(data$price), "\n")
cat("first 5 values of vehicle configuration:\n", head(data$vehicleConfiguration), "\n")
cat("first 5 values of engine name:\n", head(data$engineName), "\n")
cat("first 5 values of engine displacement:\n", head(data$engineDisplacement), "\n")
cat("first 5 values of date:\n", head(data$date), "\n")
cat("first 5 values of location:\n", head(data$location), "\n")
cat("first 5 values of link:\n", head(data$link), "\n")
cat("first 5 values of parse date:\n", head(data$parse_date), "\n")

# show type of each variable
print(sapply(data, typeof))

# show class of each variable
print(sapply(data, class))

# rename column parse_date to parseDate (using %<% operator in dplyr)
data <- data %>% rename(parseDate = parse_date)
print(colnames(data)[17])

# view types of all columns
dataColumnNames <- names(data)
columnLength <- length(dataColumnNames)
#for(columnName in dataColumnNames)
#{
  # also use typeof() instead class()
#  cat(columnName, ": ", typeof(data[]), "\n")
#}

'String inconsistencies: this includes typos, capitalization errors,
misplaced punctuation, or similar character data errors'

# check unique data in each column (using unique())
print(unique(data$brand))
#print(unique(data$name))

# calculate the total missing value for dataset
# is.na(data) return a dataset include boolean value for each cell that
# represent for its cell has value or not
#print(is.na(data))

# sum in this context will calculate the total cell if the cell has
# value is true
print(sum(is.na(data))) # 446341 cells have no value

# missing values rows per variable
#`%++%` = function(value){
#  eval.parent(substitute(value <- value + 1))
#}
countMissingValues <- function(column){
  #cat("Số hàng có giá trị NA của cột là: ", sum(is.na(column)), "\n")
  return(c("NA quantities"= sum(is.na(column))))
}

naQuantities = apply(data, 2, countMissingValues)
print(naQuantities)
# percent missing values per variable (column)
percentMissing <- function(column) {
  return(sum(is.na(column)) / length(column))
}

'apply(x, margin, func)
* x: an array or matrix
* margin: take a value or range between 1 and 2 to define where to 
apply the function
  * margin = 1: the manipulation is performed on rows
  * margin = 2: the manipulation is performed on columns
  * margin = c(1,2): the manipulation is performed on rows and columns
* func: tells which function to aplly'
print(apply(data, 2, function(column)
  sum(is.na(column)) / length(column)))
# the behind will show the percentage missing values per columns


# remove the variable (column)
# remove column link, its not necessary in project
data <- data[, -16] # get all but delete column at index 16
print(colnames(data))

# remove rows which have missing value (remove the observation)
'Remove the observation: delete row with NA value'

# To remove observations with missing value, we can employ dplyr lib again
# identifying the rows with NAs
print(head(rownames(data)[apply(data, 2, anyNA)]))

# removing all obversation with NAs
dataClean <- data %>% na.omit()
cat("length of data set after clean NA: ", nrow(dataClean), "\n")
# percentage missing values per variable after clean
print(apply(dataClean, 2, function(column)
  sum(is.na(column)) / length(column)))
# everything now is 0%

# Maximum & Minimum values for each numeric variables
cat("Mã lực lớn nhất là: ", max(dataClean$power), "\n")
cat("Mã lực thấp nhất là: ", min(dataClean$power), "\n")
cat("Giá xe cao nhất là: ", max(dataClean$price), "\n")
cat("Giá xe thấp nhất là: ", min(dataClean$price), "\n")
cat("Năm sản xuất lâu đời nhất là: ", min(dataClean$year), "\n")
cat("Năm sản xuất mới nhất là: ", max(dataClean$year), "\n")

# dùng hàm range() để xem min-max (độ giao động)
cat("Giao động mã lực của các xe: ", range(dataClean$power), "\n")
cat("Giao động năm sản xuất của các xe: ", range(dataClean$year), "\n")
cat("Giao động giá cả của các xe: ", range(dataClean$price), "\n")

# dùng hàm mean() để tính giá trị trung bình của 1 variable
cat("Giá trung bình của các mẫu xe: ", mean(dataClean$price), "\n")
cat("Mã lực trung bình của các mẫu xe: ", mean(dataClean$power), "\n")
cat("Năm sản xuất trung bình của các mẫu xe: ", mean(dataClean$year), "\n")

# dùng hàm median() để tính giá trị trung vị của 1 variable
cat("Giá trị trung vị về giá của các mẫu xe: ", median(dataClean$price), "\n")
cat("Giá trị trung vị về mã lực của các mẫu xe: ", median(dataClean$power), "\n")
cat("Giá trị trung vị về năm sản xuất của các mẫu xe: ", median(dataClean$year), "\n")

# As the median, the first and third quartiles can be computed thanks to the quantile() function and by setting the second argument to 0.25 or 0.75
# dùng quantile để lấy value ở mốc 25% và 75% 
cat("Mốc 0.25 về giá của các mẫu xe: ", quantile(dataClean$price, 0.25), "\n")
cat("Mốc 0.25 về mã lực của các mẫu xe: ", quantile(dataClean$power, 0.25, na.rm = TRUE), "\n")
cat("Mốc 0.25 về năm sản xuất của các mẫu xe: ", quantile(dataClean$year, 0.25, na.rm = TRUE), "\n")

cat("Mốc 0.75 về giá của các mẫu xe: ", quantile(dataClean$price, 0.75), "\n")
cat("Mốc 0.75 về mã lực của các mẫu xe: ", quantile(dataClean$power, 0.75, na.rm = TRUE), "\n")
cat("Mốc 0.75 về năm sản xuất của các mẫu xe: ", quantile(dataClean$year, 0.75, na.rm = TRUE), "\n")

# Standard deviation and variance
# Độ lệch chuẩn và phương sai (sử dụng sd() và var())
cat("Độ lệch chuẩn giá cả của xe: ", sd(dataClean$price), "\n")
cat("Phương sai giá cả của xe: ", var(dataClean$price), "\n")
cat("Độ lệch chuẩn mã lực cả của xe: ", sd(dataClean$power), "\n")
cat("Phương sai mã lực của xe: ", var(dataClean$power), "\n")

# Tính Mode (yếu vị, giá trị có số lần xuất hiện nhỏ nhất) (2 cách, thường hoặc dùng summary)
print("Mode của giá: ")
tab <- table(dataClean$price) # number of occurrences for each unique value
tab <- sort(tab, decreasing = FALSE) # sort highest to lowest
print(head(tab))

print("Mode của power: ")
tab <- table(dataClean$power) # number of occurrences for each unique value
tab <- sort(tab, decreasing = FALSE) # sort highest to lowest
print(head(tab))

print("Mode của năm sản xuất: ")
tab <- table(dataClean$year) # number of occurrences for each unique value
tab <- sort(tab, decreasing = FALSE) # sort highest to lowest
print(head(tab))

# linear regression analysis
# Correlation coefficient (hệ số tương quan)
# (= Standard deviation / Mean (độ lệch chuẩn / giá trị trung bình) hoặc dùng cor())

# Hệ số tương quan giữa giá xe và sức mạnh (price standard deviation and power mean)
#priceSd <- sd(dataClean$price, na.rm = TRUE) # same as (sum(dataClean$price) / nrow(dataClean))
#powerMean <- mean(dataClean$power) # sum(dataClean$power) / nrow(dataClean$power)
#cat("Độ lệch chuẩn của giá xe: ", priceSd, "\n")
#cat("Trung bình về power: ", powerMean, "\n")
# Pearson correlation between 2 variables: use cor()
cat("Hệ số tương quan giữa giá xe và sức mạnh (cor): ",
    cor(dataClean$price, dataClean$power), "\n")
cat("Cor.test price and power: \n")
cor.test(dataClean$price, dataClean$power)


# Hệ số tương quan giữa sức mạnh và năm sản xuất
cat("hệ số tương quan giữa power và năm sản xuất (cor): ",
    cor(dataClean$power, dataClean$year), "\n")
cat("Cor.test power and year: \n")
cor.test(dataClean$power, dataClean$year)



# Linear regression model (get a line for predicting find value y when have x)
# Method 1: Scattergraph method
# Simple scatter plot 
# import library for visualizations for correlation coefficient
#install.packages("ggplot2")

# use first 500 rows only, many rows will consume long time for making
# visual and ravel
visualData <- dataClean[1:1000, ]

library(ggplot2)
# make a scattergraph for price and price

# Biểu đồ phân tán giữa giá xe và sức mạnh
sgPriceAndPower <- ggplot(visualData, aes(x = price, y = power)) +
  geom_point(colour = "#0c4c8a") +
  geom_smooth(method = lm, se = FALSE) +
  theme_minimal()
print(sgPriceAndPower)

# mật độ tập trung của xe có mốc 200 mã lực đổ lại
getEngine <- function(power){
  sgnPriceAndPower <- ggplot(visualData, aes(x = price, y = power, color = power <= power)) +
    geom_point() +
    stat_ellipse(type="norm") +
    theme_minimal()
  sgnPriceAndPower
}
#print(sgnPriceAndPower)
# partial correlation
# se : logical value. If TRUE, confidence interval is displayed around smooth.


# Biểu đồ phân tán giữa sức mạnh và năm sản xuất
sgYearAndPower <- ggplot(visualData) + 
  aes(x = year, y = power) +
  geom_point(color = "#0c4c8a") +
  geom_smooth(method = lm) +
  theme_minimal()
print(sgYearAndPower)

# Biểu đồ phân tán giữa số tiền mua xe và năm sản xuất
sgYearAndPrice <- ggplot (visualData) +
  aes(x = price, y = year) +
  geom_point(color = "#0c4c8a") +
  geom_smooth(method = lm) +
  theme_minimal()
print(sgYearAndPrice)

# Method 2: Linear Regression using the least squares method
# Tạo mô hình dự đoán số tiền bỏ ra khi mua xe có mã lực cho trước
carPrices <- visualData$price
carPowers <- visualData$power
linearModelPricePerPower <- lm(carPowers ~ carPrices)
print(linearModelPricePerPower$coefficients)
a <- linearModelPricePerPower$coefficients[1]
b <- linearModelPricePerPower$coefficients[2]

# ví dụ: ta cần mua 1 chiếc xe tầm 250 horsepowers thì mất bao nhiêu
# áp dụng công thức y = a + bx (test)
totalPrice <- (a + (b * 250))
print(totalPrice) # có vẻ hơi sai


visualData <- visualData[1:50, ] # lấy 50 rows
# bảng so sánh các lượng mua của các hãng xe theo dòng đời
tblCompareModelYearBiggerThan2000 <- ifelse(visualData$year >= 2000,
                                       ">=2000", "<2000")
table(tblCompareYearBiggerThan2000)
table(visualData$brand, tblCompareYearBiggerThan2000)
mosaicplot(table(visualData$brand, tblCompareYearBiggerThan2000),
           color = TRUE,
           xlab = "Car brand",
           ylab = "Model year")

# So sánh xe có giá trên dưới 200 mã lực kèm giá
tblComparePowerBiggerThan200 <- ifelse(visualData$power >= 200,
                                       ">=200", "<200")
tblComparePriceBiggerThan1000000 <- ifelse(visualData$price >= 1000000,
                                         ">=1000000", "<1000000")
table(tblComparePowerBiggerThan200)
table(tblComparePriceBiggerThan1000000)
table(visualData$price, tblComparePowerBiggerThan200)
mosaicplot(table(tblComparePriceBiggerThan1000000, tblComparePowerBiggerThan200),
           color = TRUE,
           xlab = "Price",
           ylab = "Power")


# remove duplicate data
# group duplicate data in column brand
#duplicated(dataClean$brand) # for identity what rows are duplicate
# to extract duplicate elements (information of duplicate data)
print(head(dataClean[duplicated(dataClean$brand),]))

# or remove all duplicate data in data set
#print(head(dataClean[duplicated(dataClean), ]))
print(nrow(dataClean[duplicated(dataClean), ]))

# remove duplicated elements, use !duplicate
cat("\n length of data set before remove duplicate: ", nrow(dataClean), "\n")
dataClean <- dataClean[!(duplicated(dataClean)), ]
cat("\n length of data set after remove remove duplicate: ", nrow(dataClean), "\n")

print(summary(dataClean))

# Comparing cars are released before 2000s and after 2000s
carsYearGreaterThan2000 <- nrow(dataClean[dataClean$year >= 2000, ]) / nrow(dataClean)
carsYearSmallerThan2000 <- nrow(dataClean[dataClean$year < 2000, ]) / nrow(dataClean)

carYearComparing.result <- c(carsYearSmallerThan2000, carsYearGreaterThan2000)
carYearComparing.names <- c("< 2000", ">= 2000")

png("barplot_compare_ratio_year.png")
barplot(carYearComparing.result, names.arg=carYearComparing.names,
        xlab="Year", ylab="Ratio", main="Comparing car year")
dev.off()

# Comparing cars are gasonline or diesel
quantityCarsAreGasonline <- nrow(dataClean[dataClean$fuelType == unique(dataClean$fuelType)[1], ]) / nrow(dataClean)
quantityCarsAreDiesel <- nrow(dataClean[dataClean$fuelType == unique(dataClean$fuelType)[2], ]) / nrow(dataClean)
carsFuelTypeComparing.ratio <- c(quantityCarsAreGasonline, quantityCarsAreDiesel)
carsFuelTypeComparing.title <- c("Gasonline", "Diesel")
png("barplot_compare_ratio_fuelType.png")
barplot(carsFuelTypeComparing.ratio, names.arg=carsFuelTypeComparing.title, xlab="Fuel type", 
        ylab="Ratio", main="Comparing ratio of fuel type")

dev.off()

# Comparing the released year (dividing to 4 parts, 1971 -> 1981 -> 1991 -> 2001 -> 2015)
quantityCarsReleasedFrom1971 <- nrow(dataClean[((dataClean$year >= 1971) & (dataClean$year < 1981)), ])
quantityCarsReleasedFrom1981 <- nrow(dataClean[((dataClean$year >= 1981) & (dataClean$year < 1991)), ])
quantityCarsReleasedFrom1991 <- nrow(dataClean[((dataClean$year >= 1991) & (dataClean$year < 2001)), ])
quantityCarsReleasedFrom2001 <- nrow(dataClean[((dataClean$year >= 2001) & (dataClean$year < 2015)), ])

quantityCarReleasedYear.result <- c(quantityCarsReleasedFrom1971, quantityCarsReleasedFrom1981, quantityCarsReleasedFrom1991,
                                    quantityCarsReleasedFrom2001)
quantityCarReleasedYear.title <- c(1981, 1991, 2001, 2015)
quantityCarReleasedYear.rowNames <- data.frame("Năm sản xuất", "tỉ lệ %")
png("lineplot_quantity_cars_at_year.png")
plot(quantityCarReleasedYear.title,
    quantityCarReleasedYear.result,
     type="o", 
     col="red", 
     xlab="Year released", 
     ylab="Quantity cars", 
     main="Car released year")

dev.off()

quantityCarReleasedYearTable <- matrix(quantityCarReleasedYear.result, ncol=1, byrow=TRUE)
quantityCarReleasedYearTable <- table(quantityCarReleasedYearTable)
colnames(quantityCarReleasedYearTable) <- quantityCarReleasedYear.title
row.names(quantityCarReleasedYearTable) <- quantityCarReleasedYear.rowNames
