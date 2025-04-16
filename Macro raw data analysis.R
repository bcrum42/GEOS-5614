##-------------------------------##
##this code exists to help analyze our bug SI data##

library(dplyr)

#read in data 
data <- read.csv("C:/Users/brown/OneDrive/Desktop/BIOGEO/BIOGEO/rawmacro.csv")

#generate data frame with only N15 and sample name 
dataN <- data.frame(data$Sample.Description, data$N15)
colnames(dataN)

#filter data to only have protein 
pdata <- filter(dataN,data.Sample.Description == "protein" )

#range data 
range(pdata$data.N15)
# 8.53 8.90

#standard deviation 
sd(pdata$data.N15)
# 0.1167109

#mean
mean(pdata$data.N15)
# 8.6925

#filter data to only have CH6 

datc6 <- filter(dataN, data.Sample.Description == "CH6 " ) 


#range data 
range(datc6$data.N15)
# -37.60  29.89

#standard deviation 
sd(datc6$data.N15)
# 29.58365

#mean
mean(datc6$data.N15)
# -6.13

#filter data to only have CH6 

datc7 <- filter(dataN, data.Sample.Description == "CH7 " ) 


#range data 
range(datc7$data.N15)
#-33.14  53.66

#standard deviation 
sd(datc7$data.N15)
#] 35.93708

#mean
mean(datc7$data.N15)
# 10.445



#filter usgs 25
datau5 <- filter(dataN, data.Sample.Description == "USGS 25 ")

#range data 
range(datau5$data.N15)
#  -27.75 -27.44


#standard deviation 
sd(datau5$data.N15)
#  0.1345053

#mean
mean(datau5$data.N15)
# -27.6225

#filter usgs 26
datau6 <- filter(dataN, data.Sample.Description == "USGS 26")

#range data 
range(datau6$data.N15)
#   55.51 56.51


#standard deviation 
sd(datau6$data.N15)
#  0.4215349

#mean
mean(datau6$data.N15)
# 56.0975

