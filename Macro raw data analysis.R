##-------------------------------##
##this code exists to help analyze our bug SI data##

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

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

##--------------------begin N regression-----------------------##
regndf <- data.frame(data$Sample.Description, data$N15, data$Sample.Number)

#filter data for Protein reg 
npdf <- filter(regndf,data.Sample.Description == "protein" )
colnames(npdf)

#generate LM for our P data 
nplm <- lm(data.N15 ~ data.Sample.Number, data=npdf)
summary(nplm)

#plot using R 
plot(npdf$data.Sample.Number, npdf$data.N15) +
  abline(nplm, col= 'red')
#plot using GGplot 
ggplot(data=npdf, aes(x=data.Sample.Number, data.N15))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()

#filter data for USGS 25 reg
nU25df<- filter(regndf,data.Sample.Description == "USGS 25 ")
colnames(nU25df)

#lm for our usgs 25 data
nu25lm <- lm(data.N15 ~ data.Sample.Number, data=nU25df )
summary(nu25lm)

#plot using gg plot 
ggplot(data=nU25df, aes(x=data.Sample.Number, data.N15))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()

#filter data for usgs 26 reg 

nU26df<- filter(regndf,data.Sample.Description == "USGS 26")
colnames(nU26df)

#lm for our usgs 25 data
nu26lm <- lm(data.N15 ~ data.Sample.Number, data=nU26df)
summary(nu26lm)

#plot using gg plot 
ggplot(data=nU26df, aes(x=data.Sample.Number, data.N15))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()

##-------------------------end N code-------------------------##

#------------------------begin C code------------------------##

dataC <- data.frame(data$Sample.Description, data$X13C, data$X18O,
                    data$delta18O.w.r.t..SMOW)

#generate df with only protein C data 
dataCP <- filter(dataC, data.Sample.Description == "protein")

#range of C data
range(dataCP$data.X13C)


#sd of c data 
sd(dataCP$data.X13C)
                
#mean of C data
mean(dataCP$data.X13C)

#generate df with only CH6 C13 data 
dataCCH6 <- filter(dataC, data.Sample.Description == "CH6 ")

#range of C data
range(dataCCH6$data.X13C)


#sd of c data 
sd(dataCCH6$data.X13C)

#mean of C data
mean(dataCCH6$data.X13C)

#generate df with only CH7 C13 data 
dataCCH7 <- filter(dataC, data.Sample.Description == "CH7 ")
                 
#range of C data
range(dataCCH7$data.X13C)


#sd of c data 
sd(dataCCH7$data.X13C)

#mean of C data
mean(dataCCH7$data.X13C)


#------------------begin C regression code-------------##
#dataframe for c13 regressions 
regcdf <- data.frame(data$Sample.Description, data$X13C, data$Sample.Number)

#filter data for Protein reg 
cpdf <- filter(regcdf,data.Sample.Description == "protein" )
colnames(cpdf)

#generate LM for our P data 
cplm <- lm(data.X13C ~ data.Sample.Number, data=cpdf)
summary(cplm)

#plot using GGplot 
ggplot(data=cpdf, aes(x=data.Sample.Number, data.X13C))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()

#filter data for cH6 reg 
c6df <- filter(regcdf,data.Sample.Description == "CH6 " )
colnames(cpdf)

#generate LM for our P data 
c6lm <- lm(data.X13C ~ data.Sample.Number, data=c6df)
summary(c6lm)

#plot using GGplot 
ggplot(data=c6df, aes(x=data.Sample.Number, data.X13C))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()

#filter data for cH7 reg 
c7df <- filter(regcdf,data.Sample.Description == "CH7 " )
colnames(c7df)

#generate LM for our P data 
c7lm <- lm(data.X13C ~ data.Sample.Number, data=c7df)
summary(c7lm)

#plot using GGplot 
ggplot(data=c7df, aes(x=data.Sample.Number, data.X13C))+
  stat_poly_line()+
  stat_poly_eq(use_label(c("eq","R2")))+
  geom_point()
