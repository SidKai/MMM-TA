getwd()
setwd("C:/Users/siddharth.kaithw/MMX Modelling Project/Data")
getwd()

install.packages("readxl")

library(readxl)

macro <- read_excel("Macro Data.xlsx") #reading data

head(macro)
tail(macro)

summary(macro) #mean and median of oil prices, gasoline prices, Population,
#stringency index , total confirmed cases, new confirmed cases , total ,
#new total deaths , retail and recreation , residential mi , transit mi , parks
# mi reflect small to large diff in mean and median and possibly skewed

nrow(macro) #41 rows

ncol(macro) #23 col

str(macro)
#all the columns are numerical except for date column

#null value treatment

is.null(macro) #false

is.na(macro$total_confirmed_cases) #true
is.na(macro$new_confirmed_cases) #true
is.na(macro$total_deaths) #true
is.na(macro$new_total_deaths) #true

# 1 row with no value / blank found, removing it wont affect our data.

macro1 <- macro


#is.na(macro1$total_confirmed_cases) #no na values left

#Plotting variables

#oil Prices
hist(macro1$Oil_Prices, main = "Summary of Oil Prices", xlab = "Oil Price" )

boxplot(macro1$Oil_Prices , main = "Summary of Oil Prices" , ylab = 'Oil Price')


#Gasoline Prices
hist(macro1$Gasoline_Prices, main = "Summary of Gasoline Prices", xlab = "Gasoline Price" )

boxplot(macro1$Gasoline_Prices , main = "Summary of Gasoline Prices" , ylab = 'Gasoline Price')


#GDP
hist(macro1$GDP, main = "Summary of GDP", xlab = "GDP" )

boxplot(macro1$GDP , main = "Summary of GDP" , ylab = 'GDP')


#Stringency Index
hist(macro1$stringency_index, main = "Summary of Stringency Index", xlab = "Stringency Index" )

boxplot(macro1$stringency_index , main = "Summary of Stringency Index" , ylab = 'Stringency Index')


#Retail and Recreation Mi
hist(macro1$retail_and_recreation_mobility_index, main = "Summary of Retail and Recreation MI", xlab = "Retail and Recreation MI" )

boxplot(macro1$retail_and_recreation_mobility_index , main = "Summary of Summary of Retail and Recreation MI" , ylab = "Summary of Retail and Recreation MI")


#Transit station mi
hist(macro1$transit_stations_mobility_index , main = "Summary of Transit Station MI", xlab = "Transit Station MI" )

boxplot(macro1$transit_stations_mobility_index , main = "Summary of Transit Station MI" , ylab = 'Transit Station MI')


#park mi

hist(macro1$parks_mobility_index, main = "Summary of Parks MI", xlab = "Parks MI" )

boxplot(macro1$Oil_Prices , main = "Summary of Oil Prices" , ylab = 'Oil Price')


#grocery and pharmacy mi
hist(macro1$grocery_and_pharmacy_mobility_index , main = "Summary of Parks Grocery and Pharmacy", xlab = "Grocery and Pharmacy MI" )


#workplaces mi
hist(macro1$workplaces_mobility_index, main = "Summary of Workplaces MI", xlab = "Workplaces MI" )

boxplot(macro1$workplaces_mobility_index , main = "Summary of Workplaces MI" , ylab = 'Workplaces MI')


#total confirmed cases 
plot(density(macro1$total_confirmed_cases, main = "Summary of Total Confirmed Cases", xlab = "Total COnfirmed cases" ))


#total deaths
plot(density(macro1$total_deaths, main = "Summary of Total Deaths", xlab = "Total Deaths" ))


#New Confirmed Cases
plot(density(macro1$new_confirmed_cases))


library(lubridate)
library(ggplot2)


library(dplyr)

p1 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=total_deaths)) +
  geom_line() + 
  xlab("Year")
p1

p2 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=total_confirmed_cases)) +
  geom_line() + 
  xlab("Year")
p2

p3 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=new_confirmed_cases)) +
  geom_line() + 
  xlab("Year")
p3

p4 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=new_total_deaths)) +
  geom_line() + 
  xlab("Year")
p4

p10 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=Population)) +
  geom_line() + 
  xlab("Year")
p10
factor(macro1$Population) #converted to factor

p5 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=CCI)) +
  geom_line() + 
  xlab("Year")
p5

p6 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=stringency_index)) +
  geom_line() + 
  xlab("Year")
p6

p7 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=Oil_Prices)) +
  geom_line() + 
  xlab("Year")
p7

p8 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=Gasoline_Prices)) +
  geom_line() + 
  xlab("Year")
p8

p9 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=Inflation)) +
  geom_line() + 
  xlab("Year")
p9

p11 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=retail_and_recreation_mobility_index)) +
  geom_line() + 
  xlab("Year")
p11

p12 <- ggplot(data = macro1, aes(x= MONTH_YEAR, y=transit_stations_mobility_index)) +
  geom_line() + 
  xlab("Year")
p12

#fill NA values

macro1$total_confirmed_cases[is.na(macro1$total_confirmed_cases)] <- 13447425
macro1$new_confirmed_cases[is.na(macro1$new_confirmed_cases)] <- 33073
macro1$total_deaths[is.na(macro1$total_deaths)] <- 222026
macro1$new_total_deaths[is.na(macro1$new_total_deaths)] <- 405

labels(macro1)

macro1$MONTH <- month(macro1$MONTH_YEAR)


#checking normality distribution of all city temp
#Using Shapiro-Wilk Test

shapiro.test(macro1$Temp_City1) #data is not normally distributed
shapiro.test(macro1$Temp_City2) #data is not normally distributed
shapiro.test(macro1$Temp_City3) #data is not normally distributed
shapiro.test(macro1$Temp_City4) #data is not normally distributed
shapiro.test(macro1$Temp_City5) #data is not normally distributed

#calculating correlation using Spearman

citytemp = c("Temp_City1","Temp_City2","Temp_City3","Temp_City4","Temp_City5")
citytemp = macro1[citytemp]
citytemp

cor(citytemp, method = 'spearman')

#temperature in all the cities throughout the time are highly positively
# correlated so we can take average of all city temperatures

macro1$Average_City_Temp <- rowMeans(citytemp)
head(macro1$Average_City_Temp)

table(macro1$total_confirmed_cases)

#since there are 26 rows with 0 values and 15 rows with distinct numerical values
# i.e. 63% of values in the col are zero

nocovid = macro1[1:26,]
covid = macro1[27:41,]


#checking correaltion using heatmap

cor_vars <- subset(macro1 ,select = -c(MONTH_YEAR:Temp_City5))
head(cor_vars)

cor_values <- round(cor(cor_vars),2)

heatmap(cor_values)

install.packages("reshape2")
library(reshape2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(cor_values)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size =3)


##oil prices vs gasoline prices

#Checking normality of data
#Perform shapiro-wilk test
shapiro.test(macro1$Oil_Prices) #data is not normally distributed

shapiro.test(macro1$Gasoline_Prices) #data is not normally distributed


library(lubridate)
mts <- ts(cbind(macro1$Oil_Prices, macro1$Gasoline_Prices),
          start = decimal_date(macro1$MONTH_YEAR),
          frequency = 10)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Oil vs Gasoline Prices",
     col.main ="darkgreen")

#corealtion coeff using spearman correaltion
cor(macro1$Oil_Prices, macro1$Gasoline_Prices, method = "spearman")
#0.5564



##inflation vs covid cases

#Checking normality of data
#Perform shapiro-wilk test
shapiro.test(macro1$Inflation) #data is not normally distributed

shapiro.test(macro1$total_confirmed_cases) #data is not normally distributed

shapiro.test(macro1$total_deaths) #data is not normally distributed


mts <- ts(cbind(macro1$Inflation, macro1$total_confirmed_cases, macro1$total_deaths),
          start = decimal_date(macro1$MONTH_YEAR),
          frequency = 10)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Inflation vs total confirmed cases and deaths",
     col.main ="darkgreen")

#calculating coeff using Spearman method
cor(macro1$Inflation , macro1$total_confirmed_cases,method = "spearman")
#0.67

cor(macro1$Inflation,macro1$total_deaths,method = "spearman")
#0.67


#checking the data after covid for inflation
shapiro.test(covid$Inflation) #not normally distributed
shapiro.test(covid$total_confirmed_cases) #not normally distributed
shapiro.test(covid$total_deaths) #not normally distributed

cor(covid$Inflation , covid$total_confirmed_cases, method = 'spearman')
#0.48
#positively correlated but not very reliable
cor(covid$Inflation, covid$total_deaths , method= 'spearman')
#0.44
#same case
#not evident



##gdp vs mi

#Checking normality of data
#Perform shapiro-wilk test
shapiro.test(macro1$GDP) #data is not normally distributed

shapiro.test(macro1$retail_and_recreation_mobility_index) #data is not normally distributed

shapiro.test(macro1$grocery_and_pharmacy_mobility_index) #data is not normally distributed

shapiro.test(macro1$residential_mobility_index) #data is not normally distributed

shapiro.test(macro1$transit_stations_mobility_index) #data is not normally distributed

shapiro.test(macro1$parks_mobility_index) #data is not normally distributed

shapiro.test(macro1$workplaces_mobility_index) #data is not normally distributed



mts <- ts(cbind(macro1$GDP, macro1$retail_and_recreation_mobility_index, macro1$residential_mobility_index,macro1$transit_stations_mobility_index, macro1$parks_mobility_index , macro1$workplaces_mobility_index , macro1$grocery_and_pharmacy_mobility_index),
          start = decimal_date(macro1$MONTH_YEAR),
          frequency = 10)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="GDP vs Mobility Indexes",
     col.main ="darkgreen")

mi <- subset(macro1 ,select = c(retail_and_recreation_mobility_index,grocery_and_pharmacy_mobility_index,residential_mobility_index,transit_stations_mobility_index,parks_mobility_index,workplaces_mobility_index))

#calculating correlation using Spearman Correaltion
cor(macro$GDP , mi , method = "spearman")


#calculating with data after covid
shapiro.test(covid$GDP) #not normally distributed
shapiro.test(covid$retail_and_recreation_mobility_index) #not normally distributed
shapiro.test(covid$grocery_and_pharmacy_mobility_index) #not normally distributed
shapiro.test(covid$residential_mobility_index) #data is normally distributed
shapiro.test(covid$transit_stations_mobility_index) #not normally distributed
shapiro.test(covid$parks_mobility_index) #not normally distributed
shapiro.test(covid$workplaces_mobility_index) #data is normally distributed

#calculating correlation with 'after covid' data

cor(covid$GDP , covid$retail_and_recreation_mobility_index, method = 'spearman')
#0.91
cor(covid$GDP , covid$grocery_and_pharmacy_mobility_index, method = 'spearman')
#0.84
cor(covid$GDP , covid$residential_mobility_index, method = 'pearson')
#-0.75
cor(covid$GDP , covid$transit_stations_mobility_index, method = 'spearman')
#0.75
cor(covid$GDP , covid$parks_mobility_index, method ='spearman')
#0.51
cor(covid$GDP , covid$workplaces_mobility_index, method = 'pearson')
#0.70

#almost all the mi except parks mi are highly correlated with gdp after covid



##stringency vs mi and gdp

#Checking normality of data
#Perform shapiro-wilk test
shapiro.test(macro1$stringency_index) #data is not normally distributed


mts <- ts(cbind(macro1$stringency_index, macro1$retail_and_recreation_mobility_index, macro1$residential_mobility_index,macro1$transit_stations_mobility_index, macro1$parks_mobility_index , macro1$workplaces_mobility_index , macro1$grocery_and_pharmacy_mobility_index, macro1$GDP),
          start = decimal_date(macro1$MONTH_YEAR),
          frequency = 10)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Stringency Index vs Mobility Indexes and GDP",
     col.main ="darkgreen")

#3rd series- residential MI


#correlation coeff using Spearman
cor(macro$stringency_index,mi, method = "spearman")
#highly negatively correlated except residential mobility index which is highly positvely correlated


##total confirmed cases and total deaths

mts <- ts(cbind(macro1$total_confirmed_cases , macro1$total_deaths),
          start = decimal_date(macro1$MONTH_YEAR),
          frequency = 10)
# plotting the graph
plot(mts, xlab ="Month Year",
     main ="total confirmed cases and total deaths",
     col.main ="darkgreen")



#calculating correlation using Spearman Correaltion
cor(macro1$total_confirmed_cases , macro1$total_deaths , method = "spearman")
#0.999
#very highly positively correlated so we can substitute one of them



names(macro2)[names(macro2)=="retail_and_recreation_mobility_index"] <- "retail_recreation_mi"
names(macro2)[names(macro2)=="grocery_and_pharmacy_mobility_index"] <- "grocery_and_pharmacy_mi"
names(macro2)[names(macro2)=="residential_mobility_index"] <- "residential_mi"
names(macro2)[names(macro2)=="transit_stations_mobility_index"] <- "transit_stations_mi"
names(macro2)[names(macro2)=="parks_mobility_index"] <- "parks_mi"
names(macro2)[names(macro2)=="workplaces_mobility_index"] <- "workplaces_mi"



neilson <- read_excel("Nielsen _ Shipment Data.xlsx")
head(neilson) 

neilson['Avg_selling_price'] = neilson['B1_NIELSEN_VALUE_SALES'] / neilson['B1_NIELSEN_VOLUME_SALES']

neilson['Avg_selling_price'] = neilson['Avg_selling_price']






macro2 <- merge(x = macro1, y = neilson[,c("Corrected_Volume","MONTH_YEAR","CHANNEL","Avg_selling_price")], by = "MONTH_YEAR")
head(macro2)

install.packages("writexl")
library(writexl)
write_xlsx(macro2,"macro_merged.xlsx")
tempfile(fileext = "C:/Downloads/macro_merged.xlsx")
write_xlsx(d)

install.packages("xlsx")
library(rio)
library(xlsx)

macro3 = subset(macro2, macro2$CHANNEL == "NATIONAL")


#checking normality of corrected volume

#0.03
shapiro.test(macro3$Corrected_Volume) #data is not normally distributed
shapiro.test(macro3$CCI) #data is normally distributed
#0.06
shapiro.test(macro3$Avg_selling_price) #data is not normally distributed
#0.18

macro3 = subset(macro3, select = -c(Temp_City1:Temp_City5) )

mi <- subset(macro1 ,select = c(retail_and_recreation_mobility_index,grocery_and_pharmacy_mobility_index,residential_mobility_index,transit_stations_mobility_index,parks_mobility_index,workplaces_mobility_index))



cor_vars1 <- subset(macro3 ,select = -c(MONTH_YEAR,CHANNEL))

cor_values <- round(cor(cor_vars1),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(cor_values)

head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size =3)

nocovid = macro3[1:26,]
covid = macro3[27:41,]


#CV vs CCI
cor(macro3$Corrected_Volume , macro3$CCI, method = 'pearson')

#0.449
#positve correlation
#more the confidence of customers on purchasing, more we have volume sold

mts <- ts(cbind(macro3$Corrected_Volume, macro3$CCI),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

mts <- ts(cbind(macro3$Corrected_Volume, macro3$CCI),
          start = c(2020,12), end = c(2021,5),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#in 2021, vol sold is dropped despite of increase in cci
#cor coeff after 12/2020
cor(covid$Corrected_Volume[10:15],covid$CCI[10:15])
# -0.84



#CV vs GDP
cor(macro3$Corrected_Volume , macro3$GDP, method = 'spearman')
#-0.706
#high negative correlation

mts <- ts(cbind(macro3$Corrected_Volume, macro3$GDP),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs GDP",
     col.main ="darkgreen")

#as we can see brand has sold more volume in the country regardless of the GDP
#being dropped


mts <- ts(cbind(covid$Corrected_Volume, covid$GDP),
          start = c(2020,4),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs GDP in covid",
     col.main ="darkgreen")

cor(covid$Corrected_Volume, covid$GDP, method = 'spearman')
#0.18
#GDP increased but volume sold also increased

#so we can conclude that vol being sold doesn't have much impact with GDP



#Corrected Volume vs oil vs gasoline

a<-subset(macro3,select = c(Oil_Prices,Gasoline_Prices))
cor(macro3$Corrected_Volume,a ,method= "spearman")
#oil- -0.55
#gasoline- -0.081

mts <- ts(cbind(macro3$Corrected_Volume, macro3$Oil_Prices , macro3$Gasoline_Prices),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Oil vs Gasoline",
     col.main ="darkgreen")

cor(macro3$Oil_Prices, macro3$Gasoline_Prices,method = 'spearman')
#0.55
#correlation is positive but not very high
#but we can say after 2019, high correlation is visible
#checking correlation till mid of 2019

cor(macro3$Oil_Prices[1:17], macro3$Gasoline_Prices[1:17],method = 'spearman')
#-0.22
cor(macro3$Oil_Prices[17:41], macro3$Gasoline_Prices[17:41],method = 'spearman')
#0.87
#we can see till 05/2019, both were negatively correlated
#but after that there is very high correlation
#so we can check by taking mean of both oil and gas

macro3$Oil_Gasoline_Prices <- rowMeans(macro3[,c('Oil_Prices', 'Gasoline_Prices')])

#checking corr with volume
cor(macro3$Corrected_Volume,macro3$Oil_Gasoline_Prices ,method= "spearman")
# -0.55

mts <- ts(cbind(macro3$Corrected_Volume, macro3$Oil_Gasoline_Prices),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Oil Gasoline Prices",
     col.main ="darkgreen")

#Corrected vol vs Stringency index

cor(macro3$Corrected_Volume, macro3$stringency_index,method = 'spearman')
#0.48

mts <- ts(cbind(macro3$Corrected_Volume, macro3$stringency_index),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Stringency Index",
     col.main ="darkgreen")

#till covid, stringency index was zero so checking data after covid

cor(covid$Corrected_Volume, covid$stringency_index,method = 'spearman') #-0.33
shapiro.test(covid$Corrected_Volume) #data is normally distributed

#after covid, stringency index is negatively affecting the volumes sold
#checking after 2020

mts <- ts(cbind(macro3$Corrected_Volume, macro3$stringency_index),
          start = c(2020,8), end=c(2021,5),
          frequency = 9)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Stringency Index",
     col.main ="darkgreen")

#volume sold has decreased in 2021 even after the stringency is constant


#Corrected Volume vs Covid Variables

covid_vars <- subset(macro3 , select = c(total_confirmed_cases,new_confirmed_cases,total_deaths , new_total_deaths))

cor(macro3$Corrected_Volume , covid_vars)
#totalconfirmed- 0.36 , newconfirmed- -0.00, totaldeath- 0.36 , newtotaldeath- 0.23

#new confirmed cases and new total deaths are not highly correlated
#so we can simply eliminate these variables

#also correlation between total confirmed cases and total death is 0.99
#so we can take one of the variable
#in this case we can take total confirmed cases as total deaths is mutually inclusive


#Corrected Volume vs Mobility Indexes

heatmap(cor(mi))
#we can see that variables except residential mobility index are highly positively correlated

#plotting data of corrected vol and all MI after covid as there is no data before covid
mts <- ts(cbind(covid$Corrected_Volume, covid$retail_and_recreation_mobility_index, covid$grocery_and_pharmacy_mobility_index, covid$residential_mobility_index , covid$transit_stations_mobility_index, covid$parks_mobility_index, covid$workplaces_mobility_index),
           end=c(2021,5),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Mobility Indexes",
     col.main ="darkgreen")

#calculating correlation of 'after covid' data with corrected volume

covid_mi <- subset(covid,select = c(retail_and_recreation_mobility_index, grocery_and_pharmacy_mobility_index, residential_mobility_index,transit_stations_mobility_index ,parks_mobility_index, workplaces_mobility_index))

cor(covid$Corrected_Volume , covid_mi)

#retail and recreation mi is positively correlated but after 2020, vol sales has
#decreased even after increased in retail and recreation
#so retail and recreation might not be the correct var for the model

#same case with grocery and pharmacy mi

#same case with residential mobility

#same case with all

#so we can say that none of the MI directly correlate with the 
#potential increase and decrease in volume sold
#since none of the mobility index is major factor responsible, 
#we can take average of all mobility as they have high multicollinearlity within themselves


macro3$Average_mobility_index <- rowMeans(mi)
cor(macro3$Corrected_Volume , macro3$Average_mobility_index, method = "spearman")
# -0.22
# -0.40

#CV vs Inflation

cor(macro3$Corrected_Volume,macro3$Inflation)

mts <- ts(cbind(macro3$Corrected_Volume, macro3$Inflation),
          start=c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Mobility Indexes",
     col.main ="darkgreen")

#inflation has high multicollinearlity with stringency index and total confirmed cases
#so we can eliminate this feature 


#stringency index has high multicol with total confirmed cases, MI, oil gasoline prices
#total confirmed cases also has high multicol with MI

#CCI is a good independent variable (less multicol) also it had good corr till 2020
#so we can consider it in our model

#Checking multicol with new variables

cor_vars2 <- subset(macro3 ,select = -c(MONTH_YEAR,CHANNEL,Oil_Prices, Gasoline_Prices , Inflation, GDP , Population , new_confirmed_cases , total_deaths, new_total_deaths ,
                                        retail_and_recreation_mobility_index , grocery_and_pharmacy_mobility_index,residential_mobility_index , transit_stations_mobility_index , parks_mobility_index , workplaces_mobility_index,MONTH))

cor_values <- round(cor(cor_vars2),2)

   # reduce the size of correlation matrix
melted_corr_mat <- melt(cor_values)

head(melted_corr_mat)

   # plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size =3)


#removing MI and Stringency index as these are highly multicol with other var


#-----------------------------MINI MARKET/SELF SERVICE------------------------------------
macro4 <- macro3


macro4 = subset(macro2, macro2$CHANNEL == "MINIMARKET/SELF SERVICE")
head(macro4)

shapiro.test(macro4$Corrected_Volume) #data is not normally distributed

#REMOVING UNNECESSARY VARIABLES

macro5 <- subset(macro4, select = -c(Temp_City1, Temp_City2, Temp_City3, Temp_City4,Temp_City5,Oil_Prices, Gasoline_Prices , Inflation, GDP , Population , new_confirmed_cases , total_deaths, new_total_deaths ,
                                     retail_and_recreation_mobility_index , grocery_and_pharmacy_mobility_index,residential_mobility_index , transit_stations_mobility_index , parks_mobility_index , workplaces_mobility_index,
                                     MONTH, stringency_index))
head(macro5)


#checking heatmap of CV with all
cor_vars4 <- subset(macro5 ,select = -c(MONTH_YEAR,CHANNEL))

cor_values <- round(cor(cor_vars4),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(cor_values)

head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size =3)




#CV vs CCI

cor(macro5$Corrected_Volume , macro5$CCI, method = 'pearson')
#0.11
#little to no correlation

mts <- ts(cbind(macro5$Corrected_Volume, macro5$CCI),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#checking before covid
nocovid2 = macro5[1:26,]
covid2 = macro5[27:41,]

mts <- ts(cbind(nocovid2$Corrected_Volume, nocovid2$CCI),
          start = c(2018,1), end = c(2020,2),
          frequency = 13)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#checking corr
cor(nocovid2$Corrected_Volume, nocovid2$CCI) #0.205
cor(covid2$Corrected_Volume, covid2$CCI) #-0.187
#so we can see that consumer confidence has little positive impact on volumes sold
# before the covid
#but after covid it shows no correlation and no dependency on CCI


#CI vs total confirmed cases

cor(macro5$Corrected_Volume, macro5$total_confirmed_cases)
#0.39

mts <- ts(cbind(macro5$Corrected_Volume, macro5$total_confirmed_cases),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Total Confimed Cases",
     col.main ="darkgreen")

#checking data after covid

cor(covid2$Corrected_Volume, covid2$total_confirmed_cases) #-0.33


mts <- ts(cbind(covid2$Corrected_Volume, covid2$total_confirmed_cases),
          start = c(2020,3), end=c(2021,5),
          frequency = 15)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Total Confimed Cases",
     col.main ="darkgreen")

#when the cases started increasing, volume sold decreased
#but after that there was sudden increase in volumes sold even after inc in cases
#but in 2021, although the cases decreased the volume also decreased
#means it was not the probable factor for the dec of vol sold

# CV vs Average Temp


cor(macro5$Corrected_Volume, macro5$Average_City_Temp)
#-0.2

mts <- ts(cbind(macro5$Corrected_Volume, macro5$Average_City_Temp),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Average Temp",
     col.main ="darkgreen")

#it has a little correlaion with temp


#cor(macro5$Corrected_Volume, macro4$stringency_index)
#-0.2

#mts <- ts(cbind(macro5$Corrected_Volume, macro5$Average_City_Temp),
#          start = c(2018,1),
#          frequency = 12)

# plotting the graph
#plot(mts, xlab ="Month Year",
#     main ="Corrected volume vs Average Temp",
#     col.main ="darkgreen")


#----------------------------SUPERMARKETS---------------------------------------


macro6 <- macro5


macro6 = subset(macro2, macro2$CHANNEL == "SUPERMARKETS")
head(macro6)

shapiro.test(macro6$Corrected_Volume) #data is not normally distributed

#REMOVING UNNECESSARY VARIABLES

macro7 <- subset(macro6, select = -c(Temp_City1, Temp_City2, Temp_City3, Temp_City4,Temp_City5,Oil_Prices, Gasoline_Prices , Inflation, GDP , Population , new_confirmed_cases , total_deaths, new_total_deaths ,
                                     retail_and_recreation_mobility_index , grocery_and_pharmacy_mobility_index,residential_mobility_index , transit_stations_mobility_index , parks_mobility_index , workplaces_mobility_index,
                                     MONTH, stringency_index))
head(macro7)


#checking heatmap of CV with all
cor_vars4 <- subset(macro7 ,select = -c(MONTH_YEAR,CHANNEL))

cor_values <- round(cor(cor_vars4),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(cor_values)

head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size =3)




#CV vs CCI

cor(macro7$Corrected_Volume , macro7$CCI, method = 'pearson')
#0.49
#in supermarkets, cci is +vely correalted with volumes

mts <- ts(cbind(macro7$Corrected_Volume, macro7$CCI),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#checking before covid
nocovid3 = macro7[1:26,]
covid3 = macro7[27:41,]

mts <- ts(cbind(nocovid3$Corrected_Volume, nocovid3$CCI),
          start = c(2018,1), end = c(2020,2),
          frequency = 13)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#checking corr
cor(nocovid3$Corrected_Volume, nocovid2$CCI) #0.75

#we can see that consumer confidence has highly impacted volumes sold
#before the covid

mts <- ts(cbind(covid3$Corrected_Volume, covid3$CCI),
          start = c(2020,3), end = c(2021,5),
          frequency = 13)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")
cor(covid3$Corrected_Volume, covid2$CCI) #-0.083

#after covid, as the cci decreased, sales also dec but due to some reason,
#volume pushed up again despite of of low cci
#but when cci increased later in 2021, volume sold decreased
#it shows no dependency on cci after covid

#CI vs total confirmed cases

cor(macro7$Corrected_Volume, macro7$total_confirmed_cases)
#0.42

mts <- ts(cbind(macro7$Corrected_Volume, macro7$total_confirmed_cases),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Total Confimed Cases",
     col.main ="darkgreen")

#checking data after covid

cor(covid3$Corrected_Volume, covid3$total_confirmed_cases) #-0.15


mts <- ts(cbind(covid3$Corrected_Volume, covid3$total_confirmed_cases),
          start = c(2020,3), end=c(2021,5),
          frequency = 15)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Total Confimed Cases",
     col.main ="darkgreen")

#vol sales had good growth despite of covid
#but it crashed due to some reason in the last months of 2020
#the increase in vol sold after that could be because of the fall in cases
#but after that despite of low cases, vol sales decreased

# CV vs Average Temp


cor(macro7$Corrected_Volume, macro7$Average_City_Temp)
#-0.12

mts <- ts(cbind(macro7$Corrected_Volume, macro7$Average_City_Temp),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Average Temp",
     col.main ="darkgreen")

#whenever the city temp is at its peak, vol sold have been low in that year

#----------------------------TRADITIONAL TRADE---------------------------------

macro8 <- macro7


macro8 = subset(macro2, macro2$CHANNEL == "TRADITIONAL TRADE")
head(macro8)

shapiro.test(macro8$Corrected_Volume) #data is not normally distributed

#REMOVING UNNECESSARY VARIABLES

macro9 <- subset(macro8, select = -c(Temp_City1, Temp_City2, Temp_City3, Temp_City4,Temp_City5,Oil_Prices, Gasoline_Prices , Inflation, GDP , Population , new_confirmed_cases , total_deaths, new_total_deaths ,
                                     retail_and_recreation_mobility_index , grocery_and_pharmacy_mobility_index,residential_mobility_index , transit_stations_mobility_index , parks_mobility_index , workplaces_mobility_index,
                                     MONTH, stringency_index))
head(macro9)


#checking heatmap of CV with all
cor_vars5 <- subset(macro9 ,select = -c(MONTH_YEAR,CHANNEL))

cor_values <- round(cor(cor_vars4),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(cor_values)

head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size =3)




#CV vs CCI

cor(macro9$Corrected_Volume , macro9$CCI, method = 'pearson')
#0.47
#in traditional trade, cci is +vely correalted with volumes

mts <- ts(cbind(macro9$Corrected_Volume, macro9$CCI),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#checking before covid
nocovid4 = macro7[1:26,]
covid4 = macro7[27:41,]

mts <- ts(cbind(nocovid4$Corrected_Volume, nocovid4$CCI),
          start = c(2018,1), end = c(2020,2),
          frequency = 13)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")

#checking corr
cor(nocovid4$Corrected_Volume, nocovid4$CCI) #0.75

#we can see that consumer confidence has highly impacted volumes sold
#before the covid in traditional trade

mts <- ts(cbind(covid4$Corrected_Volume, covid4$CCI),
          start = c(2020,3), end = c(2021,5),
          frequency = 13)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs CCI",
     col.main ="darkgreen")
cor(covid4$Corrected_Volume, covid4$CCI) #-0.083

#after covid, as the cci decreased, sales also dec but due to some reason,
#volume pushed up again despite of low cci
#but when cci increased later in 2021, volume sold decreased
#it shows no dependency on cci after covid

#CI vs total confirmed cases

cor(macro9$Corrected_Volume, macro9$total_confirmed_cases)
#0.27

mts <- ts(cbind(macro9$Corrected_Volume, macro9$total_confirmed_cases),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Total Confimed Cases",
     col.main ="darkgreen")

#checking data after covid

cor(covid4$Corrected_Volume, covid4$total_confirmed_cases) #-0.15


mts <- ts(cbind(covid4$Corrected_Volume, covid4$total_confirmed_cases),
          start = c(2020,3), end=c(2021,5),
          frequency = 15)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Total Confimed Cases",
     col.main ="darkgreen")

#vol sales had good despite of increase in covid cases in traditional market
#but it crashed due to some reason in the last months of 2020
#vol sold increased after that
#the increase in vol sold after that could be because of the fall in cases
#but after that despite of low cases, vol sales decreased

# CV vs Average Temp


cor(macro9$Corrected_Volume, macro9$Average_City_Temp)
#-0.20

mts <- ts(cbind(macro9$Corrected_Volume, macro9$Average_City_Temp),
          start = c(2018,1),
          frequency = 12)

# plotting the graph
plot(mts, xlab ="Month Year",
     main ="Corrected volume vs Average Temp",
     col.main ="darkgreen")

#whenever the city temp is at its peak, vol sold have been low in that year

