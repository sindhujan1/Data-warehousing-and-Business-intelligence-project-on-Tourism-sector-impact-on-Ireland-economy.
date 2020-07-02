#Cleaning Expenditure.CSV file
library(readr)
Exp <- read_csv("E:/#DWBI/DW&BI Project/Tourism in ireland/Eurostat-Employment&/Expenditure for ireland/Expenditure by sex(S)/tour_dem_exsex_1_Data.csv", 
                col_types = cols(`Flag and Footnotes` = col_skip(), 
                                 GEO = col_skip(), Value = col_number()))
ABC <- c("TIME","SEX","PURPOSE","DURATION","PARTNER")
Exp[ABC] <- lapply(Exp[ABC],factor)
cols <- c("YEAR","GENDER","UNIT",'PURPOSE','DURATION','REGION','VALUE IN EUROS')
colnames(Exp) <- cols
#Merging the Units Column with Value in Euros column by multiplying with 1000 units
Exp$`VALUE IN EUROS` <- Exp$`VALUE IN EUROS` * 1000
Exp$UNIT <- NULL
Exp <- Exp[-grep("4 nights or over",Exp$DURATION,perl=T),]
Exp <- Exp[-grep("From 1 to 3 nights",Exp$DURATION,perl=T),]
names(Exp)[6] <- "VALUE(EUROS)"
summary(Exp)
write.csv(Exp,file='E:/DWBIPROJECT/Expenditure.csv', row.names = FALSE)


#Cleaning Employment.csv file
library(readr)
Emp <- read_csv("E:/#DWBI/DW&BI Project/Tourism in ireland/Eurostat-Employment&/Employment for ireland/Employed persons and employees by sex and full-timepart-time activity and Nace Rev. 2 activity/Employment.csv", 
                col_types = cols(`Flag and Footnotes` = col_skip(), 
                                 GEO = col_skip(), Value = col_number()))
FAC <- c("TIME", 'WORKTIME', 'SEX', 'NACE_R2', 'WSTATUS')
Emp[FAC] <- lapply(Emp[FAC], factor)
Emp <- Emp[-grep("Full-time",Emp$WORKTIME,perl=T),]
Emp <- Emp[-grep("No response",Emp$WORKTIME,perl=T),]
Emp <- Emp[-grep("Part-time",Emp$WORKTIME,perl=T),]
Emp <- Emp[-grep("Employees",Emp$WSTATUS,perl=T),]
Emp$UNIT <- NULL
Emp$WORKTIME <- NULL
Emp$WSTATUS <- NULL
Emp$Value <- Emp$Value * 1000
colnames(Emp) <- c("EMPLOYMENT_SECTOR","YEAR","GENDER","EMPLOYED(PERSONs)")
summary(Emp)
write.csv(Emp, file = 'E:/DWBIPROJECT/Employment.csv', row.names = FALSE)

#Cleaning Revenue.csv file
library("readxl")
Rev <- read_excel("E:/#DWBI/DW&BI Project/Tourism in ireland/Statista - Revenue/Revenue.xlsx",sheet = 2)
#Removing unwanted columns and rows
Rev <- Rev[3:8,1:2]
R <- c("YEAR","REVENUE(EUROS)")
colnames(Rev) <- R
Rev$`REVENUE(EUROS)`<- as.numeric(Rev$`REVENUE(EUROS)`)
Rev$YEAR <- as.factor(Rev$YEAR)
# Converting Revenue column in millions as per the units given in source by * with 1000000
Rev$`REVENUE(EUROS)` <- Rev$`REVENUE(EUROS)` * 1000000
summary(Rev)
write.csv(Rev, file = 'E:/DWBIPROJECT/Revenue.csv', row.names = FALSE)

#Webscrapping - GDP - Unstructured
#Source : https://countryeconomy.com/gdp/ireland?year=2013
#Learned the R coding steps from https://www.kdnuggets.com/2018/01/primer-web-scraping-r.html

library(tidyverse)
install.packages("rvest")
install.packages("stringr")
install.packages("rebus")
install.packages("lubricate")
library(rvest)
library(stringr)
library(rebus)
library(lubridate)

url <- 'https://countryeconomy.com/gdp/ireland?year=2012'
Webpage <- read_html(url)
GDP2012 <- html_nodes(Webpage,'.dol , strong')
GDP12 <- html_text(GDP2012)
GDP12
value12 <- GDP12[2]
GDPC12 <- GDP12[5]

url13 <- 'https://countryeconomy.com/gdp/ireland?year=2013'
Webpage13 <- read_html(url13)
GDP2013 <- html_nodes(Webpage13,'.dol , strong')
GDP13 <- html_text(GDP2013)
value13 <- GDP13[2]
GDPC13 <- GDP13[5]

url14 <- 'https://countryeconomy.com/gdp/ireland?year=2014'
Webpage14 <- read_html(url14)
GDP2014 <- html_nodes(Webpage14,'.dol , strong')
GDP14 <- html_text(GDP2014)
value14 <- GDP14[2]
GDPC14 <- GDP14[5]

url15 <- 'https://countryeconomy.com/gdp/ireland?year=2015'
Webpage15 <- read_html(url15)
GDP2015 <- html_nodes(Webpage15,'.dol , strong')
GDP15 <- html_text(GDP2015)
value15 <- GDP15[2]
GDPC15 <- GDP15[5]
url16 <- 'https://countryeconomy.com/gdp/ireland?year=2016'
Webpage16 <- read_html(url16)
GDP2016 <- html_nodes(Webpage16,'.dol , strong')
GDP16 <- html_text(GDP2016)
value16 <- GDP16[2]
GDPC16 <- GDP16[5]

url17 <- 'https://countryeconomy.com/gdp/ireland?year=2017'
Webpage17 <- read_html(url17)
GDP2017<- html_nodes(Webpage17,'.dol , strong')
GDP17 <- html_text(GDP2017)
value17 <- GDP17[2]
GDPC17 <- GDP17[5]

Y = c('2012','2013','2014','2015','2016','2017')
G = c(value12,value13,value14,value15,value16,value17)
C <- c(GDPC12,GDPC13,GDPC14,GDPC15,GDPC16,GDPC17)
GDP <- data.frame( Y, G, C)
colnames(GDP) <- c("YEAR","GDP","GDP per capita income")
GDP$GDP <- str_replace_all(GDP$GDP, "[^[:alnum:]]", "")
GDP$`GDP per capita income` <- str_replace_all(GDP$`GDP per capita income` , "[^[:alnum:]]", "")
GDP$GDP <- as.numeric(GDP$GDP)
GDP$`GDP per capita income` <- as.numeric(GDP$`GDP per capita income`)
GDP$YEAR <- as.factor(GDP$YEAR)
# converting $ to Euros( 1$ = 0.87 Euros as of 08-11-2018)
GDP$GDP <- GDP$GDP * 0.87
GDP$`GDP per capita income` <- GDP$`GDP per capita income` * 0.87
colnames(GDP) <- c("YEAR","GDP(EUROS)","GDP per capita income(EUROS)")
summary(GDP)
write.csv(GDP, file = 'E:/DWBIPROJECT/GDP.csv', row.names = FALSE)

