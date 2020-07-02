

library(readr)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)


#Cleaning Expenditure.CSV file

Exp <- read_csv("C:/Masters/#DWBI/DW&BI Project/Tourism in ireland/Eurostat-Employment&/Expenditure for ireland/Expenditure by sex(S)/tour_dem_exsex_1_Data.csv", 
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
Exp <- Exp[-grep("Total",Exp$GENDER,perl=T),]
Exp <- Exp[-grep("Total",Exp$PURPOSE,perl=T),]
Exp <- Exp[-grep("All countries of the world",Exp$REGION,perl=T),]
names(Exp)[6] <- "VALUE(EUROS)"
write.csv(Exp,file='C:/Masters/#DWBIPROJECT/Expenditure.csv', row.names =  FALSE)


#Cleaning Employment.csv file
Emp <- read_csv("C:/Masters/#DWBI/DW&BI Project/Tourism in ireland/Eurostat-Employment&/Employment for ireland/Employed persons and employees by sex and full-timepart-time activity and Nace Rev. 2 activity/Employment.csv", 
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
Emp <- Emp[-grep("Total",Emp$GENDER,perl=T),]
levels(Emp$EMPLOYMENT_SECTOR)[4] <- 'All NACE Activities'

write.csv(Emp, file = 'C:/Masters/#DWBIPROJECT/Rawemp.csv', row.names = FALSE)

#Cleaning Revenue.csv file
#install.packages("readxl")
Rev <- read_excel("C:/Masters/#DWBI/DW&BI Project/Tourism in ireland/Statista - Revenue/Revenue.xlsx",sheet = 2)
#Removing unwanted columns and rows
Rev <- Rev[3:8,1:2]
R <- c("YEAR","REVENUE(EUROS)")
colnames(Rev) <- R
Rev$`REVENUE(EUROS)`<- as.numeric(Rev$`REVENUE(EUROS)`)
Rev$YEAR <- as.factor(Rev$YEAR)
# Converting Revenue column in millions as per the units given in source by * with 1000000
Rev$`REVENUE(EUROS)` <- Rev$`REVENUE(EUROS)` * 1000000
write.csv(Rev, file = 'C:/Masters/#DWBIPROJECT/Revenue.csv', row.names = FALSE)

#TOP5sites - webscrapping texts from News aritcle Independent.ie & failte ireland using R

#Top 5 visited sites in ireland - 2017 - Independent.ie
url <-read_html("https://www.independent.ie/life/travel/travel-news/revealed-irelands-top-20-tourist-attractions-paid-and-free-36986059.html")
Top <- url %>% html_nodes("ol") 
Top <- Top[1]
Top <- html_text(Top)
Places <- c(substr(Top,1, 27),substr(Top,40,65),substr(Top,78,87),substr(Top,100,130),substr(Top,143,181))
Visitors <- c(substr(Top,31,39),substr(Top,69,77),substr(Top,91,99),substr(Top,134,142),substr(Top,185,191))
Year <- c('2017')
Visitors <-  gsub("[^0-9]", "", Visitors)
Visitors <- as.numeric(Visitors)
Top2017 <- data.frame(Places,Visitors,Year)

#Top 5 visited sites in ireland - 2016 - Failteireland
url <-read_html("http://www.failteireland.ie/Utility/News-Library/Ireland%E2%80%99s-Most-Popular-Visitor-Attractions-Reveale.aspx")
Top2016 <- url %>% html_nodes("ul") 
Top2016 <- html_text(Top2016)
Top2016 <- Top2016[5]
Visitors <- c(substr(Top2016,27,35),substr(Top2016,87,95),substr(Top2016,123,131),substr(Top2016,171,179),substr(Top2016,211,217))
Visitors <-  gsub("[^0-9]", "", Visitors) 
Visitors <- as.numeric(Visitors)
Year <- c('2016')
Top2016 <- data.frame(Places,Visitors,Year)

#Top 5 visited sites in ireland - 2015 - Independent.ie
url <-read_html("https://www.independent.ie/life/travel/ireland/irelands-top-visitor-attractions-revealed-and-the-numbers-are-booming-34947131.html")
Top <- url %>% html_nodes("ol") 
Top <- Top[1]
Top <- html_text(Top)
Visitors <- c(substr(Top,31,39),substr(Top,77,85),substr(Top,99,107),substr(Top,142,148),substr(Top,173,179))
Year <- c('2015')
Visitors <-  gsub("[^0-9]", "", Visitors)
Visitors <- as.numeric(Visitors)
Top2015 <- data.frame(Places,Visitors,Year)

#Top 5 visited sites in ireland - 2013 - Independent.ie
url <-read_html("https://www.independent.ie/life/travel/ireland/irelands-top-10-tourist-attractions-revealed--30397521.html")
Top <- url %>% html_nodes(".p402_hide") 
Top <- Top[2]
Top <- html_text(Top)
Top <- str_replace_all(Top, "[^[:alnum:]]", " ") 
Visitors <- c(substr(Top,448,456),substr(Top,533,539),substr(Top,473,481),substr(Top,577,583),substr(Top,611,618))
Visitors <-  gsub("[^0-9]", "", Visitors) 
Visitors <- as.numeric(Visitors)
Year <- c('2013')
Top2013 <- data.frame(Places,Visitors,Year)

#Top 5 visited sites in ireland - 2014 - Independent.ie
url <-read_html("https://www.independent.ie/life/travel/ireland/irelands-top-tourist-attractions-revealed-31370553.html")
Top <- url %>% html_nodes(".p402_hide") 
Top <- Top[2]
Top <- html_text(Top)
Top <- str_replace_all(Top, "[^[:alnum:]]", " ") 
Visitors <- c(substr(Top,570,578),substr(Top,635,643),substr(Top,669,677),substr(Top,724,730),substr(Top,766,772))
Visitors <-  gsub("[^0-9]", "", Visitors) 
Visitors <- as.numeric(Visitors)
Year <- c('2014')
Top2014 <- data.frame(Places,Visitors,Year)


#Top 5 visited sites in ireland - 2012 - Failteireland
url <-read_html("http://www.failteireland.ie/Footer/Media-Centre/Ireland-s-most-popular-tourist-attractions-for-201.aspx")
Top2012 <- url %>% html_nodes("p") 
Top2012 <- html_text(Top2012)
Top2012 <- Top2012[15]
Top2012 <- str_replace_all(Top2012, "[^[:alnum:]]", " ") 
Visitors <- c(substr(Top2012,59,67),substr(Top2012,199,205),substr(Top2012,132,140),substr(Top2012,267,273),substr(Top2012,340,346))
Visitors <-  gsub("[^0-9]", "", Visitors) 
Visitors <- as.numeric(Visitors)
Year <- c('2012')
Top2012 <- data.frame(Places,Visitors,Year)
Top <- rbind(Top2016,Top2017,Top2013,Top2014,Top2012,Top2015)
Top$Places <- str_replace_all(Top$Places, "[[()]]", "")   
Top <- Top %>% separate(Places, c("Attraction", "County"), ",")
Top$County <- as.character(Top$County)
Top$County <- ifelse(is.na(Top$County), 
                     ' Dublin', Top$County)
Top$County <- as.factor(Top$County)
Top$Attraction <- as.factor(Top$Attraction)
summary(Top)
write.csv(Top, file = 'C:/Masters/#DWBIPROJECT/Top5sites.csv')

#GDP & GDP per capita income
GDPCI <- read_excel("C:/Masters/#DWBI/DW&BI Project/Tourism in ireland/Statista - Revenue/GDP per capita income.xlsx",sheet = 2)
GDP <- read_excel("C:/Masters/#DWBI/DW&BI Project/Tourism in ireland/Statista - Revenue/GDP.xlsx",sheet = 2)
#Removing unwanted columns and rows for GDPCI
GDPCI <- GDPCI[3:8,1:2]
colnames(GDPCI) <- c("YEAR","GDP per capita(EUROS)")
GDPCI$`GDP per capita(EUROS)`<- as.numeric(GDPCI$`GDP per capita(EUROS)`)
GDPCI$YEAR <- as.factor(GDPCI$YEAR)
# converting $ to Euros( 1$ = 0.87 Euros as of 08-11-2018)
GDPCI$`GDP per capita(EUROS)` <- GDPCI$`GDP per capita(EUROS)` * 0.87
#Removing unwanted columns and rows for GDP
GDP <- GDP[3:8,1:2]
colnames(GDP) <- c("YEAR","GDP in billion euros")
GDP$`GDP in billion euros` <- as.numeric(GDP$`GDP in billion euros`)
GDP$YEAR <- as.factor(GDP$YEAR)
# converting $ to Euros( 1$ = 0.87 Euros as of 08-11-2018)
GDP$`GDP in billion euros` <- GDP$`GDP in billion euros` * 0.87
TGDP <- cbind.data.frame(GDPCI,GDP$`GDP in billion euros`)
colnames(TGDP) <- c("YEAR","GDP per capita(EUROS)","GDP in billion euros")
write.csv(TGDP, file = 'C:/Masters/#DWBIPROJECT/TotalGDP.csv')

#TRIPS to Ireland - CSO
TRIP <- read_excel("C:/Masters/#DWBI/DW&BI Project/Tourism in ireland/CSO - Trips to ireland/OT2018M01TBL7.xls", sheet = 1)
colnames(TRIP) <- c("AREA OF RESIDENCE","2010","2011","2012","2013","2014","2015","2016","2017")
TRIP$`2010` <- NULL
TRIP$`2011` <- NULL
TRIP <- TRIP[-c(1, 22, 23, 24, 25, 26, 27),  ]
AREA <- TRIP[,1]
YEAR <- c('2012','2013','2014','2015','2016','2017')
value2012 <- TRIP[,2]
value2013 <- TRIP[,3]
value2014 <- TRIP[,4]
value2015 <- TRIP[,5]
value2016 <- TRIP[,6]
value2017 <- TRIP[,7]
TT12 <- data.frame(AREA,'2012',value2012)
colnames(TT12) <- c("AREA OF RESIDENCE","YEAR","TRIPS")
TT13 <- data.frame(AREA,'2013',value2013)
colnames(TT13) <- c("AREA OF RESIDENCE","YEAR","TRIPS")
TT14 <- data.frame(AREA,'2014',value2014)
colnames(TT14) <- c("AREA OF RESIDENCE","YEAR","TRIPS")
TT15 <- data.frame(AREA,'2015',value2015)
colnames(TT15) <- c("AREA OF RESIDENCE","YEAR","TRIPS")
TT16 <- data.frame(AREA,'2016',value2016)
colnames(TT16) <- c("AREA OF RESIDENCE","YEAR","TRIPS")
TT17 <- data.frame(AREA,'2017',value2017)
colnames(TT17) <- c("AREA OF RESIDENCE","YEAR","TRIPS")
TRIP <- rbind(TT12,TT13,TT14,TT15,TT16,TT17)
TRIP$`AREA OF RESIDENCE` <- as.factor(TRIP$`AREA OF RESIDENCE`)
K <- c(1,2,3,7,15)
levels(TRIP$`AREA OF RESIDENCE`) [K] <- c("Africa","Asia","Australia, New Zealand and Other Oceania","Central, South and other America","Other Europe")
write.csv(TRIP, file = 'C:/Masters/#DWBIPROJECT/TRIPS.csv' )
