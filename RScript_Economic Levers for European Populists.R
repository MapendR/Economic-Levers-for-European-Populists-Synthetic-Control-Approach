#### POPULIST LEADERS AND THE ECONOMY ####
#What are potential economic explanations for the comparatively longer persistence of populist leaders in Europe? 

# 1.SETUP ####
#Set working directory
setwd("C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension")

# Deactivate scientific notation
options(scipen=999)

# Library
pacman::p_load("lattice", "ggplot2", "skimr", "caret", "dplyr", "esquisse", "corrplot", 
               "rpart", "rpart.plot", "outliers", "ppcor", "reshape2", "gmodels", "nnet", 
               "lmtest", "plm", "tidyr", "panelr", "tidyverse", "stargazer", "psych", "writexl",
               "car","MSCMT", "RANN", "cmaes", "nloptr", "NMOF", "DEoptim", "GA", "rgenoud", "GenSA",
               "hydroPSO", "pso", "soma", "imputeTS", "mice", "ggcorrplot")



# 2. DATA ####
###### Read Data #####
population_world <- readxl::read_xls("population.wb.xls") # not in stata

# Economic Wellbeing
gdp_world <- readxl::read_xls("gdp.wb.xls") # not in stata
rgdp_world <- readxl::read_xls("rgdp.wb.xls") # not same as in stata
gdp.gr_world <- readxl::read_xls("gdp.gr.wb.xls") # not available in stata

gdppc_world <- readxl::read_xls("gdppc.wb.xls")
rgdppc_world <- readxl::read_xls("rgdppc.wb.xls")
# realconsumption from stata to be added

# Fiscal Policy
inflation_world <- readxl::read_xls("inflation.wb.xls") # same as in stata but *100
debtgdp_world <- readxl::read_xls("debtgdp.wb.xls") # same as in stata but 1- value
#pbalance from stata (not necessarily important)


# Economic Integration
tariffs_world <- readxl::read_xls("tariffs.wb.xls") # same as in stata
# here: Tariff rate, applied, weighted mean, all products (%)
kof_world <- readxl::read_xlsx("kofgi.kof.xlsx") # KOFFiGI needed!
fdi_world <- readxl::read_xls("fdi.wb.xls") # not in stata

# labor share from stata!!
gini_world <- readxl::read_xls("gini.wb.xls") # not same but similar as in stata (~ value + 2)
unemployment_world <- readxl::read_xls("unemployment.wb.xls") #same as in stata 

# institutions from stata
# judicial from stata
# media from stata
# electoral from stata

# currcrisis from stata
# debtcrisis from stata
# bankcrisis from stata
# 
# conflicts from stata
# war from stata

rightpop_world <- readxl::read_xlsx("rightpop.xlsx") # same as in stata
leftpop_world <- readxl::read_xlsx("leftpop.xlsx") # same as in stata


# populist5 from stata
# populist15 from stata
# rightpop5 to be created
# rightpop15 to be created
# leftpop5 to be created
# leftpop15 to be created

stata <- read.csv("stata.csv", sep = ";")


##### Data Preperation ####
#Change label Country Code to code
colnames(population_world)[colnames(population_world)=="Country Code"] <- "code"
colnames(gdp_world)[colnames(gdp_world)=="Country Code"] <- "code"
colnames(gdp.gr_world)[colnames(gdp.gr_world)=="Country Code"] <- "code"
colnames(inflation_world)[colnames(inflation_world)=="Country Code"] <- "code"
colnames(debtgdp_world)[colnames(debtgdp_world)=="Country Code"] <- "code"
colnames(tariffs_world)[colnames(tariffs_world)=="Country Code"] <- "code"
colnames(kof_world)[colnames(kof_world)=="Country Code"] <- "code"
colnames(fdi_world)[colnames(fdi_world)=="Country Code"] <- "code"
colnames(gini_world)[colnames(gini_world)=="Country Code"] <- "code"
colnames(unemployment_world)[colnames(unemployment_world)=="Country Code"] <- "code"



# Creating Sample index
sample_index <- c("Argentina",
                  "Australia", 
                  "Austria", 
                  "Belgium", 
                  "Bolivia",
                  "Brazil",
                  "Bulgaria",
                  "Canada",
                  "Chile",
                  "China",
                  "Colombia",
                  "Croatia",
                  "Cyprus",
"Czechia",
                  "Denmark",
                  "Ecuador",
"Egypt, Arab Rep.",
                  "Estonia",
                  "Finland",
                  "France",
                  "Germany",
                  "Greece",
                  "Hungary",
                  "Iceland",
                  "India",
                  "Indonesia",
                  "Ireland",
                  "Israel",
                  "Italy",
                  "Japan",
                  "Latvia",
                  "Lithuania",
                  "Luxembourg",
                  "Malaysia",
                  "Malta",
                  "Mexico",
                  "Netherlands",
                  "New Zealand",
                  "Norway",
                  "Paraguay",
                  "Peru",
                  "Philippines",
                  "Poland",
                  "Portugal",
                  "Romania",
"Russian Federation",
"Slovak Republic",
                  "Slovenia",
                  "South Africa",
"Korea, Rep.",
                  "Spain",
                  "Sweden",
                  "Switzerland",
                  "Thailand",
"Turkiye",
                  "United Kingdom",
                  "United States",
                  "Uruguay",
"Venezuela, RB")

# Creating Subsets with sample index for all data sets
population <- filter(population_world, Country %in% sample_index) # full data
gdp <- filter(gdp_world, Country %in% sample_index) # Just use gdp data from stata!
rgdp <- filter(rgdp_world, Country %in% sample_index)
gdp.gr <- filter(gdp.gr_world, Country %in% sample_index)
gdppc <- filter(gdppc_world, Country %in% sample_index)
rgdppc <- filter(rgdppc_world, Country %in% sample_index)
inflation <- filter(inflation_world, Country %in% sample_index)
debtgdp <- filter(debtgdp_world, Country %in% sample_index)
tariffs <- filter(tariffs_world, Country %in% sample_index)
kof <- filter(kof_world, Country %in% sample_index)
fdi <- filter(fdi_world, Country %in% sample_index)
gini <- filter(gini_world, Country %in% sample_index)
unemployment <- filter(unemployment_world, Country %in% sample_index)
rightpop <- filter(rightpop_world, Country %in% sample_index)
leftpop <- filter(leftpop_world, Country %in% sample_index)

# Removing data_world data sets (no further use)
rm(unemployment_world)
rm(population_world)
rm(fdi_world)
rm(gdp_world)
rm(rgdp_world)
rm(gdp.gr_world)
rm(gdppc_world)
rm(rgdppc_world)
rm(debtgdp_world)
rm(tariffs_world)
rm(kof_world)
rm(inflation_world)
rm(leftpop_world)
rm(rightpop_world)
rm(gini_world)

?mscmt

# Deleting unnecessary columns & rename
kof <- dplyr::select(kof, Country, code, year, KOFFiGI, KOFTrGI)

colnames(kof)[colnames(kof)=="KOFFiGI"] <- "kofglobal"
colnames(kof)[colnames(kof)=="KOFTrGI"] <- "koftrade"


## HERE ITS INTERESTING TO SEE WHICH VARIABLES ARE COMPLETELY AVAILABLE
# population - full data 
# gdp - use from stata!
# gdp.gr - compute again with fstgdp
# inflation - some missing values starting in 1961
# debtgdp - a lot of NAs, okayisch data from 1990
# tariffs - okayisch from 1988
# kof - good data starting in 1970
# fdi - okayisch starting in 1970
# gini - bad data
# unemployment - good data starting from 1991
# rightpop - full
# leftpop - full


# Reshaping data to long format
population <- population%>% pivot_longer(cols = where(is.numeric),
                                             names_to='year',
                                             values_to='population')

str(gdp)
gdp$"2022" <- as.numeric(gdp$"2022" )
gdp <- gdp%>% pivot_longer(cols = where(is.numeric),
                                             names_to='year',
                                             values_to='gdp')

str(rgdp)
rgdp$"2022" <- as.numeric(rgdp$"2022" )
rgdp <- rgdp%>% pivot_longer(cols = where(is.numeric),
                           names_to='year',
                           values_to='rgdp')

str(gdp.gr)
gdp.gr$"2022" <- as.numeric(gdp.gr$"2022" )
gdp.gr$"1960" <- as.numeric(gdp.gr$"1960" )

gdp.gr <- gdp.gr%>% pivot_longer(cols = where(is.numeric),
                           names_to='year',
                           values_to='gdp.gr')

str(gdppc)
gdppc$"2022" <- as.numeric(gdppc$"2022" )

gdppc <- gdppc%>% pivot_longer(cols = where(is.numeric),
                                 names_to='year',
                                 values_to='gdppc')

str(rgdppc)
rgdppc <- rgdppc%>% pivot_longer(cols = where(is.numeric),
                                 names_to='year',
                                 values_to='rgdppc')



# Change data types for year values to disapear
debtgdp[1,3] <- as.numeric(debtgdp[1,3])
debtgdp[1,4] <- as.numeric(debtgdp[1,4])
debtgdp[1,5] <- as.numeric(debtgdp[1,5])
debtgdp[1,6] <- as.numeric(debtgdp[1,6])
debtgdp[1,7] <- as.numeric(debtgdp[1,7])
debtgdp[1,8] <- as.numeric(debtgdp[1,8])
debtgdp[1,9] <- as.numeric(debtgdp[1,9])
debtgdp[1,10] <- as.numeric(debtgdp[1,10])
debtgdp[1,11] <- as.numeric(debtgdp[1,11])
debtgdp[1,12] <- as.numeric(debtgdp[1,12])
debtgdp[1,13] <- as.numeric(debtgdp[1,13])
debtgdp[1,14] <- as.numeric(debtgdp[1,14])
debtgdp[1,15] <- as.numeric(debtgdp[1,15])
debtgdp[1,16] <- as.numeric(debtgdp[1,16])
debtgdp[1,17] <- as.numeric(debtgdp[1,17])
debtgdp[1,18] <- as.numeric(debtgdp[1,18])
debtgdp[1,19] <- as.numeric(debtgdp[1,19])
debtgdp[1,20] <- as.numeric(debtgdp[1,20])
debtgdp[1,21] <- as.numeric(debtgdp[1,21])
debtgdp[1,22] <- as.numeric(debtgdp[1,22])
debtgdp[1,23] <- as.numeric(debtgdp[1,23])
debtgdp[1,24] <- as.numeric(debtgdp[1,24])
debtgdp[1,25] <- as.numeric(debtgdp[1,25])
debtgdp[1,26] <- as.numeric(debtgdp[1,26])
debtgdp[1,27] <- as.numeric(debtgdp[1,27])
debtgdp[1,28] <- as.numeric(debtgdp[1,28])
debtgdp[1,29] <- as.numeric(debtgdp[1,29])
debtgdp[1,30] <- as.numeric(debtgdp[1,30])
debtgdp[1,31] <- as.numeric(debtgdp[1,31])
debtgdp[1,32] <- as.numeric(debtgdp[1,32])
debtgdp[1,33] <- as.numeric(debtgdp[1,33])
debtgdp$"2022" <- as.numeric(debtgdp$"2022")
str(debtgdp)

debtgdp <- debtgdp%>% pivot_longer(cols = where(is.numeric),
                                 names_to='year',
                                 values_to='debtgdp')

str(tariffs)

# Use for-loop to get job done
cols <- c(3:65)

for (x in cols) {
  tariffs[1,x] <- as.numeric(tariffs[1,x])
}

str(tariffs)

tariffs <- tariffs%>% pivot_longer(cols = where(is.numeric),
                                 names_to='year',
                                 values_to='tariffs')




str(inflation)
inflation <- inflation%>% pivot_longer(cols = where(is.numeric),
                                         names_to='year',
                                         values_to='inflation')


str(fdi)
# Change data types for year values to disapear
fdi[1,3] <- as.numeric(fdi[1,3])
fdi[1,4] <- as.numeric(fdi[1,4])
fdi[1,5] <- as.numeric(fdi[1,5])
fdi[1,6] <- as.numeric(fdi[1,6])
fdi[1,7] <- as.numeric(fdi[1,7])
fdi[1,8] <- as.numeric(fdi[1,8])
fdi[1,9] <- as.numeric(fdi[1,9])
fdi[1,10] <- as.numeric(fdi[1,10])
fdi[1,11] <- as.numeric(fdi[1,11])
fdi[1,12] <- as.numeric(fdi[1,12])
str(fdi)

fdi <- fdi%>% pivot_longer(cols = where(is.numeric),
                                       names_to='year',
                                       values_to='fdi')
str(fdi)


# Change data types for year values to disapear
gini[1,3] <- as.numeric(gini[1,3])
gini[1,4] <- as.numeric(gini[1,4])
gini[1,5] <- as.numeric(gini[1,5])
gini[1,6] <- as.numeric(gini[1,6])
gini[1,7] <- as.numeric(gini[1,7])
gini[1,8] <- as.numeric(gini[1,8])
gini[1,9] <- as.numeric(gini[1,9])
gini[1,10] <- as.numeric(gini[1,10])
str(gini)


gini <- gini%>% pivot_longer(cols = where(is.numeric),
                                       names_to='year',
                                       values_to='gini')

str(unemployment)
unemployment[1,3] <- as.numeric(unemployment[1,3])
unemployment[1,4] <- as.numeric(unemployment[1,4])
unemployment[1,5] <- as.numeric(unemployment[1,5])
unemployment[1,6] <- as.numeric(unemployment[1,6])
unemployment[1,7] <- as.numeric(unemployment[1,7])
unemployment[1,8] <- as.numeric(unemployment[1,8])
unemployment[1,9] <- as.numeric(unemployment[1,9])
unemployment[1,10] <- as.numeric(unemployment[1,10])
unemployment[1,11] <- as.numeric(unemployment[1,11])
unemployment[1,12] <- as.numeric(unemployment[1,12])
unemployment[1,13] <- as.numeric(unemployment[1,13])
unemployment[1,14] <- as.numeric(unemployment[1,14])
unemployment[1,15] <- as.numeric(unemployment[1,15])
unemployment[1,16] <- as.numeric(unemployment[1,16])
unemployment[1,17] <- as.numeric(unemployment[1,17])
unemployment[1,18] <- as.numeric(unemployment[1,18])
unemployment[1,19] <- as.numeric(unemployment[1,19])
unemployment[1,20] <- as.numeric(unemployment[1,20])
unemployment[1,21] <- as.numeric(unemployment[1,21])
unemployment[1,22] <- as.numeric(unemployment[1,22])
unemployment[1,23] <- as.numeric(unemployment[1,23])
unemployment[1,24] <- as.numeric(unemployment[1,24])
unemployment[1,25] <- as.numeric(unemployment[1,25])
unemployment[1,26] <- as.numeric(unemployment[1,26])
unemployment[1,27] <- as.numeric(unemployment[1,27])
unemployment[1,28] <- as.numeric(unemployment[1,28])
unemployment[1,29] <- as.numeric(unemployment[1,29])
unemployment[1,30] <- as.numeric(unemployment[1,30])
unemployment[1,31] <- as.numeric(unemployment[1,31])
unemployment[1,32] <- as.numeric(unemployment[1,32])
unemployment[1,33] <- as.numeric(unemployment[1,33])
str(unemployment)

unemployment <- unemployment%>% pivot_longer(cols = where(is.numeric),
                                       names_to='year',
                                       values_to='unemployment')



rightpop <- rightpop%>% pivot_longer(cols = where(is.numeric),
                                       names_to='year',
                                       values_to='rightpop')

leftpop <- leftpop%>% pivot_longer(cols = where(is.numeric),
                                       names_to='year',
                                       values_to='leftpop')


# Change data types
# population - full data 
# gdp - use from stata!
# gdp.gr - compute again with fstgdp
# inflation - some missing values starting in 1961
# debtgdp - a lot of NAs, okayisch data from 1990
# tariffs - okayisch from 1988
# kof - good data starting in 1970
# fdi - okayisch starting in 1970
# gini - bad data
# unemployment - good data starting from 1991
# rightpop - full
# leftpop - full

variables <- c(population, gdp, gdp.gr, rgdppc, inflation, debtgdp, tariffs, kof, fdi, gini, unemployment, rightpop, leftpop )

str(population)
str(gdp.gr)
str(gdp.gr)
str(rgdppc)
str(inflation)
str(debtgdp)
str(tariffs)
str(kof)
str(fdi)
str(gini)
str(unemployment)
str(rightpop)
str(leftpop)

debtgdp$Country <- as.factor(debtgdp$Country)
debtgdp$code <- as.factor(debtgdp$code)
debtgdp$year <- as.factor(debtgdp$year)

gdp$Country <- as.factor(gdp$Country)
gdp$code <- as.factor(gdp$code)
gdp$year <- as.factor(gdp$year)

rgdp$Country <- as.factor(rgdp$Country)
rgdp$code <- as.factor(rgdp$code)
rgdp$year <- as.factor(rgdp$year)

gdp.gr$Country <- as.factor(gdp.gr$Country)
gdp.gr$code <- as.factor(gdp.gr$code)
gdp.gr$year <- as.factor(gdp.gr$year)

gdppc$Country <- as.factor(gdppc$Country)
gdppc$code <- as.factor(gdppc$code)
gdppc$year <- as.factor(gdppc$year)

rgdppc$Country <- as.factor(rgdppc$Country)
rgdppc$code <- as.factor(rgdppc$code)
rgdppc$year <- as.factor(rgdppc$year)

inflation$Country <- as.factor(inflation$Country)
inflation$code <- as.factor(inflation$code)
inflation$year <- as.factor(inflation$year)

population$population <- as.integer(population$population)
population$Country <- as.factor(population$Country)
population$code <- as.factor(population$code)
population$year <- as.factor(population$year)

tariffs$Country <- as.factor(tariffs$Country)
tariffs$code <- as.factor(tariffs$code)
tariffs$year <- as.factor(tariffs$year)

kof$Country <- as.factor(kof$Country)
kof$code <- as.factor(kof$code)
kof$year <- as.factor(kof$year)

fdi$Country <- as.factor(fdi$Country)
fdi$code <- as.factor(fdi$code)
fdi$year <- as.factor(fdi$year)

gini$Country <- as.factor(gini$Country)
gini$code <- as.factor(gini$code)
gini$year <- as.factor(gini$year)

unemployment$Country <- as.factor(unemployment$Country)
unemployment$code <- as.factor(unemployment$code)
unemployment$year <- as.factor(unemployment$year)


rightpop$rightpop <- as.factor(rightpop$rightpop)
rightpop$Country <- as.factor(rightpop$Country)
rightpop$year <- as.factor(rightpop$year)

leftpop$leftpop <- as.factor(leftpop$leftpop)
leftpop$Country <- as.factor(leftpop$Country)
leftpop$year <- as.factor(leftpop$year)


str(population)
str(gdp)
str(gdp.gr)
str(rgdppc)
str(inflation)
str(debtgdp)
str(tariffs)
str(kof)
str(fdi)
str(gini)
str(unemployment)
str(rightpop)
str(leftpop)




# Create new dummy populist and nonpopulist and compile everythin in populist data set
populist <- merge(rightpop, leftpop, by=c("Country","year"), all=TRUE)
populist$populist <- ifelse(populist$rightpop == 1 | populist$leftpop == 1, 1, 0)
nonpopulist <- merge(rightpop, leftpop, by=c("Country","year"), all=TRUE)
nonpopulist$nonpopulist <- ifelse(nonpopulist$rightpop == 1 | nonpopulist$leftpop == 1, 0, 1)
nonpopulist$rightpop <- NULL
nonpopulist$leftpop <- NULL
populist <- merge(populist, nonpopulist, by=c("Country","year"), all=TRUE)
populist <- populist[, c("Country", "year", "populist", "rightpop", "leftpop", "nonpopulist")]

# Change data types
populist$populist <- as.factor(populist$populist)
populist$nonpopulist <- as.factor(populist$nonpopulist)

str(populist)


# Prepare stata data set
stata$realgrosscapform <- NULL
stata$placebo <- NULL
stata$independent <- NULL
stata$advanced <- NULL
stata$popepc <- NULL
stata$populist <- NULL
stata$leftpop <- NULL
stata$rightpop <- NULL
stata$populist_takeover <- NULL
stata$rightpop_takeover <- NULL
stata$leftpop_takeover <- NULL
stata$populist5 <- NULL
stata$populist15 <- NULL

colnames(stata)[colnames(stata)=="realconsumption"] <- "consumption"

str(stata)

# Chang data types
stata$Country <- as.factor(stata$Country)
stata$code <- as.factor(stata$code)
stata$cid <- as.factor(stata$cid)
stata$year <- as.factor(stata$year)

stata$currcrisis <- as.factor(stata$currcrisis)
stata$debtcrisis <- as.factor(stata$debtcrisis)
stata$bankcrisis <- as.factor(stata$bankcrisis)
stata$currcrisis <- as.factor(stata$currcrisis)
stata$conflicts <- as.factor(stata$conflicts)
stata$war <- as.factor(stata$war)

str(stata)
str(kof)

# Merging data frames to one single data frame

# Merge all World bank data together
data <- merge(population, gdp, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, rgdp, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, gdp.gr, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, gdppc, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, rgdppc, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, inflation, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, tariffs, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, kof, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, fdi, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, gini, by=c("Country","year", "code"), all=TRUE)
data <- merge(data, unemployment, by=c("Country","year", "code"), all=TRUE)

str(data)

# Merge with stata data
str(stata)
data <- dplyr::full_join(data, stata, by=c("Country","year", "code"))

# Define year and Country again as factors to re-write the factors integers in the right order
data$year <- as.character(data$year)
data$year <- as.factor(data$year)

data$Country <- as.character(data$Country)
data$Country <- as.factor(data$Country)

str(data$year)
str(data$Country)


# Declare data to panel data
data <- pdata.frame(data, index = c("Country","year"))



# Manually re-enter populist dummy and merge to data
#write_xlsx(data, path = "C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension/data.xlsx")

# Read data back into R after manually filling in populist dummies
df <- readxl::read_xlsx("data.xlsx")
str(df)

df$Count <- as.integer(df$Count)
df$Country <- as.factor(df$Country)
df$code <- as.factor(df$code)
df$cid <- as.factor(df$cid )
df$year <- as.factor(df$year)
df$currcrisis <- as.factor(df$currcrisis)
df$debtcrisis <- as.factor(df$debtcrisis)
df$bankcrisis <- as.factor(df$bankcrisis)
df$conflicts <- as.integer(df$conflicts)
df$war <- as.factor(df$war)
df$populist <- as.factor(df$populist)
df$rightpop <- as.factor(df$rightpop)
df$leftpop <- as.factor(df$leftpop)
df$populist5 <- as.factor(df$populist)
df$populist15<- as.factor(df$populist15)
df$rightpop5 <- as.factor(df$rightpop5)
df$rightpop15 <- as.factor(df$rightpop15)
df$leftpop5  <- as.factor(df$leftpop5 )
df$leftpop15 <- as.factor(df$leftpop15)
str(df)

# create subset with only data on populist dummies
populist <- dplyr::select(df, Country, year, populist, rightpop, leftpop, populist5, populist15, rightpop5, rightpop15, leftpop5, leftpop15)

# remove old data on populist dummies
data$populist.x <- NULL
data$populist.y <- NULL
data$rightpop.x <- NULL
data$rightpop.y <- NULL
data$leftpop.x <- NULL
data$leftpop.y  <- NULL
data$populist5 <- NULL
data$populist15 <- NULL


# Merge new data on populist dummies to data
data <- dplyr::full_join(data, populist,  by=c("Country","year"))
str(data)

# Declare data frame as panel data
data <- pdata.frame(data, index = c("Country","year"))

# Change decimals and create new variables
data$population.gr <- ((data$population - lag(data$population))/lag(data$population))*100
data$gdp.gr <- ((data$gdp - lag(data$gdp))/lag(data$gdp))*100 # overwriting gdp.gr because gdp has less missing values!
data$rgdp.gr <- ((data$rgdp - lag(data$rgdp))/lag(data$rgdp))*100
data$gdppc.gr <- ((data$gdppc - lag(data$gdppc))/lag(data$gdppc))*100
data$rgdppc.gr <- ((data$rgdppc - lag(data$rgdppc))/lag(data$rgdppc))*100
data$fdi.gr <- round(((data$fdi - lag(data$fdi))/lag(data$fdi))*100)



# Merge/match/combine data and clean up

# inflation
data$inflation.x <- ifelse(is.na(data$inflation.x), data$inflation.y*100, data$inflation.x)
colnames(data)[colnames(data)=="inflation.x"] <- "inflation"
data$inflation.y <- NULL

# tariffs
data$tariffs.y <- ifelse(is.na(data$tariffs.y), data$tariffs.x, data$tariffs.y)
colnames(data)[colnames(data)=="tariffs.y"] <- "tariffs"
data$tariffs.x <- NULL

# kofglobal and koftrade
data$kofglobal.y <- ifelse(is.na(data$kofglobal.y), data$kofglobal.x, data$kofglobal.y)
colnames(data)[colnames(data)=="kofglobal.y"] <- "kofglobal"
data$kofglobal.x <- NULL

data$koftrade.y <- ifelse(is.na(data$koftrade.y), data$koftrade.x, data$koftrade.y)
colnames(data)[colnames(data)=="koftrade.y"] <- "koftrade"
data$koftrade.x <- NULL

# gini
data$gini_diff <- data$gini.x - data$gini.y
summary(data$gini_diff)
# Mean gini.x is 2.171 points larger than gini.y: We impute NAs for gini.y = gini.x - 2.71
data$gini.y <- ifelse(is.na(data$gini.y), data$gini.x-2.171, data$gini.y)
colnames(data)[colnames(data)=="gini.y"] <- "gini"
data$gini.x <- NULL
data$gini_diff <- NULL

# unemployment
data$unemployment_diff <- data$unemployment.x - data$unemployment.y
summary(data$unemployment_diff)
data$unemployment.y <- ifelse(is.na(data$unemployment.y), data$unemployment.x, data$unemployment.y)
colnames(data)[colnames(data)=="unemployment.y"] <- "unemployment"
data$unemployment.x <- NULL
data$unemployment_diff <- NULL


# Select columns and change order of variables
col_order <- c("Count",
               "Country",
               "code", 
               "cid",
               "year",
               "population",
               "population.gr",
               "fstgdp",
               "gdp",
               "gdp.gr",
               "rgdp",
               "rgdp.gr",
               "gdppc",
               "rgdppc",
               "rgdppc.gr",
               "consumption",
               "inflation",
               "debtgdp",
               "pbalance",
               "tariffs",
               "kofglobal",
               "koftrade",
               "tradegdp",
               "fdi",
               "fdi.gr",
               "labor",
               "gini",
               "unemployment",
               "institutions",
               "judicial",
               "media",
               "electoral",
               "currcrisis",
               "debtcrisis",
               "bankcrisis",
               "conflicts",
               "war",
               "populist",
               "rightpop",
               "leftpop",
               "populist5",
               "populist15",
               "rightpop5",
               "rightpop15",
               "leftpop5",
               "leftpop15")

data <- data[, col_order]


##### Sample Data #####

# Defining core set exlcuding WW2-data and 2022-data since, no data for most variables
data$year <- as.character(data$year)
data$year <- as.integer(data$year)
core <- subset(data, year >= "1946" & year <="2021")
str(core)

# Defining data for panel regression
core.plm <- subset(data, year >= "1960" & year <="2021")


#extended data set containing all data
ext <- data
rm(df)



#### 4. PANEL REGRESSIONS ####

######  Balancedness ####
punbalancedness(data)
#   gamma        nu 
# 0.9999936    0.9999937 
# Balanced data set: The closer to 1, the more balanced is the panel data

##### Mulitcollinearity ####
# If vif > 5 (10), multicollinearity of respective expl. variable to high (rule of thumb)
# We do this by using method = pooling i.e. the coefficients remain the same over the time periods (not recognizing the panel structure of data)
p.reg <- plm(gdp.gr ~ populist5
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.plm,
             model = "pooling")

vif(p.reg)
# populist5  inflation currcrisis debtcrisis bankcrisis 
# 1.006599   1.006693   1.001933   1.002855   1.003868 
# No collinearity between explanatory variables!

#I could add a correlation matrix to show that there is no multicollinearity!


##### rGDPpc.gr ~ Populist #####

# Populist
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist5,
                  data = core.plm,
                  model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#    Dependent variable:    
# ________________________________________
#                        rgdppc.gr         
# ________________________________________
#   populist51             -0.350           
# (0.249)          
# 
# ________________________________________
#   Observations            3,053           
# R2                      0.001           
# Adjusted R2            -0.019           
# F Statistic     1.975 (df = 1; 2993)    
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01



# controls
p.reg <- plm(rgdppc.gr ~ populist5
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.plm,
             model = "within")
stargazer(p.reg, type = "text")


#          Dependent variable:    
# ________________________________________
#                        rgdppc.gr         
# ________________________________________
#   populist51             -0.384           
# (0.273)          
# 
# inflation             -0.001***         
#   (0.0002)          
# 
# currcrisis1           -0.666***         
#   (0.253)          
# 
# debtcrisis1           -2.138***         
#   (0.498)          
# 
# bankcrisis1           -2.047***         
#   (0.384)          
# 
# ________________________________________
#   Observations            2,279           
# R2                      0.038           
# Adjusted R2             0.016           
# F Statistic   17.596*** (df = 5; 2227)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  

# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#          Dependent variable:    
# ________________________________________
#                      rgdppc.gr         
# ________________________________________
#   populist151           -0.524**          
#   (0.231)          
# ________________________________________
#   Observations            3,053           
# R2                      0.002           
# Adjusted R2            -0.018           
# F Statistic    5.155** (df = 1; 2993)   
# ________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# controls: 
p.reg <- plm(rgdppc.gr ~ populist15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#           Dependent variable:    
# ________________________________________
#                        rgdppc.gr         
# ________________________________________
#   populist151           -0.525**          
#   (0.244)          
# 
# inflation             -0.001***         
#   (0.0002)          
# 
# currcrisis1           -0.665***         
#   (0.253)          
# 
# debtcrisis1           -2.095***         
#   (0.498)          
# 
# bankcrisis1           -2.053***         
#   (0.383)          
# 
# ________________________________________
# Observations            2,279           
# R2                      0.039           
# Adjusted R2             0.017           
# F Statistic   18.152*** (df = 5; 2227)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01



# Rightpop
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ rightpop5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                       rgdppc.gr         
# ________________________________________
#   rightpop51             -0.172           
# (0.365)          
# 
# ________________________________________
# Observations            3,053           
# R2                     0.0001           
# Adjusted R2            -0.020           
# F Statistic     0.223 (df = 1; 2993)    
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01

  
# controls
p.reg <- plm(rgdppc.gr ~ rightpop5
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                     rgdppc.gr         
# ________________________________________
#   rightpop51             -0.322           
# (0.403)          
# 
# inflation             -0.001***         
#   (0.0002)          
# 
# currcrisis1           -0.677***         
#   (0.253)          
# 
# debtcrisis1           -2.128***         
#   (0.498)          
# 
# bankcrisis1           -2.047***         
#   (0.384)          
# 
# ________________________________________
#   Observations            2,279           
# R2                      0.037           
# Adjusted R2             0.015           
# F Statistic   17.319*** (df = 5; 2227)  
# ________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01




  
# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ rightpop15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                          rgdppc.gr         
# ________________________________________
#   rightpop151            -0.515*          
#   (0.281)          
# 
# ________________________________________
#   Observations            3,053           
# R2                      0.001           
# Adjusted R2            -0.019           
# F Statistic     3.351* (df = 1; 2993)   
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01



# controls: 
p.reg <- plm(rgdppc.gr ~ rightpop15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")

stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                        rgdppc.gr         
# ________________________________________
#   rightpop151           -0.970***         
#   (0.308)          
# 
# inflation             -0.001***         
#   (0.0002)          
# 
# currcrisis1           -0.665***         
#   (0.252)          
# 
# debtcrisis1           -2.175***         
#   (0.498)          
# 
# bankcrisis1           -2.040***         
#   (0.383)          
# 
# ________________________________________
# Observations            2,279           
# R2                      0.041           
# Adjusted R2             0.019           
# F Statistic   19.245*** (df = 5; 2227)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01

  
  
# Leftpop
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ leftpop5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# controls
p.reg <- plm(rgdppc.gr ~ leftpop5
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ leftpop15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# controls: 
p.reg <- plm(rgdppc.gr ~ leftpop15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")


##### Employment ~ Populist ####
  
# !QUESTIONS:
# Check Controls (which controls are necessary): With debtgdp, populist dummy becomes insignificant
# unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + populist from two Papers

# Check if we can draw causality because we look at 5 (15) year aftermath and have controls


#Populist 
# 5 Years Aftermath
# simple
p.reg <- plm(unemployment ~ populist5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#   unemployment        
# ________________________________________-
#   populist51            -1.135***         
#                      (0.218)          
# 
# ________________________________________
# Observations            1,829           
# R2                      0.015           
# Adjusted R2            -0.018           
# F Statistic   27.095*** (df = 1; 1769)  
# ________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# controls
p.reg <- plm(unemployment ~ populist5
               + population.gr
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                        unemployment        
# ________________________________________
#   populist51             -0.710***         
#   (0.243)          
# 
# gdp.gr                 -0.047***         
#   (0.006)          
# 
# population.gr          -1.476***         
#   (0.200)          
# 
# labor                  -5.124**          
#   (2.221)          
# 
# gini                   0.290***          
#   (0.046)          
# 
# inflation              -0.002**          
#   (0.001)          
# 
# kofglobal              -0.040***         
#   (0.010)          
# 
# currcrisis1             -0.429*          
#   (0.236)          
# 
# debtcrisis1             1.450**          
#   (0.582)          
# 
# bankcrisis1             -0.489           
# (0.319)          
# 
# ________________________________________
# Observations             1,117           
# R2                       0.155           
# Adjusted R2              0.110           
# F Statistic    19.437*** (df = 10; 1060) 
# ________________________________________
# Note:         *p<0.1; **p<0.05; ***p<0.01



# 15 Years Aftermath
# simple
p.reg <- plm(unemployment ~ populist15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                      unemployment        
# ________________________________________
# populist151           -1.197***         
#   (0.222)          
# 
# ________________________________________
# Observations            1,829           
# R2                      0.016           
# Adjusted R2            -0.017           
# F Statistic   29.174*** (df = 1; 1769)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  
# controls: 
p.reg <- plm(unemployment ~ populist15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                     unemployment        
# ________________________________________
# populist151           -0.930***         
#   (0.257)          
# 
# inflation              -0.001           
# (0.001)          
# 
# currcrisis1            -0.112           
# (0.248)          
# 
# debtcrisis1           1.706***          
#   (0.622)          
# 
# bankcrisis1            -0.584*          
#   (0.336)          
# ________________________________________
# Observations            1,128           
# R2                      0.024           
# Adjusted R2            -0.022           
# F Statistic    5.344*** (df = 5; 1076)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01


# Rightpop ####


)



# 5 Years Aftermath
# simple
p.reg <- plm(unemployment ~ rightpop5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                     unemployment        
# ________________________________________
#   rightpop51             -0.528*          
#   (0.286)          
# 
# ________________________________________
# Observations            1,829           
# R2                      0.002           
# Adjusted R2            -0.031           
# F Statistic     3.405* (df = 1; 1769)   
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  
# controls
p.reg <- plm(unemployment ~ rightpop5
               + population.gr
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                         unemployment        
# ________________________________________
#   rightpop51            -0.641**          
#   (0.324)          
# 
# inflation              -0.001           
# (0.001)          
# 
# currcrisis1            -0.152           
# (0.249)          
# 
# debtcrisis1           1.757***          
#   (0.625)          
# 
# bankcrisis1            -0.535           
# (0.337)          
# 
# ________________________________________
# Observations            1,128           
# R2                      0.016           
# Adjusted R2            -0.031           
# F Statistic    3.487*** (df = 5; 1076)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  
  
# 15 Years Aftermath
# simple
p.reg <- plm(unemployment ~ rightpop15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#             unemployment        
# ________________________________________
#   rightpop151            -0.308           
# (0.246)          
# 
#  ________________________________________
# Observations            1,829           
# R2                      0.001           
# Adjusted R2            -0.032           
# F Statistic     1.570 (df = 1; 1769)    
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  
# controls: 
p.reg <- plm(unemployment ~ rightpop15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                unemployment        
# ________________________________________
#   rightpop151            0.524*           
#   (0.280)          
# 
# inflation              -0.001           
# (0.001)          
# 
# currcrisis1            -0.175           
# (0.249)          
# 
# debtcrisis1           1.770***          
#   (0.625)          
# 
# bankcrisis1            -0.461           
# (0.337)          
# 
# ________________________________________
# Observations            1,128           
# R2                      0.016           
# Adjusted R2            -0.031           
# F Statistic    3.406*** (df = 5; 1076)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01


# Leftpop
# 5 Years Aftermath
# simple
p.reg <- plm(unemployment ~ leftpop5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
#   ________________________________________
#   unemployment        
# ________________________________________
#   leftpop51              -0.031           
# (0.386)          
# 
# ________________________________________
#   Observations            1,829           
# R2                     0.00000          
# Adjusted R2            -0.033           
# F Statistic     0.006 (df = 1; 1769)    
# ________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# controls
p.reg <- plm(unemployment ~ leftpop5
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#   unemployment        
# ________________________________________
#   leftpop51             -0.862**          
#   (0.408)          
# 
# inflation              -0.001           
# (0.001)          
# 
# currcrisis1            -0.154           
# (0.249)          
# 
# debtcrisis1           1.825***          
#   (0.626)          
# 
# bankcrisis1            -0.528           
# (0.337)          
# 
# ________________________________________
#   Observations            1,128           
# R2                      0.016           
# Adjusted R2            -0.030           
# F Statistic    3.600*** (df = 5; 1076)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  
  
# 15 Years Aftermath
# simple
p.reg <- plm(unemployment ~ leftpop15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                unemployment        
# ________________________________________
#   leftpop151            -1.881***         
#   (0.312)          
# 
# ________________________________________
# Observations            1,829           
# R2                      0.020           
# Adjusted R2            -0.013           
# F Statistic   36.225*** (df = 1; 1769)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01
  
# controls: 
p.reg <- plm(unemployment ~ leftpop15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")

# ________________________________________
#   Dependent variable:    
# ________________________________________
#                   unemployment        
# ________________________________________
#   leftpop151            -2.654***         
#   (0.350)          
# 
# inflation              -0.001           
# (0.001)          
# 
# currcrisis1            -0.087           
# (0.243)          
# 
# debtcrisis1           1.698***          
#   (0.610)          
# 
# bankcrisis1            -0.640*          
#   (0.329)          
# 
# ________________________________________
# Observations            1,128           
# R2                      0.062           
# Adjusted R2             0.018           
# F Statistic   14.340*** (df = 5; 1076)  
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01


##### Labor ~ Populist #####

# Populist
# 5 Years Aftermath
# simple
p.reg <- plm(labor ~ populist5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# controls
p.reg <- plm(labor ~ populist5
               + debtgdp
               + kofglobal
               + tariffs
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
  
# 15 Years Aftermath
# simple
p.reg <- plm(labor ~ populist15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# controls: 
p.reg <- plm(labor ~ populist15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
  
# Rightpop
# 5 Years Aftermath
# simple
p.reg <- plm(labor ~ rightpop5,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# controls
p.reg <- plm(labor ~ rightpop5
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
  
# 15 Years Aftermath
# simple
p.reg <- plm(labor ~ rightpop15,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")
  
# controls: 
p.reg <- plm(labor ~ rightpop15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.plm,
               model = "within")
stargazer(p.reg, type = "text")



  
  
### 5. SYNTHETIC COUNTERFACTUAL METHOD ####

# Save Core data panel!
write_xlsx(core, path = "C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension/CORE.xlsx")

# Read data back into R after manually filling in populist dummies
core <- readxl::read_xlsx("CORE.xlsx")

# Define 




# HERE SAFE AND LOAD NEEDED DATA FRAME (IMPUTED) TO AVOID COMPLICATIONS








###### Data Imputation #######

# DO SAME FOR EXT DATA !!! ###

# Define new data set excluding growth rates (the)to be calculated again afterwards) 
core.plm$population <- as.numeric(core.plm$population)

# We exclude variables that we don't want to impute:
# Count, cid, 5x var.gr (can be recalculated afterwards based on imputed data), war (exogenous), conflicts (exogenous)
col_order.imp <- c("Country",
                   "code",
                   "year",
                   "population",
                   "population.gr",
                   "fstgdp",
                   "gdp",
                   "gdp.gr",
                   "rgdp",
                   "rgdp.gr",
                   "gdppc",
                   "rgdppc",
                   "rgdppc.gr",
                   "consumption",
                   "inflation",
                   "debtgdp",
                   "pbalance",
                   "tariffs",
                   "kofglobal",
                   "koftrade",
                   "tradegdp",
                   "fdi",
                   "fdi.gr",
                   "labor",
                   "gini",
                   "unemployment",
                   "institutions",
                   "judicial",
                   "media",
                   "electoral",
                   "currcrisis",
                   "debtcrisis",
                   "bankcrisis",
                   "populist",
                   "rightpop",
                   "leftpop",
                   "populist5",
                   "populist15",
                   "rightpop5",
                   "rightpop15",
                   "leftpop5",
                   "leftpop15")

core.imp <- core[, col_order.imp]
str(core.imp)

# Impute "0" for NAs in bankcrisis, debtcrisis and currcrisis (no data and only control)
core.imp$currcrisis <- as.character(core.imp$currcrisis)
core.imp$currcrisis <- as.numeric(core.imp$currcrisis)
core.imp$debtcrisis <- as.character(core.imp$debtcrisis)
core.imp$debtcrisis <- as.numeric(core.imp$debtcrisis)
core.imp$bankcrisis <- as.character(core.imp$bankcrisis)
core.imp$bankcrisis <- as.numeric(core.imp$bankcrisis)

core.imp$currcrisis <- ifelse(is.na(core.imp$currcrisis), 0, core.imp$currcrisis )
core.imp$debtcrisis <- ifelse(is.na(core.imp$debtcrisis), 0, core.imp$debtcrisis )
core.imp$bankcrisis <- ifelse(is.na(core.imp$bankcrisis), 0, core.imp$bankcrisis )

core.imp$currcrisis <- as.factor(core.imp$currcrisis)
core.imp$debtcrisis <- as.factor(core.imp$debtcrisis)
core.imp$bankcrisis <- as.factor(core.imp$bankcrisis)

sum(is.na(core.imp$currcrisis))
sum(is.na(core.imp$debtcrisis))
sum(is.na(core.imp$bankcrisis))

summary(core.imp)

# Impute missing values
# All variables:
"population",
"population.gr",
"fstgdp",
"gdp",
"gdp.gr",
"rgdppc",
"rgdppc.gr",
"consumption",
"inflation",
"debtgdp",
"pbalance",
"tariffs",
"kofglobal",
"koftrade",
"tradegdp",
"fdi",
"fdi.gr",
"labor",
"gini",
"unemployment",
"institutions",
"judicial",
"media",
"electoral",
"currcrisis",
"debtcrisis",
"bankcrisis",
"conflicts",
"war",
"populist",
"rightpop",
"leftpop",
"populist5",
"populist15",
"rightpop5",
"rightpop15",
"leftpop5",
"leftpop15"


# Defining variables to use for imputation - Different Approaches

# 1. Old approach (gut feeling)
predmatrix.old <-quickpred(core.imp, 
                           include=c("population",
                                     "fstgdp",
                                     "consumption",
                                     "inflation",
                                     "debtgdp",
                                     
                                     "kofglobal",
                                     "tradegdp",
                                     "fdi",
                                     
                                     "labor",
                                     "gini",
                                     "unemployment",
                                     
                                     "judicial",
                                     
                                     "electoral",
                                     "currcrisis",
                                     "debtcrisis",
                                     "bankcrisis"),
                           exclude=c("Country",
                                     "code",
                                     "year",
                                     "population.gr",
                                     "gdp.gr",
                                     "gdp",
                                     "rgdppc",
                                     "rgdppc.gr",
                                     "pbalance",
                                     "tariffs",
                                     "koftrade",
                                     "fdi.gr",
                                     "institutions",
                                     "media",
                                     "conflicts",
                                     "war",
                                     "populist",
                                     "rightpop",
                                     "leftpop",
                                     "populist5",
                                     "populist15",
                                     "rightpop5",
                                     "rightpop15",
                                     "leftpop5",
                                     "leftpop15"),
                      mincor = 0.1)


# 2. Define prediction matrix including variables with highest expected prediction power (& least missing values)
# and excluding values leading to mulitcollinearity (& most missing values).

# Correlation matrix
core.cor <- dplyr::select(core.plm, c("population", 
                                      "fstgdp",
                                      "gdp",
                                      "consumption",
                                      "inflation",
                                      "debtgdp",
                                      "pbalance",
                                      "tariffs",
                                      "kofglobal",
                                      "koftrade",
                                      "tradegdp",
                                      "fdi",
                                      "labor",
                                      "gini",
                                      "unemployment",
                                      "institutions",
                                      "judicial",
                                      "media",
                                      "electoral",
                                      "currcrisis",
                                      "debtcrisis",
                                      "bankcrisis",
                                      "conflicts",
                                      "populist",
                                      "rightpop") )

core.cor$institutions <- as.numeric(core.cor$institutions)
core.cor$judicial <- as.numeric(core.cor$judicial)
core.cor$media <- as.numeric(core.cor$media)
core.cor$electoral <- as.numeric(core.cor$electoral)
core.cor$currcrisis <- as.numeric(core.cor$currcrisis)
core.cor$debtcrisis <- as.numeric(core.cor$debtcrisis)
core.cor$bankcrisis <- as.numeric(core.cor$bankcrisis)
core.cor$conflicts <- as.numeric(core.cor$conflicts)
core.cor$populist <- as.numeric(core.cor$populist)
core.cor$rightpop <- as.numeric(core.cor$rightpop)

correlations <- cor(core.cor, method = "pearson", use = "complete.obs")


#CorrMatrix GDP
cor.gdp <- dplyr::select(core.plm, c("fstgdp", "gdp", "gdp.gr", "rgdp", "rgdp.gr", "gdppc", "rgdppc", "rgdppc.gr"))
corel.gdp <- cor(cor.gdp, method = "pearson", use = "complete.obs")

#CorrMatrix populist
cor.pop <- dplyr::select(core.plm, c("populist", "rightpop", "leftpop", "populist5", "populist15", "rightpop5", "rightpop15", "leftpop5", "leftpop15"))

cor.pop$populist <- as.numeric(cor.pop$populist)
cor.pop$rightpop <- as.numeric(cor.pop$rightpop)
cor.pop$leftpop <- as.numeric(cor.pop$leftpop )
cor.pop$populist5 <- as.numeric(cor.pop$populist5)
cor.pop$populist5 <- as.numeric(cor.pop$populist15)
cor.pop$populist15 <- as.numeric(cor.pop$rightpop5)
cor.pop$rightpop5 <- as.numeric(cor.pop$rightpop15)
cor.pop$leftpop5 <- as.numeric(cor.pop$leftpop5)
cor.pop$leftpop15 <- as.numeric(cor.pop$leftpop15)

str(cor.pop)
  


corel.pop <- cor(cor.pop, method = "pearson", use = "complete.obs")

# minor=0.23 indicates that the correlation conducted by quickpred will be compared to 30% of the data set without any missing values
# minor=0.23 calculated by share of core.omit/core.imp to get share of core.imp that is withou NAs
# core.omit <- na.omit(core.plm)

predmatrix<-quickpred(core.imp, 
                      include=c("population.gr", 
                                "fstgdp",
                                "gdppc",
                                "rgdppc",
                                "rdgppc.gr",
                                "inflation",
                                "debtgdp",
                                "kofglobal",
                                "pbalance",
                                "fdi",
                                "tradegdp",
                                "labor",
                                "unemployment",
                                "media",
                                "bankcrisis",
                                "populist",
                                "leftpop"), 
                      exclude=c("Country",
                                "code",
                                "year",
                                "population",
                                "gdp", # !correlated with fdi (0.6)# correlated with many variables
                                "gdp.gr",
                                "rgdp",
                                "rgdp.gr",
                                "fdi.gr",
                                "consumption", # correlates with tariffs, kofglobal, labor!, institutions
                                "tariffs",# correlated with kofglobal, gini
                                "koftrade",# correlated with kofglobal
                                "gini", # correlated with labor: https://www.emerald.com/insight/content/doi/10.1108/AEA-04-2020-0028/full/html
                                "institutions", # all correlated with media and idividually with many other vars (gdp, labor, gini etc.)
                                "judicial",
                                "electoral",
                                "currcrisis",
                                "debtcrisis",
                                "populist",
                                "populist5", # all correlated with populist 
                                "populist15",
                                "rightpop",
                                "leftpop",
                                "rightpop5",
                                "rightpop15",
                                "leftpop5",
                                "leftpop15"),
                      mincor = 0.2)


# 3. Using all variables neglecting multicollinearity
predmatrix.all<-quickpred(core.imp, 
                          include=c("fstgdp",
                                    "gdp", # !correlated with fdi (0.6)
                                    "inflation",
                                    "debtgdp",
                                    "kofglobal",
                                    "pbalance",
                                    "fdi",
                                    "tradegdp",
                                    "labor",
                                    "unemployment",
                                    "media",
                                    "currcrisis",
                                    "debtcrisis",
                                    "bankcrisis",
                                    "populist",
                                    "population", # correlated with many variables
                                    "rgdp",# correlated with gdp
                                    "gdppc",# correlated with gdp
                                    "rgdppc",# correlated with gdp
                                    "consumption", # correlates with tariffs, kofglobal, labor!, institutions
                                    "tariffs",# correlated with kofglobal, gini
                                    "koftrade",# correlated with kofglobal
                                    "gini", # correlated with labor: https://www.emerald.com/insight/content/doi/10.1108/AEA-04-2020-0028/full/html
                                    "institutions", # all correlated with media and idividually with many other vars (gdp, labor, gini etc.)
                                    "judicial",
                                    "electoral",
                                    "populist5", # all correlated with populist 
                                    "populist15",
                                    "rightpop",
                                    "leftpop",
                                    "rightpop5",
                                    "rightpop15",
                                    "leftpop5",
                                    "leftpop15"),
                          exclude=c("Country",
                                    "code",
                                    "year"),
                          mincor = 0.2)



# IMPUTATION WITH MICE (20 iterations, creating 5 data sets)

# RANDOM FOREST IMPUTATION - PREDMATRIX
imputation.rf <- mice(core.imp, 
            predictorMatrix = predmatrix,
            m=5,
            maxit=5,
            meth='rf',
            seed = 500)

# Now the result is without populist as predictor for imputation:
# Dependent variable:    
# ________________________________________
#   rgdppc.gr         
#   ________________________________________
#   populist51            -0.519**          
#   (0.246)          
# 
#   ________________________________________
#   Observations            3,718           
# R2                      0.001           
# Adjusted R2            -0.015           
# F Statistic    4.474** (df = 1; 3657)   
# ________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# Now the result ist without populist in "included" AND now also in "EXCLUDED" for imputation:
# ________________________________________
#   Dependent variable:    
# ________________________________________
#                          rgdppc.gr         
# ________________________________________
#   populist51             -0.438*          
#   (0.249)          
# 
# ________________________________________
# Observations            3,718           
# R2                      0.001           
# Adjusted R2            -0.016           
# F Statistic     3.103* (df = 1; 3657)   
# ________________________________________
# Note:        *p<0.1; **p<0.05; ***p<0.01


# Transform imputations into data frame
core.imp.rf <- complete(imputation.rf)

# Define as panel data
core.imp.rf <- pdata.frame(core.imp.rf, index = c("Country","year"))

summary(core.plm$rgdppc.gr)
summary(core.plm$rgdppc.gr)
summary(core.imp.rf)
summary(core.imp)

str(core.imp.rf)

# Recalculate growth rates
core.imp.rf$population.gr.comp <- ((core.imp.rf$population - lag(core.imp.rf$population))/lag(core.imp.rf$population))*100
core.imp.rf$gdp.gr.comp <- ((core.imp.rf$gdp - lag(core.imp.rf$gdp))/lag(core.imp.rf$gdp))*100 
core.imp.rf$rgdp.gr.comp <- ((core.imp.rf$rgdp - lag(core.imp.rf$rgdp))/lag(core.imp.rf$rgdp))*100
core.imp.rf$gdppc.gr.comp <- ((core.imp.rf$gdppc - lag(core.imp.rf$gdppc))/lag(core.imp.rf$gdppc))*100
core.imp.rf$rgdppc.gr.comp <- ((core.imp.rf$rgdppc - lag(core.imp.rf$rgdppc))/lag(core.imp.rf$rgdppc))*100
core.imp.rf$fdi.gr.comp <- round(((core.imp.rf$fdi - lag(core.imp.rf$fdi))/lag(core.imp.rf$fdi))*100)

# Check for NAs
summary(core.plm)

# fdi.gr still has NAs
core.imp.rf$fdi.gr <- NULL



# # Transform imputations into data frame
# core.imp.rf2 <- complete(imputation.rf)
# 
# # Define as panel data
# core.imp.rf2 <- pdata.frame(core.imp.rf2, index = c("Country","year"))




# Panel Regressions to check quality of imputation with core.imp.rf
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist5,
             data = core.imp.rf,
             model = "within")
stargazer(p.reg, type = "text")

#                 Dependent variable:
# _______________________________________#
#                       rgdppc.gr
#   _______________________________________
#   populist51             -0.357
# (0.236)
# 
# ----------------------------------------#
# Observations            4,558
# R2                      0.001
# Adjusted R2            -0.013
# F Statistic     2.275 (df = 1; 4497)
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.0


# controls
p.reg <- plm(rgdppc.gr ~ populist5
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.rf,
             model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#                       rgdppc.gr         
# ----------------------------------------#
#   populist51             -0.373           
# (0.235)          
# 
# inflation             -0.002***         
#   (0.0003)          
# 
# currcrisis1            -0.008           
# (0.222)          
# 
# debtcrisis1           -2.025***         
#   (0.536)          
# 
# bankcrisis1           -2.145***         
#   (0.396)          
# 
# ----------------------------------------#
# Observations            4,558           
# R2                      0.019           
# Adjusted R2             0.005           
# F Statistic   17.037*** (df = 5; 4493)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist15,
             data = core.imp.rf,
             model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   populist151            -0.403*          
#   (0.214)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.001           
# Adjusted R2            -0.013           
# F Statistic     3.537* (df = 1; 4497)   
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# controls: 
p.reg <- plm(rgdppc.gr ~ populist15
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.rf,
             model = "within")
stargazer(p.reg, type = "text")


# Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   populist151           -0.425**          
#   (0.212)          
# 
# inflation             -0.002***         
#   (0.0003)          
# 
# currcrisis1            -0.006           
# (0.222)          
# 
# debtcrisis1           -1.990***         
#   (0.536)          
# 
# bankcrisis1           -2.147***         
#   (0.396)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.019           
# Adjusted R2             0.005           
# F Statistic   17.339*** (df = 5; 4493)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# Rightpop
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ rightpop5,
               data = core.imp.rf,
               model = "within")
stargazer(p.reg, type = "text")

# #                 Dependent variable:    
# ---------------------------##
#   rgdppc.gr         
# ----------------------------------------#
#   rightpop51             -0.029           
# (0.365)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                     0.00000          
# Adjusted R2            -0.013           
# F Statistic     0.006 (df = 1; 4497)    
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01

  
# controls
p.reg <- plm(rgdppc.gr ~ rightpop5
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.imp.rf,
               model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   rightpop51             -0.069           
# (0.362)          
# 
# inflation             -0.002***         
#   (0.0003)          
# 
# currcrisis1            -0.012           
# (0.222)          
# 
# debtcrisis1           -1.995***         
#   (0.536)          
# 
# bankcrisis1           -2.131***         
#   (0.396)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.018           
# Adjusted R2             0.004           
# F Statistic   16.530*** (df = 5; 4493)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01




  
# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ rightpop15,
               data = core.imp.rf,
               model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   rightpop151            -0.432           
# (0.270)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.001           
# Adjusted R2            -0.013           
# F Statistic     2.561 (df = 1; 4497)    
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# controls: 
p.reg <- plm(rgdppc.gr ~ rightpop15
               + inflation
               + currcrisis
               + debtcrisis
               + bankcrisis,
               data = core.imp.rf,
               model = "within")

stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   rightpop151            -0.478*          
#   (0.268)          
# 
# inflation             -0.002***         
#   (0.0003)          
# 
# currcrisis1            -0.013           
# (0.222)          
# 
# debtcrisis1           -2.019***         
#   (0.536)          
# 
# bankcrisis1           -2.131***         
#   (0.396)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.019           
# Adjusted R2             0.005           
# F Statistic   17.171*** (df = 5; 4493)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# Testing with Unemployment ~ Populist
#Populist 
# 5 Years Aftermath
# simple
p.reg <- plm(unemployment ~ populist5,
             data = core.imp.rf,
             model = "within")
stargazer(p.reg, type = "text")

# For Unemployment there were ro many NAs!



#### CART IMPUTATION - PREDMATRIX  #
imputation.cart <- mice(core.imp, 
                      predictorMatrix = predmatrix,
                      m=5,
                      maxit=10,
                      meth='cart',
                      seed = 500)

# Transform imputations into data frame
core.imp.cart <- complete(imputation.cart)

# Define as panel data
core.imp.cart <- pdata.frame(core.imp.cart, index = c("Country","year"))

# Check for remaining  NAs
sum(is.na(core.imp.cart))
summary(core.imp.cart)

# fdi.gr still has NAs
core.imp.cart$fdi.gr <- NULL


# Recalculate growth rates
core.imp.cart$population.gr.comp <- ((core.imp.cart$population - lag(core.imp.cart$population))/lag(core.imp.cart$population))*100
core.imp.cart$gdp.gr.comp <- ((core.imp.cart$gdp - lag(core.imp.cart$gdp))/lag(core.imp.cart$gdp))*100 
core.imp.cart$rgdp.gr.comp <- ((core.imp.cart$rgdp - lag(core.imp.cart$rgdp))/lag(core.imp.cart$rgdp))*100
core.imp.cart$gdppc.gr.comp <- ((core.imp.cart$gdppc - lag(core.imp.cart$gdppc))/lag(core.imp.cart$gdppc))*100
core.imp.cart$rgdppc.gr.comp <- ((core.imp.cart$rgdppc - lag(core.imp.cart$rgdppc))/lag(core.imp.cart$rgdppc))*100
core.imp.cart$fdi.gr.comp <- round(((core.imp.cart$fdi - lag(core.imp.cart$fdi))/lag(core.imp.cart$fdi))*100)


# Panel Regressions to check quality of imputation with core.imp.rf
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist5,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")


# ========================================#
#   Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ---------------------------------------- #
#   populist51             -0.470*          
#   (0.240)          
# 
# ----------------------------------------#
#   Observations            3,718           
# R2                      0.001           
# Adjusted R2            -0.015           
# F Statistic     3.825* (df = 1; 3657)   
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# controls
p.reg <- plm(rgdppc.gr ~ populist5
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# ========================================#
#   Dependent variable:
#   --------------------------- #
#   rgdppc.gr
# ----------------------------------------#
#   populist51             -0.459*
#   (0.238)
# 
# inflation             -0.002***
#   (0.0002)
# 
# currcrisis1            -0.388*
#   (0.220)
# 
# debtcrisis1           -1.937***
#   (0.461)
# 
# bankcrisis1           -1.732***
#   (0.369)
# 
# ----------------------------------------#
#   Observations            3,718
# R2                      0.025
# Adjusted R2             0.008
# F Statistic   18.483*** (df = 5; 3653)
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist15,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# ======================================== #
#   Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   populist151           -0.469**          
#   (0.219)          
# 
# ----------------------------------------#
#   Observations            3,718           
# R2                      0.001           
# Adjusted R2            -0.015           
# F Statistic    4.562** (df = 1; 3657)   
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# controls: 
p.reg <- plm(rgdppc.gr ~ populist15
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# ========================================#
#   Dependent variable:    
#   ---------------------------#
#   rgdppc.gr         
# ----------------------------------------#
#   populist151           -0.480**          
#   (0.217)          
# 
# inflation             -0.002***         
#   (0.0002)          
# 
# currcrisis1            -0.388*          
#   (0.220)          
# 
# debtcrisis1           -1.899***         
#   (0.461)          
# 
# bankcrisis1           -1.736***         
#   (0.369)          
# 
# ----------------------------------------#
#   Observations            3,718           
# R2                      0.025           
# Adjusted R2             0.008           
# F Statistic   18.721*** (df = 5; 3653)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# Testing with Unemployment ~ Populist
#Populist 
# 5 Years Aftermath
# simple
p.reg <- plm(unemployment ~ populist5,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   unemployment        
# ----------------------------------------#
#   populist51            0.712***          
#   (0.257)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.002           
# Adjusted R2            -0.012           
# F Statistic    7.690*** (df = 1; 4497)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



# controls
p.reg <- plm(unemployment ~ populist5
             + gdp.gr
             + population.gr
             + labor
             + gini
             + inflation
             + kofglobal
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   unemployment        
# -----------------------------------------#
#   populist51               0.305           
# (0.258)          
# 
# gdp.gr                 -0.016***         
#   (0.005)          
# 
# population.gr          -1.019***         
#   (0.096)          
# 
# labor                  7.380***          
#   (1.278)          
# 
# gini                   0.062***          
#   (0.013)          
# 
# inflation              -0.00002          
# (0.0003)          
# 
# kofglobal               -0.006           
# (0.004)          
# 
# currcrisis1            0.658***          
#   (0.221)          
# 
# debtcrisis1              0.550           
# (0.510)          
# 
# bankcrisis1             -0.546           
# (0.423)          
# 
# -----------------------------------------#
#   Observations             4,558           
# R2                       0.039           
# Adjusted R2              0.025           
# F Statistic    18.367*** (df = 10; 4488) 
# =========================================#
#   Note:         *p<0.1; **p<0.05; ***p<0.01


# 15 Years Aftermath
# simple
p.reg <- plm(unemployment ~ populist15,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# Dependent variable:    
#   ---------------------------#
#   unemployment        
# ----------------------------------------#
#   populist151           0.861***          
#   (0.232)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.003           
# Adjusted R2            -0.010           
# F Statistic   13.745*** (df = 1; 4497)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# controls: 
p.reg <- plm(unemployment ~ populist15
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")


# Dependent variable:    
#   ---------------------------#
#   unemployment        
# ----------------------------------------#
#   populist151           0.841***          
#   (0.232)          
# 
# inflation             -0.00003          
# (0.0003)          
# 
# currcrisis1           0.736***          
#   (0.223)          
# 
# debtcrisis1             0.618           
# (0.516)          
# 
# bankcrisis1            -0.491           
# (0.429)          
# 
# ----------------------------------------#
#   Observations            4,558           
# R2                      0.006           
# Adjusted R2            -0.008           
# F Statistic    5.448*** (df = 5; 4493)  
# ========================================#
#   Note:        *p<0.1; **p<0.05; ***p<0.01



## IMPUTATION CONCLUSION ###

# core.imp.cart provides similar results as in paper

# core.imp.rf provides simimlar results as data.omit regressions

## Unemployment with over 2729 NAs has to many imputations (>50%) to provide good plm results. Thus, stick to df.omit results
summary(core)



# CART IMPUTATION - PREDMATRIX.OLD
imputation.cart.old <- mice(core.plm, 
                        predictorMatrix = predmatrix.old,
                        m=5,
                        maxit=5,
                        meth='cart',
                        seed = 500)

rm(core.imp.cart.old)

# Transform imputations into data frame
core.imp.cart.old <- complete(imputation.cart.old)

# Define as panel data
core.imp.cart.old <- pdata.frame(core.imp.cart.old, index = c("Country","year"))


# Recalculate growth rates
core.imp.cart.old$population.gr <- ((core.imp.cart.old$population - lag(core.imp.cart.old$population))/lag(core.imp.cart.old$population))*100
core.imp.cart.old$gdp.gr <- ((core.imp.cart.old$gdp - lag(core.imp.cart.old$gdp))/lag(core.imp.cart.old$gdp))*100 
core.imp.cart.old$rgdp.gr <- ((core.imp.cart.old$rgdp - lag(core.imp.cart.old$rgdp))/lag(core.imp.cart.old$rgdp))*100
core.imp.cart.old$gdppc.gr <- ((core.imp.cart.old$gdppc - lag(core.imp.cart.old$gdppc))/lag(core.imp.cart.old$gdppc))*100
core.imp.cart.old$rgdppc.gr <- ((core.imp.cart.old$rgdppc - lag(core.imp.cart.old$rgdppc))/lag(core.imp.cart.old$rgdppc))*100
core.imp.cart.old$fdi.gr <- round(((core.imp.cart.old$fdi - lag(core.imp.cart.old$fdi))/lag(core.imp.cart.old$fdi))*100)



core.imp.cart.old$rgdppc.gr <- core.imp.cart.old$rgdp/core.imp.cart.old$population


# Panel Regressions to check quality of imputation with core.imp.rf
# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist5,
             data = core.imp.cart.old,
             model = "within")
stargazer(p.reg, type = "text")


# controls
p.reg <- plm(rgdppc.gr ~ populist5
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.cart.old,
             model = "within")
stargazer(p.reg, type = "text")


# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist15,
             data = core.imp.cart.old,
             model = "within")
stargazer(p.reg, type = "text")

# controls: 
p.reg <- plm(rgdppc.gr ~ populist15
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.cart.old,
             model = "within")
stargazer(p.reg, type = "text")


# RANDOM FOREST IMPUTATION - PREDMATRIX.ALL
imputation.rf.all <- mice(core.imp, 
                          predictorMatrix = predmatrix.all,
                          m=5,
                          maxit=20,
                          meth='rf',
                          seed = 500)

# Transform imputations into data frame
core.imp.rf.all <- complete(imputation.rf)

# Define as panel data
core.imp.rf.all <- pdata.frame(core.imp.rf, index = c("Country","year"))

# Recalculate growth rates
core.imp.rf.all$population.gr <- ((core.imp.rf.all$population - lag(core.imp.rf.all$population))/lag(core.imp.rf.all$population))*100
core.imp.rf.all$gdp.gr <- ((core.imp.rf.all$gdp - lag(core.imp.rf.all$gdp))/lag(core.imp.rf.all$gdp))*100 
core.imp.rf.all$rgdp.gr <- ((core.imp.rf.all$rgdp - lag(core.imp.rf.all$rgdp))/lag(core.imp.rf.all$rgdp))*100
core.imp.rf.all$gdppc.gr <- ((core.imp.rf.all$gdppc - lag(core.imp.rf.all$gdppc))/lag(core.imp.rf.all$gdppc))*100
core.imp.rf.all$rgdppc.gr <- ((core.imp.rf.all$rgdppc - lag(core.imp.rf.all$rgdppc))/lag(core.imp.rf.all$rgdppc))*100
core.imp.rf.all$fdi.gr <- round(((core.imp.rf.all$fdi - lag(core.imp.rf.all$fdi))/lag(core.imp.rf.all$fdi))*100)



# THAT WAS WITH PREVIOUS DATA: TO GO BACK USE UNDO

# 5 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist5,
             data = core.imp.rf1,
             model = "within")
stargazer(p.reg, type = "text")


#RESULTS:
# _________________________________________
#   Dependent variable:    
# _________________________________________
#   rgdppc.gr         
# _________________________________________
#   populist51            -1.823**          
#   (0.800)          
# 
# _________________________________________
#   Observations            3,718           
# R2                      0.001           
# Adjusted R2            -0.015           
# F Statistic    5.199** (df = 1; 3657)   
# _________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01


# controls
p.reg <- plm(rgdppc.gr ~ populist5
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.rf1,
             model = "within")
stargazer(p.reg, type = "text")


# 15 Years Aftermath
# simple
p.reg <- plm(rgdppc.gr ~ populist15,
             data = core.imp.rf1,
             model = "within")
stargazer(p.reg, type = "text")

# controls: 
p.reg <- plm(rgdppc.gr ~ populist15
             + inflation
             + currcrisis
             + debtcrisis
             + bankcrisis,
             data = core.imp.rf1,
             model = "within")
stargazer(p.reg, type = "text")



# CART AND VARIABLES BELOW
imputation.cart <- mice(core.imp, 
                   predictorMatrix = predmatrix,
                   m=5,
                   maxit=5,
                   meth='cart',
                   seed = 500)
# Using cart and the inclusion/exclusion of these variables:



core.imp.cart <- complete(imputation.cart)
core.imp.cart <- pdata.frame(core.imp.cart, index = c("Country","year"))

p.reg <- plm(rgdppc.gr ~ populist5,
             data = core.imp.cart,
             model = "within")
stargazer(p.reg, type = "text")

# Results:
# _________________________________________
#   Dependent variable:    
# _________________________________________
#   rgdppc.gr         
# _________________________________________
#   populist51            -1.980**          
#   (0.821)          
# 
# _________________________________________
#   Observations            3,718           
# R2                      0.002           
# Adjusted R2            -0.015           
# F Statistic    5.822** (df = 1; 3657)   
# _________________________________________
#   Note:        *p<0.1; **p<0.05; ***p<0.01


summary(core.imp.cart.old$rgdppc.gr)






##### SCM Application ####

# SETUP ####
#Set working directory
setwd("C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension")

# Deactivate scientific notation
options(scipen=999)

# Library
pacman::p_load("lattice", "ggplot2", "skimr", "caret", "dplyr", "esquisse", "corrplot", 
               "rpart", "rpart.plot", "outliers", "ppcor", "reshape2", "gmodels", "nnet", 
               "lmtest", "plm", "tidyr", "panelr", "tidyverse", "stargazer", "psych", "writexl",
               "car","MSCMT", "RANN", "cmaes", "nloptr", "NMOF", "DEoptim", "GA", "rgenoud", "GenSA",
               "hydroPSO", "pso", "soma", "imputeTS", "mice", "ggcorrplot")



### DATA ###
#write_xlsx(core.imp.rf, path = "C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension/CORE.IMP.RF.xlsx")
# Read data back into R after manually filling in populist dummies
core.imp.rf <- readxl::read_xlsx("CORE.IMP.RF.xlsx")
# Create democracy index
core.plm$democracy <- ((core.plm$institutions)^2 + core.plm$judicial + core.plm$media + core.plm$electoral)/4
summary(core.imp.rf)




##### PANEL RESULTS ####

write_xlsx(core.plm, path = "C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension/CORE.PLM.xlsx")
core.plm <- readxl::read_xlsx("CORE.PLM.xlsx")


df.plm <- subset(core.imp.rf, year >= "1960" & year <="2018")

p.reg <- plm(unemployment ~ 
             + population.gr
             + fstgdp
             + kofglobal
             + inflation
             + democracy,
             data = core.plm,
             model = "within")
stargazer(p.reg, type = "text")



















### FIRST APPROACH ###
col_order.scm <- c("Country",
                   "year",
                   "population",
                   "gdp",
                   "inflation",
                   "fdi",
                   "unemployment",
                   "populist")

core.scm <- core.imp.rf[, col_order.scm]
str(core.imp.rf)
summary(core.imp.rf)
sum(is.na(core.imp.rf))

# Reduce time period to 1990 - 2018
core.scm$year <- as.character(core.scm$year)
core.scm$year <- as.numeric(core.scm$year)
core.scm <- subset(core.scm, year >= "1999" & year <="2021")
core.scm$year <- as.factor(core.scm$year)
core.scm <- pdata.frame(core.scm, index = c("Country", "year"))
str(core.scm)


# Defining lists of European and rightwing countries
europe <- c("Bulgaria", 
            "Germany",
            "Hungary",
            "Italy",
            "Poland",
            "Slovak Republic",
            "Turkiye")

rightwing <- c("Bulgaria", 
               "Ecuador",
               "Germany",
               "Hungary",
               "India",
               "Israel",
               "Italy",
               "Japan",
               "New Zealand", 
               "Poland",
               "Korea, Rep.",
               "Taiwan",
               "Turkiye",
               "United States")


  
  
#Unemployment - Hungary
### IDENTIFIER ###
# Bulgaria 2009-2021  - check
# Hungary 2010 - 2021 - check
# Italy 1994 - 2021 attention with interuptions
# Poland 2005 - 2021 attention with interruption between 2007-2015
# Slovakia 1990 - 1998
# Turkiye 2003 - 2021
# united states


### IDENTIFIER ###
# Define vector with treated and donor (non-treated) countries
treated <- dplyr::filter(core.scm2, populist == 1)
treated$Country <- as.character(treated$Country)
str(treated)
treated.index <- unique(treated$Country)
print(treated.index)
is.vector(treated.index)

# Define controls.identifier containing all donor countries (non-treated)
controls.identifier <- dplyr::setdiff(core.scm2$Country, treated.index)
str(controls.identifier)
is.vector(controls.identifier)
print(controls.identifier)

# Define treatment.identifier containing the treated country under investigation
treatment.identifier <- c("Bulgaria")
is.vector(treatment.identifier)
# Defining time periods for optimization of dependent variable and covariates
# Hungary: Populist event takes place in 20010, thus optimization period: 1991-2009
# HERE WE USE THE ABSOLUTE VALUES AND NOT GRWOTH RATES!
check <- subset(core.plm, year >= "1990" & year <="2018")

summary(check)

# First APPROACH: Good fit
times.dep <- cbind("unemployment" = c(1999,2009))
times.pred <- cbind("population"= c(1999,2009),
                    "gdp"= c(1999,2009),
                    "inflation"= c(1999,2009),
                    "fdi"= c(1999,2009),
                    "unemployment"= c(1999,2009))



is.matrix(times.dep)
is.matrix(times.pred)
# Change data type and check if its a matrix
# times.dep <- as.matrix(times.dep)
# times.pred <- as.matrix(times.pred)
  
# WHAT ABOUT THIS STEP? -> Not necessary
agg.fns <- rep("mean", ncol(times.pred)) 
  
  
# Unit variable & time variable must be factors
# core.scm$year <- as.character(core.scm$year)
# core.scm$year <- as.numeric(core.scm$year)
str(core.scm)
summary(core.scm)
sum(is.na(core.scm))


# Delete populist from core.scm
core.scm$populist <- NULL

core.scm_list.bul3 <- listFromLong(core.scm, unit.variable = "Country", time.variable = "year")
# MAYBE I NEED TO ADD THE OPTIONAL ARGZUMENT "unit.names.variable ="??

# FIRST APPROACH: Apply the SCM Method
scm.bul3 <- mscmt(core.scm_list.bul3, treatment.identifier, controls.identifier, times.dep, times.pred)
scm.hun
  
# Plotting SCM Results
# ggplot(scm.hun, type = "comparison", 
#        mapping = aes(), 
#        ratio.type = "mspe", 
#        labels = c("Hungary", 
#                   "Synthetic Control"))
# 
# 



# 
# ggplot(scm.hun, type = "gaps")



windowsFonts(A = windowsFont("Times New Roman"))
plot(scm.bul3, "unemployment", 
     type = ("comparison"), 
     2009,
     legend = FALSE,
     zero.line = TRUE, 
     ylab = "Unemployment rate", 
     xlab = "Year",
     main = "Hungary", 
     sub = "",
     family = "A")
legend(x = "topright",
       legend = c("Hungary", "Synthetic control"),
       col = c(1,2),
       lwd = 2)


# Optimal weights:
#   Austria          Croatia Egypt, Arab Rep.          Germany          Ireland         Portugal            Spain 
# 0.41615732       0.03132249       0.02715964       0.01897987       0.31243461       0.02502408       0.16892199 






























### SECOND APPROACH ###
col_order.scm2 <- c("Country",
                    "year",
                    "rgdppc",
                    "rgdppc.gr",
                    "tradegdp",
                    "debtgdp",
                    "labor",
                    "unemployment",
                    "democracy",
                    "populist")


core.scm2 <- core.imp.rf[, col_order.scm2]
str(core.scm2)
summary(core.scm2)
sum(is.na(core.scm2))

# Reduce time period to 1990 - 2021
core.scm2$year <- as.character(core.scm2$year)
core.scm2$year <- as.numeric(core.scm2$year)
core.scm2 <- subset(core.scm2, year >= "1995" & year <="2021")
core.scm2$year <- as.factor(core.scm2$year)
core.scm2 <- pdata.frame(core.scm2, index = c("Country", "year"))
str(core.scm)
summary(core.scm2)

### IDENTIFIER ###
treatment.identifier <- c("Hungary")


# Define vector with treated and donor (non-treated) countries
treated <- dplyr::filter(core.scm2, populist == 1)
treated$Country <- as.character(treated$Country)
str(treated)
treated.index <- unique(treated$Country)
print(treated.index)
is.vector(treated.index)

# Define controls.identifier containing all donor countries (non-treated)
controls.identifier <- dplyr::setdiff(core.scm2$Country, treated.index)
str(controls.identifier)
is.vector(controls.identifier)
print(controls.identifier)



# Bulgaria 2009-2021 
# Hunary 2010 - 2021
# Italy 1994 - 2021 attention with interuptions
# Poland 2005 - 2021 attention with interruption between 2007-2015
# Slovakia 1990 - 1998
# Turkey 2003 - 2021












# Second APPROACH:
times.dep <- cbind("unemployment" = c(1995,2010))
times.pred2 <- cbind("rgdppc"= c(1995,2010), # economic well-being
                     "rgdppc.gr"= c(1995,2010), # dynamics of economic well-being
                     "tradegdp" = c(1995, 2010), # economic integration
                     "deptgdp" = c(1995, 2010), # fiscal policy
                     "labor"= c(1995,2010), # income inequality
                     "democracy"= c(1995,2010), # democracy
                     "unemployment"= c(1995,2010)) # variable of interest
                     
# Delete populist from core.scm
core.scm2$populist <- NULL

core.scm_list2 <- listFromLong(core.scm2, unit.variable = "Country", time.variable = "year")
# MAYBE I NEED TO ADD THE OPTIONAL ARGZUMENT "unit.names.variable ="??

print(core.scm_list)
str(core.scm_list)
summary(core.scm_list)
sum(is.na(core.scm_list2))


# SECOND APPROACH: Apply the SCM Method2
scm.hun2 <- mscmt(core.scm_list2, treatment.identifier, controls.identifier, times.dep, times.pred2)
scm.hun2

# Plotting SCM Results
ggplot(scm.hun2, type = "comparison", 2009)
ggplot(scm.hun2, type = "gaps")
plot(scm.hun2, "unemployment", type = ("comparison"), 2009, zero.line = TRUE, ylab = "Unemployment rate", xlab = "Year",
     main = "Hungary", sub = "")





























# ## Results are too unprecise: Trying to reduce pre-event period and variables:

col_order.scm2 <- c("Country",
                   "year",
                   "population",
                   "fstgdp",
                   "rgdp",
                   "rgdppc",
                   "consumption",
                   "inflation",
                   "debtgdp",
                   "tariffs",
                   "kofglobal",
                   "tradegdp",
                   "labor",
                   "unemployment",
                   "institutions",
                   "judicial",
                   "media")


core.scm2 <- core.imp.cart[, col_order.scm2]

# Cut data along time dimension
core.scm2$year <- as.character(core.scm2$year)
core.scm2$year <- as.numeric(core.scm2$year)
core.scm2 <- subset(core.scm2, year >= "1960" & year <="2021")
core.scm2$year <- as.factor(core.scm2$year)

str(core.scm2)
summary(core.scm2)
sum(is.na(core.scm2))


times.dep2 <- cbind("unemployment" = c(1960,2009))
times.pred2 <- cbind("population"= c(1960,2009),
                    "fstgdp"= c(1960,2009),
                    "rgdp"= c(1960,2009),
                    "rgdppc"= c(1960,2009),
                    "consumption"= c(1960,2009),
                    "inflation"= c(1960,2009),
                    "debtgdp"= c(1960,2009),
                    "tariffs"= c(1960,2009),
                    "kofglobal"= c(1960,2009),
                    "tradegdp"= c(1960,2009),
                    "labor"= c(1960,2009),
                    "unemployment"= c(1960,2009),
                    "institutions"= c(1960,2009),
                    "judicial"= c(1960,2009),
                    "media"= c(1960,2009))


# Create list from data to have correct format
core.scm_list2 <- listFromLong(core.scm2, unit.variable = "Country", time.variable = "year")

# Apply the SCM Method
scm.hun2 <- mscmt(core.scm_list2, treatment.identifier, controls.identifier, times.dep2, times.pred2)
  
  
# Plotting SCM Results
ggplot(scm.hun2, type = "comparison")
ggplot(scm.hun2, type = "gaps")

plot(scm.hun2, "unemployment", type = ("comparison"), 2009, zero.line = TRUE, ylab = "Unemployment rate", xlab = "Year",
     main = "Hungary", sub = "")


print(scm.hun2)

compare(scm.hun2)

improveSynth(scm.hun2, core.scm2)
  



### 3. Bulgaria
treatment.identifier3 <- c("Poland")

# Check which variables have lowest number of imputation (NAs in core).
check <- subset(core, year >= "1990" & year <="2021") 
summary(check)

col_order.scm3 <- c("Country",
                    "year",
                    "population",
                    "gdp",
                    "gdp.gr",
                    "rgdppc",
                    "consumption",
                    "inflation",
                    "tariffs",
                    "kofglobal",
                    "unemployment",
                    "institutions",
                    "judicial",
                    "media")

core.scm3 <- core.imp.cart[, col_order.scm3]

# Cut data along time dimension
core.scm3$year <- as.character(core.scm3$year)
core.scm3$year <- as.numeric(core.scm3$year)
core.scm3 <- subset(core.scm3, year >= "1990" & year <="2021")
core.scm3$year <- as.factor(core.scm3$year)

core.scm3 <- pdata.frame(core.scm3, index = c("Country", "year"))

str(core.scm3)
summary(core.scm3)
sum(is.na(core.scm3))


times.dep3 <- cbind("unemployment" = c(1990,2005))
times.pred3 <- cbind("population"= c(1990,2005),
                     "gdp"= c(1990,2005),
                     "gdp.gr"= c(1990,2005),
                     "rdgppc" = c(1990,2005),
                     "consumption"= c(1990,2005),
                     "inflation"= c(1990,2005),
                     "tariffs"= c(1990,2005),
                     "kofglobal"= c(1990,2005),
                     "unemployment"= c(1990,2005),
                     "institutions"= c(1990,2005),
                     "judicial"= c(1990,2005),
                     "media"= c(1990,2005))

# Delete populist from core.scm
core.scm$populist <- NULL

# Create list from data to have correct format
core.scm_list.pol <- listFromLong(core.scm3, unit.variable = "Country", time.variable = "year")

# Apply the SCM Method
scm.pol <- mscmt(core.scm_list.pol, treatment.identifier3, controls.identifier, times.dep3, times.pred3)

# Plotting SCM Results
ggplot(scm.bgr, type = "comparison")
ggplot(scm.bgr, type = "gaps")

plot(scm.tur3, "unemployment", type = ("comparison"), 2009, zero.line = TRUE, ylab = "Unemployment rate", xlab = "Year",
     main = "Turkiye", sub = "")






















  
# Trying different optimization methods for Vp
# outer.optim = "cma_es"
scm2 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "cma_es" )
scm2
ggplot(scm2, type = "comparison")
ggplot(scm2, type = "gaps")
  
#outer.optim = "crs"
scm3 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "crs" )
scm3
ggplot(scm3, type = "comparison")
ggplot(scm3, type = "gaps")
  
  
  
scm4 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "DEopt" )
scm4
ggplot(scm4, type = "comparison")
ggplot(scm4, type = "gaps")
  
  
#DEoptim
scm5 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "DEoptim" )
scm5
ggplot(scm5, type = "comparison")
ggplot(scm5, type = "gaps")
  
#ga
scm6 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "ga" )
scm6
ggplot(scm6, type = "comparison")
ggplot(scm6, type = "gaps")
  
#genoud
scm7 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "genoud" )
scm7
ggplot(scm7, type = "comparison")
ggplot(scm7, type = "gaps")
  
  
#GenSA
scm8 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "GenSA" )
scm8
ggplot(scm8, type = "comparison")
ggplot(scm8, type = "gaps")
  
#hydroPSO
scm9 <- mscmt(data_mscmt_imputed_list, treatment.identifier, controls.identifier, times.dep, times.pred, outer.optim = "soma" )
scm9
ggplot(scm9, type = "comparison")
ggplot(scm9, type = "gaps")

  
  
  
# Notes
  
# Maybe they already used the panel regressions coefficients as weighty, since they are default in Synth package for Stata ()
  
# Figure out how to manually set the outer.optim method to panel regression or manually define Vp
  
# If that's not possible, use panel regressions for 5 years aftermath of populist event to show short- to mid-term effect of populism on the economy.
# Argue that causality can be assumed since SCM values show performance of economies (probably very sloppy)
  
# Check and do regressions for gini-index and labor share
  
# Do I use unsignificant coefficients for the Vp vector? -> Check in Paper
  
# Do I need additional data before 1991 to get enough pre-event data?
# I need the coefficients for the pre-event data (and for each country respectively?) not the overall coefficients!
  
# Do regressions with appropriate controls to get respective importance of each control variable for Vp (coefficients)
  
# Execute SCM for unemployment, gini and labor share for 1991 - 2021

# How to compare all rightpop countries? create representative country that has means a








# ARCHIVE & APPENDIX ####


# 3. Panel Regressions (No Time Lag)

##### 3.1 GDP Growth Rate ~ Populist 
# gdp.gr ~ populist
gdp.gr_populist <- plm(gdp.gr ~ populist,
                     data = data,
                     model = "within")
stargazer(gdp.gr_populist , type = 'text')

# gdp.gr ~ rightpop
gdp.gr_rightpop <- plm(gdp.gr ~ rightpop,
                       data = data,
                       model = "within")
stargazer(gdp.gr_rightpop , type = 'text')

# gdp.gr ~ leftpop
gdp.gr_leftpop <- plm(gdp.gr ~ leftpop,
                       data = data,
                       model = "within")
stargazer(gdp.gr_leftpop , type = 'text')


##### 3.2 Gini Index ~ Populist
# gdp.gr ~ populist
gini_populist <- plm(gini ~ populist,
                       data = data,
                       model = "within")
stargazer(gini_populist , type = 'text')

# gdp.gr ~ rightpop
gini_rightpop <- plm(gini ~ rightpop,
                       data = data,
                       model = "within")
stargazer(gini_rightpop , type = 'text')

# gdp.gr ~ leftpop
gini_leftpop <- plm(gini ~ leftpop,
                      data = data,
                      model = "within")
stargazer(gini_leftpop , type = 'text')



##### 3.3 Labor Share ~ Populist 
# gdp.gr ~ populist
labor_populist <- plm(labor ~ populist,
                     data = data,
                     model = "within")
stargazer(labor_populist , type = 'text')

# gdp.gr ~ rightpop
labor_rightpop <- plm(labor ~ rightpop,
                     data = data,
                     model = "within")
stargazer(labor_rightpop , type = 'text')

# gdp.gr ~ leftpop
labor_leftpop <- plm(labor ~ leftpop,
                    data = data,
                    model = "within")
stargazer(labor_leftpop , type = 'text')



##### 3.4 Unemployment ~ Populist 
# unemployment ~ populist
unemployment_populist <- plm(unemployment ~ populist,
                       data = data,
                       model = "within")
stargazer(unemployment_populist , type = 'text')

# unemployment ~ rightpop
unemployment_rightpop <- plm(unemployment ~ rightpop,
                             data = data,
                             model = "within")
stargazer(unemployment_rightpop , type = 'text')


# unemployment ~ leftpop
unemployment_leftpop <- plm(unemployment ~ leftpop,
                             data = data,
                             model = "within")
stargazer(unemployment_leftpop , type = 'text')


##### 3.5 FDI ~ Populist 
# fdi.gr ~ populist
fdi.gr_populist <- plm(fdi.gr ~ populist,
                             data = data,
                             model = "within")
stargazer(fdi.gr_populist , type = 'text')

# fdi.gr ~ rightpop
fdi.gr_rightpop <- plm(fdi.gr ~ rightpop,
                       data = data,
                       model = "within")
stargazer(fdi.gr_rightpop , type = 'text')


# fdi.gr ~ leftpop
fdi.gr_leftpop<- plm(fdi.gr ~ leftpop,
                       data = data,
                       model = "within")
stargazer(fdi.gr_leftpop, type = 'text')


#### 4. Panel Regressions (Time Lag) 
#Regressions for subsample with populist dummy = 1 for all 15 years after start of populist event (manually) 
#write_xlsx(data, path = "C:/Users/marce/OneDrive - UT Cloud/02_Semester - Tübingen (SoSe23)/01_European Economic Integration/03_model_extension/data_15.xlsx")
data_popevent15 <- readxl::read_xlsx("data_15.xlsx")

# Update (non)populist variable to new rightpop & leftpop dummies
data_popevent15$populist <- ifelse(data_popevent15$rightpop == 1 | data_popevent15$leftpop == 1, 1, 0)
data_popevent15$nonpopulist <- ifelse(data_popevent15$rightpop == 1 | data_popevent15$leftpop == 1, 0, 1)

# Update data with new populist15 dummies
data$populist15 <- data_popevent15$populist
data$rightpop15 <- data_popevent15$rightpop
data$leftpop15 <- data_popevent15$leftpop
data$nonpopulist15 <- data_popevent15$nonpopulist


###### 4.1 GDP Growth Rate ~ Populist
# gdp.gr ~ populist15
gdp.gr_populist15 <- plm(gdp.gr ~ populist15,
                       data = data,
                       model = "within")
stargazer(gdp.gr_populist15, type = 'text')

# gdp.gr ~ rightpop15
gdp.gr_rightpop15 <- plm(gdp.gr ~ rightpop15,
                       data = data,
                       model = "within")
stargazer(gdp.gr_rightpop15, type = 'text')

# gdp.gr ~ leftpop15
gdp.gr_leftpop15 <- plm(gdp.gr ~ leftpop15,
                      data = data,
                      model = "within")
stargazer(gdp.gr_leftpop15, type = 'text')



##### 4.2 Gini Index ~ Populist
# gini ~ populist15
gini_populist15 <- plm(gini ~ populist15,
                     data = data,
                     model = "within")
stargazer(gini_populist15 , type = 'text')

# gini ~ rightpop15
gini_rightpop15 <- plm(gini ~ rightpop15,
                     data = data,
                     model = "within")
stargazer(gini_rightpop15 , type = 'text')


# gini ~ leftpop15
gini_leftpop15 <- plm(gini ~ leftpop15,
                    data = data,
                    model = "within")
stargazer(gini_leftpop15, type = 'text')



##### 4.3 Labor Share ~ Populist
# labor ~ populist15
labor_populist15 <- plm(labor ~ populist15,
                      data = data,
                      model = "within")
stargazer(labor_populist15, type = 'text')

# labor ~ rightpop15
labor_rightpop15 <- plm(labor ~ rightpop15,
                      data = data,
                      model = "within")
stargazer(labor_rightpop15, type = 'text')

# labor ~ leftpop15
labor_leftpop15 <- plm(labor ~ leftpop15,
                     data = data,
                     model = "within")
stargazer(labor_leftpop15, type = 'text')



###### 4.2 Unemployment ~ Populist
# unemployment ~ populist15
unemployment_populist15 <- plm(unemployment ~ populist15,
                             data = data,
                             model = "within")
stargazer(unemployment_populist15 , type = 'text')

# unemployment ~ rightpop15
unemployment_rightpop15 <- plm(unemployment ~ rightpop15,
                             data = data,
                             model = "within")
stargazer(unemployment_rightpop15 , type = 'text')


# unemployment ~ leftpop15
unemployment_leftpop15 <- plm(unemployment ~ leftpop,
                            data = data,
                            model = "within")
stargazer(unemployment_leftpop15 , type = 'text')


# 5. Panel Regressions with Controls (No Time Lag

###### 5.1 Preparation and Tests

###### 5.1.1 Balancedness
punbalancedness(data)
# gamma    nu 
# 1         1 
# Balanced data set: The closer to 1, the more balanced is the panel data


###### 5.1.2 Outliers
# data_omit_pd_sd <- data_omit_pd
# data_omit_pd_sd$gdp <- data_omit_pd$gdp/1000000
# boxplot(data_omit_pd$gdp)
# summary(data_omit_pd$gdp)
# mean(data_omit_pd$gdp) + sd(data_omit_pd$gdp)
# n_obs <- sum(data_omit_pd$gdp > 3300361393533)
# data_omit_pd_sd <- subset(data_omit_pd_sd, gdp < 3300361393533)
# boxplot(data_omit_pd_sd$gdp)
# fixeff_gdp_sd <- plm(gdp ~ population,
#                   data = data_omit_pd_sd,
#                   model = "within")
# stargazer(fixeff_gdp_sd, type = 'text')
# summary(fixeff_gdp)


##### 5.1.3 Heteroskedasticity
#If the variance of the error terms in your model is not constant across different levels 
# of the predictors, it can cause large coefficients. In this case, you might want to use a robust regression 
# method or transform the dependent variable to address heteroskedasticity.


###### 5.1.4 Mulitcollinearity
# If vif > 5 (10), multicollinearity of respective expl. variable to high (rule of thumb)
# W do this by using method = pooling i.e. the coefficients remain the same over the time periods (not recognizing the panel structure of data)
unemployment_populistp <- plm(unemployment ~ gdp + population + labor + gini + inflation + fdi + populist,
                              data = data,
                              model = "pooling")

vif(unemployment_populistp)
# gdp population      labor       gini  inflation        fdi   populist 
# 2.005017   1.364401   1.331949   1.345172   1.157503   1.824632   1.093850
# Collinearity seems alright!


# Further test: correlation matrix
# Change data type of dummy to numeric
data$populist <- as.character(data$populist)
data$populist <- as.integer(data$populist)
data$rightpop <- as.character(data$rightpop)
data$rightpop <- as.integer(data$rightpop)
data$leftpop <- as.character(data$leftpop)
data$leftpop <- as.integer(data$leftpop)
data$populist15 <- as.character(data$populist15)
data$populist15 <- as.integer(data$populist15)
data$rightpop15 <- as.character(data$rightpop15)
data$rightpop15 <- as.integer(data$rightpop15)
data$leftpop15 <- as.character(data$leftpop15)
data$leftpop15 <- as.integer(data$leftpop15)

cormatrix <- cor(subset(data, 
                        select = c(4, 6, 7, 8, 9, 10, 13, 14, 15, 16, 18, 19, 20)), 
                 method =  "pearson")
cormatrix
# Same result: No stron multicollinearity

# Correlation Matrix of all variables for each country (individual) respectively
corrmatrix_indiv <- by(data[,c(4,6,7,8,9,10,13,14)], data$Country, cor)
corrmatrix_indiv


##### 5.1.5 !!!Large expl. Variables
# However, variables with large values such as gdp, population, fdi etc. will disturb panel regression:
unemployment_populist_fail <- plm(unemployment ~ gdp + population + labor + gini + inflation + fdi + populist,
                              data = data,
                              model = "within")
stargazer(unemployment_populist_fail, type = 'text')
# Error in solve.default(vcov(x)[names.coefs_wo_int, names.coefs_wo_int],  : 
# system is computationally singular: reciprocal condition number = 1.2527e-25

# Solution: Reduce units by division e.g. gdp/1000,0000,0000 or by taking logs (actual idea: practically using growth rates)


###### 5.2 Unemployment ~ populist + controls.gr
#unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + populist,
fixeff_unemployment_populist <- plm(unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + populist,
                                  data = data,
                                  model = "within")
stargazer(fixeff_unemployment_populist, type = 'text')

# __________________________________________ 
#          Dependent variable:    
# __________________________________________  
#                         unemployment        
# __________________________________________ 
#   gdp.gr                 -0.043***         
#                           (0.007)          
# 
# population.gr          -2.645***         
#                      (0.201)          
# 
# labor                   -0.054*          
#                        (0.028)          
# 
# gini                   0.268***          
#                           (0.054)          
# 
# inflation              -0.085***         
#                         (0.032)          
# 
# fdi.gr                 0.0001**          
#                        (0.00004)         
# 
# populist               -0.784***         
#                         (0.278)          
# 
# __________________________________________ 
# Observations              880            
# R2                       0.249           
# Adjusted R2              0.192           
# F Statistic     38.622*** (df = 7; 817)  
# __________________________________________ 
#   Note:         *p<0.1; **p<0.05; ***p<0.01




# Results (Vp for Unemployment SCM) for rightpop and leftpop doppelganger respectively:

# unemployment ~ rightpop + controls.gr
fixeff_unemployment_rightpop <- plm(unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + rightpop,
                                    data = data,
                                    model = "within")
stargazer(fixeff_unemployment_rightpop, type = 'text')

# __________________________________________ 
#                Dependent variable:    
# __________________________________________ 
#                        unemployment        
# __________________________________________ 
# gdp.gr                 -0.043***         
#                        (0.007)          
# 
# population.gr          -2.634***         
#                         (0.201)          
# 
# labor                   -0.051*          
#                        (0.028)          
# 
# gini                   0.273***          
#                       (0.054)          
# 
# inflation              -0.089***         
#                        (0.032)          
# 
# fdi.gr                 0.0001**          
#                       (0.00004)         
# 
# rightpop               -1.249***         
#                       (0.345)          
# 
# __________________________________________ 
#   Observations              880            
# R2                       0.253           
# Adjusted R2              0.197           
# F Statistic     39.591*** (df = 7; 817)  
# __________________________________________ 
#   Note:         *p<0.1; **p<0.05; ***p<0.01




# unemployment ~ leftpop + controls.gr
fixeff_unemployment_leftpop <- plm(unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + leftpop,
                                    data = data,
                                    model = "within")
stargazer(fixeff_unemployment_leftpop, type = 'text')

# __________________________________________ 
#   Dependent variable:    
# __________________________________________ 
#   unemployment        
# __________________________________________ 
# gdp.gr                 -0.042***         
#                        (0.007)          
# 
# population.gr          -2.626***         
#                              (0.202)          
# 
# labor                   -0.050*          
#                         (0.028)          
# 
# gini                   0.285***          
#                       (0.055)          
# 
# inflation              -0.077**          
#                        (0.032)          
# 
# fdi.gr                 0.0001**          
#                       (0.00004)         
# 
# leftpop                  0.061           
#                         (0.462)          
# _________________________________________ 
# Observations              880            
# R2                       0.241           
# Adjusted R2              0.184           
# F Statistic       37.131*** (df = 7; 817)  
# __________________________________________ 
# Note:         *p<0.1; **p<0.05; ***p<0.01


###### 5.3 Log(gini) ~ populist + log(controls
fixeff_loggini <- plm(log(gini) ~ log(gdp) + log(population) + log(labor) + log(unemployment) + log(inflation) + log(fdi) + populist,
                              data = data,
                              model = "within")
stargazer(fixeff_loggini , type = 'text')


fixeff_loggini_rightpop <- plm(log(gini) ~ log(gdp) + log(population) + log(labor) + log(unemployment) 
                      + log(inflation) + log(fdi) + rightpop,
                      data = data,
                      model = "within")
stargazer(fixeff_loggini_rightpop, type = 'text')


fixeff_loggini_leftpop <- plm(log(gini) ~ log(gdp) + log(population) + log(labor) + log(unemployment) 
                               + log(inflation) + log(fdi) + leftpop,
                               data = data,
                               model = "within")
stargazer(fixeff_loggini_leftpop, type = 'text')



###### 5.4 Log(labor) ~ populist + log(controls
fixeff_loglabor <- plm(log(labor) ~ log(gdp) + log(population) + log(gini) + log(unemployment) + log(inflation) + log(fdi) + populist,
                      data = data,
                      model = "within")
stargazer(fixeff_loglabor, type = 'text')


# Log(labor) ~ rightpop + log(controls)
fixeff_loglabor_rightpop <- plm(log(labor) ~ log(gdp) + log(population) + log(gini) 
                       + log(unemployment) + log(inflation) + log(fdi) + rightpop,
                       data = data,
                       model = "within")
stargazer(fixeff_loglabor_rightpop, type = 'text')



# Log(labor) ~ leftpop + log(controls)
fixeff_loglabor_leftpop <- plm(log(labor) ~ log(gdp) + log(population) + log(gini) 
                                + log(unemployment) + log(inflation) + log(fdi) + leftpop,
                                data = data,
                                model = "within")
stargazer(fixeff_loglabor_leftpop, type = 'text')




#### 6. Panel Regressions with Controls (Time Lag
# Relevant?: Do I need this for the Vp, because for doppelganger I especially need pre-event data (before the populist event) i.e.,
# all data before rightpop / leftpop dummy takes value = 1 for the first time to fit synthetic doppelganger

###### 6.1 unemployment ~ populist15 + controls.gr
#unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + populist
fixeff_unemployment_populist15 <- plm(unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + populist15,
                                    data = data,
                                    model = "within")
stargazer(fixeff_unemployment_populist15, type = 'text')


# unemployment ~ rightpop15 + controls.gr
fixeff_unemployment_rightpop15 <- plm(unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + rightpop15,
                                    data = data,
                                    model = "within")
stargazer(fixeff_unemployment_rightpop15, type = 'text')



# unemployment ~ leftpop15 + controls.gr
fixeff_unemployment_leftpop15 <- plm(unemployment ~ gdp.gr + population.gr + labor + gini + inflation + fdi.gr + leftpop15,
                                      data = data,
                                      model = "within")
stargazer(fixeff_unemployment_leftpop15, type = 'text')




###### 5.3 Log(gini) ~ populist + log(controls
fixeff_loggini15 <- plm(log(gini) ~ log(gdp) + log(population) + log(labor) + log(unemployment) + log(inflation) + log(fdi) + populist,
                      data = data,
                      model = "within")
stargazer(fixeff_loggini15 , type = 'text')


###### 5.4 Log(labor) ~ populist + log(controls
fixeff_loglabor15 <- plm(log(labor) ~ log(gdp) + log(population) + log(gini) + log(unemployment) + log(inflation) + log(fdi) + populist,
                       data = data,
                       model = "within")
stargazer(fixeff_loglabor15, type = 'text')






























# Notes, Codes and Methods ####

# # Cutting along time dimension: Subsample with specific years
# data_omit_pd_gr$year <- as.factor(data_omit_pd_gr$year )
# data_omit_pd_gr_15 <- subset(data_omit_pd_gr, year >= "1991" & year <="2007")
# fixeff_gdp_gr_15 <- plm(gdp_gr ~ populist,
#                      data = data_omit_pd_gr_15,
#                      model = "within")
# 
# stargazer(fixeff_gdp_gr_15, type = 'text')
# summary(fixeff_gdp)

# Adding controls by constructing an combined control variable
# Select control variables
#controls <- dplyr::select(data, )
#controls <- select(data = data, population, gdp, inflation, fdi)
controls_omit <- data_omit
controls_omit$Country <- NULL
controls_omit$year <- NULL
controls_omit$populist <- NULL
controls_omit$nonpopulist <- NULL
controls_omit$unemployment <- NULL

#Omit NA !!! TAKE CARE WHEN USING: WHAT IMPICATIONS? !!!
#controls_omit <- na.omit(controls)
# Run principal component analysis
pca <- principal(controls_omit, nfactors = 1)

# Extract composite variable
control_var <- data.frame(pca$scores[,1])
control_var_sq <- (control_var)^2

# Rename composite variable
colnames(control_var) <- "control_variables"
colnames(control_var_sq) <- "control_variables"

# Replace controls in data with composite control variable
data_controls <- cbind(data_control, control_var)
data_controls_sq <- cbind(data_control, control_var_sq)




# SCM APPLICATION 

# Define new dataset with only necesarry data
sum(is.na(data))
#[1] 1467
data_mscmt <- dplyr::select(data, Country, year, gdp, population, unemployment, gini, fdi, populist)
sum(is.na(data_mscmt))
#[1] 155

# ERROR HAPPENS HERE!
#delete all rows with NAs. Problem: We will later have NAs, if the periods in times.dep and times.pred 
# include years for which the data has NAs, meaning, all the needed data in the chosen time period must not have any NAs
data_mscmt_omit <- na.omit(data_mscmt)
sum(is.na(data_mscmt_omit))


# Define vector with treated and donor (non-treated) countries
treated <- dplyr::filter(data_mscmt_omit, populist == 1)
treated$Country <- as.character(treated$Country)
str(treated)
treated.index <- unique(treated$Country)
print(treated.index)
is.vector(treated.index)

# Define controls.identifier containing all donor countries (non-treated)
controls.identifier <- dplyr::setdiff(data_mscmt_omit$Country, treated.index)
str(controls.identifier)
is.vector(controls.identifier)
print(controls.identifier)


# Define treatment.identifier containing the treated country under investigation
treatment.identifier <- "Hungary"


# Defining time periods for optimization of dependent variable and covariates
# Hungary: Populist event takes place in 20010, thus optimization period: 1991-2009
# HERE WE USE THE ABSOLUTE VALUES AND NOT GROWTH RATES!
times.dep <- cbind("unemployment" = c(1991,2009))
times.pred <- cbind("gdp" = c(1991,2009),
                    "population" = c(1991,2009),
                    "gini" = c(1991,2009),
                    "fdi" = c(1991,2009))
is.matrix(times.dep)
is.matrix(times.pred)
# Change data format and check if its a matrix
# times.dep <- as.matrix(times.dep)
# times.pred <- as.matrix(times.pred)

# WHAT ABOUT THIS STEP? -> Not necessary
agg.fns <- rep("mean", ncol(times.pred))  


# Delete the column populist for the next step (populiost variable not needed)
data_mscmt_omit$populist <- NULL

# Create list from data to have necessary format
data_mscmt_omit_list <- listFromLong(data_mscmt_omit, unit.variable = "Country", time.variable = "year")
print(data_mscmt_omit_list)
# WE HAVE NAs AS DESRCIBED ABOVE!

scm <- mscmt(data_mscmt_omit_list, treatment.identifier, controls.identifier, times.dep, times.pred, seed=1 )



namevector <- c("1870",
                "1871",
                "1872",
                "1873",
                "1874",
                "1875",
                "1876",
                "1877",
                "1878",
                "1879",
                "1880",
                "1881",
                "1882",
                "1883",
                "1884",
                "1885",
                "1886",
                "1887",
                "1888",
                "1889",
                "1890",
                "1891",
                "1892",
                "1893",
                "1894",
                "1895",
                "1896",
                "1897",
                "1898",
                "1899",
                "1900",
                "1901",
                "1902",
                "1903",
                "1904",
                "1905",
                "1906",
                "1907",
                "1908",
                "1909",
                "1910",
                "1911",
                "1912",
                "1913",
                "1914",
                "1915",
                "1916",
                "1917",
                "1918",
                "1919",
                "1920",
                "1921",
                "1922",
                "1923",
                "1924",
                "1925",
                "1926",
                "1927",
                "1928",
                "1929",
                "1930",
                "1931",
                "1932",
                "1933",
                "1934",
                "1935",
                "1936",
                "1937",
                "1938",
                "1939",
                "1940",
                "1941",
                "1942",
                "1943",
                "1944",
                "1945",
                "1946",
                "1947",
                "1948",
                "1949",
                "1950",
                "1951",
                "1952",
                "1953",
                "1954",
                "1955",
                "1956",
                "1957",
                "1958",
                "1959",
                "1960",
                "1961",
                "1962",
                "1963",
                "1964",
                "1965",
                "1966",
                "1967",
                "1968",
                "1969",
                "1970",
                "1971",
                "1972",
                "1973",
                "1974",
                "1975",
                "1976",
                "1977",
                "1978",
                "1979",
                "1980",
                "1981",
                "1982",
                "1983",
                "1984",
                "1985",
                "1986",
                "1987",
                "1988",
                "1989",
                "1990")

unemployment[,namevector] <- NA



# # Create new Variables growth rates
# gdp$gdp.gr <- ((gdp$gdp - lag(gdp$gdp))/lag(gdp$gdp))*100
# inflation$inflation.gr <- ((inflation$inflation - lag(inflation$inflation))/lag(inflation$inflation))*100
# population$population.gr <- ((population$population - lag(population$population))/lag(population$population))*100
# fdi$fdi.gr <- ((fdi$fdi - lag(fdi$fdi))/lag(fdi$fdi))*100


# Define core.plm from  1960 - 2021 to reduce NAs
sum(is.na(core))
#[1] 27024

core.plm <- subset(core, year >= "1960" & year <="2021" )
core.plm <- pdata.frame(core.plm, index = c("Country","year"))
str(core.plm)

