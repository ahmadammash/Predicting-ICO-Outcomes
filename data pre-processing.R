library(dplyr)
library(VIM)
library(corrgram)
library(mice)
library(stringr)
library(stats)
library(tidyverse)
library(ggplot2)
data <- read.csv('LUBS5990M_courseworkData_202223.csv')

#Viewing the first few rows and structure
head(data)
str(data)

#missing values plot
library(corrplot)
sum(complete.cases(data))
sum(!complete.cases(data))
aggr(data,numbers = T, prop = F,cex.lab = 1.5,cex.axis=1.1,combined=T,cex.numbers=1.1)


#Drop the ID column
data <- data[,-1]

#Understanding the dataset

#Dimensions
dim(data)
#the dataset has 2,767 rows and 15 columns
#data on 2,767 ICOs with 15 features

#Structure
str(data)
#success - CHR whether the ICO is a success - N or Y
#brandSlogan - CHR text showing slogan of the brand
#hasVideo - INT 1 or 0 whether a video was provided on the campaign page or not
#rating - NUM rating by investment experts of the quality of the fundraising project 1 to 5 (highest)
#priceUSD - NUM the price of the blockchain coin issued at the time of the ICO
#countryRegion - CHR where the team/company is
#start and end date - CHR start and end date of the fundraising campaign
#team size - INT size of the team from the fundraising project
#hasGitHub - INT 1 or 0 whether the github page was shared with investors
#hasReddit - INT 1 or 0 whether the reddit page was shared with investors
#platform - CHR platform used
#coinNum - number of blockchain coins issued at the time of the ICO
#minInvestment - INT indicating if a minimal investment was set
#distributedPercentage - NUM indicating what percentage of coins was distributed to investors vs kept


# CHR - SUCCESS COLUMN ------------------------------------------------------------------------
#Summary of the success column (the target feature)
table(data$success)
#transofrming it to a factor
data$success <- as.factor(data$success)
str(data$success)
#1739 ICOs did not reach the funding target
#1028 ICOs reached their funding target


# INT - HAS VIDEO COLUMN ----------------------------------------------------------------------
#Summary of the has video column
table(data$hasVideo)
#758 did not share a video on their campaign page
#2009 shared a video on their campaign page
#create visual for that


# INT - HAS GITHUB COLUMN ---------------------------------------------------------------------
#Summary of the has github column
table(data$hasGithub)
#1168 did not share their github page on the campaign page
#1599 shared their github page on the campaign page


# INT - HAS REDDIT COLUMN ---------------------------------------------------------------------
#Summary of the has reddit column
table(data$hasReddit)
#1016 did not share their reddit page on the campaign page
#1751 shared their reddit page on the campaign page



# INT - MIN INVESTMENT COLUMN -----------------------------------------------------------------
#Summary of the minimum investment column
table(data$minInvestment)
#1513 did not set a minimum investment
#1254 set a minimum investment


# CHR - COUNTRY/REGION COLUMN -----------------------------------------------------------------
#Summary of the country/region column
table(data$countryRegion)

#Viewing the unique values
unique(data$countryRegion)

#converting the country region values to upper case for error correction
data$countryRegion <- toupper(data$countryRegion)

#viewing the unique values again
unique(data$countryRegion)

#summary of the column
table(data$countryRegion)

#replacing the empty values with an NA
data[data == ""] <- "UNKNOWN"

#correcting the errors in Mexico value
data$countryRegion <- str_replace(data$countryRegion,"MÉXICO","MEXICO")

#viewing the unique values
unique(data$countryRegion)

#converting all to upper case again
data$countryRegion <- toupper(data$countryRegion)

#sorting the column of unique values
sort(unique(data$countryRegion))

#correcting the error in curacao
data$countryRegion <- str_replace(data$countryRegion,"CURAÇAO","CURACAO")

#correcting error in south africa
data$countryRegion <- str_replace(data$countryRegion,"SOUTHAFRICA","SOUTH AFRICA")

#sorting the column of unique values
data$countryRegion <- toupper(data$countryRegion)
unique_country_list <- as.data.frame(sort(unique(data$countryRegion)))

#writing the unique country list into excel
library(writexl)
write_xlsx(unique_country_list,path = "C:\\Users\\ahmad\\OneDrive\\Desktop\\Ahmad\\LEEDS BA & DS\\S2 - 5990M - Machine Learning in Practice\\Coursework\\unique_country_list.xlsx")

sum(is.na(data$countryRegion))
sum(is.na(data))



# CHR - START/END DATE COLUMNS ----------------------------------------------------------------
#Summary of the start/end date columns

#Converting the start date to a date
data$startDate <- as.Date(data$startDate,"%d/%m/%Y")
str(data$startDate)
sum(is.na(data$startDate))
min(data$startDate)
max(data$startDate)

#Converting the end date to a date
data$endDate <- as.Date(data$endDate,"%d/%m/%Y")
str(data$endDate)
sum(is.na(data$endDate))
min(data$endDate)
max(data$endDate)

#finding the duration of the ICOs in days
#adding the duration column to the dataset
head(data)
indices <- which(data$startDate > data$endDate)
starts <- data[indices,]$startDate
ends <- data[indices,]$endDate
data[indices,]$startDate <- ends
data[indices,]$endDate <- starts
duration <- data$endDate - data$startDate
data <- data %>% mutate(duration)
data$duration <- as.numeric(data$duration)
summary(data$duration)
plot(data$duration,main = "ICO Duration Values (Before Processing)",xlab = "Row Number",ylab = "Duration (days)")
summary(data$duration)
data <- data %>% filter(duration < 500)
# cutoff_duration <- 3.5*sd(data$duration)
# data <- data %>% filter(duration <= mean(data$duration) + cutoff_duration)
plot(data$duration,main = "ICO Duration Values (After Processing)",xlab = "Row Number",ylab = "Duration (days)")
dim(data)
summary(data$duration)

#extract the year, month, and day values from the date columns
data$startYear <- as.numeric(format(data$startDate,'%Y'))
data$endYear <- as.numeric(format(data$endDate,'%Y'))
data$startMonth <- as.numeric(format(data$startDate,'%m'))
data$endMonth <- as.numeric(format(data$endDate,'%m'))
data$startDay <- as.numeric(format(data$startDate,'%d'))
data$endDay <- as.numeric(format(data$endDate,'%d'))

# CHR - PLATFORM COLUMN -----------------------------------------------------------------------
#Summary of the platform column
table(data$platform)
unique(data$platform)
length(unique(data$platform))
#Stellar is the same as Stellar Protocol
#Separate blockchain the same as Separate Blockchain
#X11 blockchain the same as X11
#Ethereum, Waves??
#TRON same as Tron
#There is a "" value
#WAVES same as Waves
#Ethereum instead of Ether
#ERC20 under Ethereum platform


data$platform <- str_trim(data$platform)
data$platform <- toupper(data$platform)
unique(data$platform)
data$platform <- str_replace(data$platform,"ETHERERUM","ETHEREUM")
data$platform <- str_replace(data$platform,"X11 BLOCKCHAIN","X11")
unique(data$platform)
data$platform <- str_replace(data$platform,"ERC20","ETHEREUM")
data$platform <- str_replace(data$platform,"STELLAR PROTOCOL","STELLAR")
data$platform <- str_replace(data$platform,"ETHERUM","ETHEREUM")
data$platform <- str_replace(data$platform,"CRYPTONOTE-BASED BLOCKCHAIN","CRYPTONOTE")
data$platform <- str_replace(data$platform,"BTC","BITCOIN")
data$platform <- str_replace(data$platform,"ISL-","ISL")
data$platform <- str_replace(data$platform,"POS,POW","POW-POS")
data$platform <- gsub("BLOCKCHAIN","",data$platform)
data$platform <- gsub("PLATFORM","",data$platform)
data$platform[data$platform == "POS + POW"] <- "POW-POS"
data$platform[data$platform == "POW/POS"] <- "POW-POS"
sort(unique(data$platform))
data$platform <- str_trim(data$platform)
data$platform[data$platform == ""] <- "UNKNOWN"
data$platform[data$platform == "ETHEREUM, WAVES"] <- "ETHEREUM-WAVES"
data$platform[data$platform == "ETH"] <- "ETHEREUM"

sort(unique(data$platform))
length(unique(data$platform))


# NUM - PRICE USD COLUMN  ---------------------------------------------------------------------
#Summary of the price USD column
summary(data$priceUSD)
plot(data$priceUSD, ylab = "Price (USD)", xlab = "Row Number", 
     main = "ICO Cryptocurrency Prices (Before Processing)")
#180 nulls
#outliers to be removed
price_mean <- mean(data$priceUSD,na.rm = T)
price_SD <- sd(data$priceUSD,na.rm = T)
dim(data)
cut_off_price <- price_mean+ price_SD*3
data <- filter(data,priceUSD <= cut_off_price | is.na(priceUSD))
summary(data$priceUSD)
dim(data)
price_SD_1 <- sd(data$priceUSD,na.rm = T)
price_mean_1 <- mean(data$priceUSD,na.rm = T)
cutt_off_price_1 <- price_mean_1 + price_SD_1*3
data <- filter(data,priceUSD <= cutt_off_price_1 | is.na(priceUSD))
dim(data)
summary(data$priceUSD)
price_SD_2 <- sd(data$priceUSD,na.rm = T)
price_mean_2 <- mean(data$priceUSD,na.rm = T)
cutt_off_price_2 <- price_mean_2 + price_SD_2*3
data <- filter(data,priceUSD <= cutt_off_price_2 | is.na(priceUSD))
dim(data)
summary(data$priceUSD)
price_SD_3 <- sd(data$priceUSD,na.rm = T)
price_mean_3 <- mean(data$priceUSD,na.rm = T)
cutt_off_price_3 <- price_mean_3 + price_SD_3*3
data <- filter(data,priceUSD <= cutt_off_price_3 | is.na(priceUSD))
summary(data$priceUSD)
dim(data)
data <- filter(data,priceUSD < 4 | is.na(priceUSD))
boxplot(data$priceUSD)
dim(data)
plot(data$priceUSD,ylab = "Price (USD)",xlab = "Row Number",
     main = "ICO Cryptocurrency Prices (After Processing)")
summary(data$priceUSD)

# NUM - RATING COLUMN -------------------------------------------------------------------------
#Summary of the rating column
summary(data$rating)
#no nulls
plot(data$rating,xlab = "Row Number",ylab = "ICO Rating (out of 5)",main = "Ratings of ICOs by Expert Investors")
#scatter plot looks normal - no serious outliers


# NUM - COIN NUM COLUMN -----------------------------------------------------------------------
#Summary of the coin number column
summary(data$coinNum)
#no nulls
#outlier of 2.262e16 to be removed
plot(data$coinNum,xlab = "Row Number",ylab = "Number of Coins",
     main = "Number of ICO Coins Issued (Before Processing)")

dim(data)
sd_coinNum_1 <- sd(data$coinNum)
cuttoff_coinNum_1 <- 3*sd_coinNum_1 + mean(data$coinNum,na.rm = T)
data <- filter(data,coinNum <= cuttoff_coinNum_1)
dim(data)
summary(data$coinNum)

sd_coinNum_2 <- sd(data$coinNum)
cuttoff_coinNum_2 <- 3*sd_coinNum_2 + mean(data$coinNum,na.rm = T)
data <- filter(data,coinNum <= cuttoff_coinNum_2)
dim(data)
summary(data$coinNum)

sd_coinNum_3 <- sd(data$coinNum)
cuttoff_coinNum_3 <- 3*sd_coinNum_3 + mean(data$coinNum,na.rm = T)
data <- filter(data,coinNum <= cuttoff_coinNum_3)
dim(data)
summary(data$coinNum)
data <- filter(data,data$coinNum <= 1e10)
summary(data$coinNum)
plot(data$coinNum,ylab="Number of Coins",xlab = "Row Number",
     main = "Number of ICO Coins Issued (After Processing)")
dim(data)
plot(data$coinNum,ylab = "coinNum")
summary(data$coinNum)

# INT - TEAM SIZE COLUMN ----------------------------------------------------------------------
#Summary of the team size column
summary(data$teamSize)
plot(data$teamSize,ylab="Team Size",xlab = "Row Number",
     main = "ICO Fundraising Team Sizes (Before Processing)")
#147 nulls
dim(data)
plot(data$teamSize)
#there are some big teams - 3 or 4
data <- filter(data, data$teamSize < 50 | is.na(data$teamSize))
plot(data$teamSize,ylab="Team Size",xlab = "Row Number",
     main = "ICO Fundraising Team Sizes (After Processing)")
summary(data$teamSize)



# NUM - DISTRIBUTED PERCENTAGE COLUMN ---------------------------------------------------------
#Summary of the Distributed Percentage column
summary(data$distributedPercentage)
plot(data$distributedPercentage,ylab="Distributed Percentage",xlab = "Row Number",
     main = "Percentage of ICO Coins Distributed (Before Processing)")
#there are outliers - filtering out values > 1
dim(data[data$distributedPercentage>1,])
data <- data %>% filter(data$distributedPercentage <= 1)
plot(data$distributedPercentage,ylab="Percentage Distributed",xlab = "Row Number",
     main = "Percentage of ICO Coins Distributed (After Processing)")
dim(data)
summary(data$distributedPercentage)

hist(data$distributedPercentage,main = "Histogram of the Distributed Percentage Feature",
     xlab = "Distributed Percentage")

# CHR - BRAND SLOGAN COLUMN  ------------------------------------------------------------------
library(stringr)
data$Slogan_Length <- str_count(data$brandSlogan,"\\S+")
head(data$Slogan_Length)

# ADDING ICO-FRIENDLY COLUMN ------------------------------------------------------------------

ICO_friendly_countries <- c("SWITZERLAND","ESTONIA","SINGAPORE","RUSSIA","GIBRALTAR","HONG KONG",
                            "GERMANY","CAYMAN ISLANDS","ISRAEL")

data$is_most_friendly <- ifelse(data$countryRegion %in% ICO_friendly_countries, 1, 0)


# Add the is_USA Column ------------------------------------------------------------------------
data$is_USA <- ifelse(data$countryRegion=="USA",1,0)



# ADDING IS_ETHEREUM FEATURE ------------------------------------------------------------------
library(DescTools)
data$is_ethereum <- ifelse(data$platform %like% "%ETHEREUM%", 1, 0)


# RECAP OF THE DATA --------------------------------------------------------------------
dim(data)
unique(data$success)
unique(data$hasVideo)
unique(data$rating)
unique(data$priceUSD)
unique(data$countryRegion)
unique(data$teamSize)
unique(data$hasGithub)
unique(data$hasReddit)
unique(data$platform)
unique(data$minInvestment)
unique(data$distributedPercentage)


# CHECKING MISSING VALUES -----------------------------------------------------------------------
dim(data)
sum(is.na(data$priceUSD))
sum(is.na(data$teamSize))
sum(is.na(data$platform))
sort(unique(data$platform))
sum(is.na(data$platform))
sum(is.na(data))
str(data)
library(corrplot)
sum(complete.cases(data))
sum(!complete.cases(data))
#aggr(data,numbers = T, prop = F)

corrgram(data)

summary(data$priceUSD)
plot(data$priceUSD)

str(data)
dim(data)
sum(is.na(data))
library(gmodels)
CrossTable(x = data$success, y = c(data$hasVideo),chisq = T)
#42.7% of ICOs which have a video succeed
#24.3% of ICOs which do not have a video succeed
#substantial difference between success rate of ICOs which have and do not have a video
#chi squared test is significant at 1%

CrossTable(x = data$hasVideo,y = data$hasGithub,chisq = T)

CrossTable(x = data$success, y = data$hasGithub,chisq = T)
#28.8% of ICOs which do not have GitHub succeed
#44% of ICOs which have GitHub succeed
#chi squared test is significant at 1%


CrossTable(x = data$success, y = data$hasReddit,chisq = T)
#26.5% of ICOs which do not have reddit succeed
#44% of ICOs which have reddit succeed
#chi squared test is significant at 1%



CrossTable(x = data$success, y = data$minInvestment,chisq = T)
#34.9% of ICOs without a minimum investment requirement succeed
#41% of ICOs with a minimum investment requirement succeed
#chi squared test is significant at 1%


# 
# CrossTable(x = data$success, y = data$is_ethereum,chisq = T)
#35% of ICOs not on ethereum succeed
#38.1% of ICOs on ethereum succeed
#chi sq test is not significant



CrossTable(x = data$success, y = data$is_most_friendly,chisq = T)
#32.9% of ICOs in non-friendly countries succeed
#44.2% of ICOs in friendly countries succeed 
#chi sq test is significant at 1%


#Histogram for the Rating Feature
hist(data$rating, main = "Histogram of the Rating Feature",xlab = "Rating Values")
library(wordcloud)
library(RColorBrewer)
frequency_platform <- data %>% group_by(platform) %>% summarise(Frequency = n())
wordcloud(words = frequency_platform$platform,freq = frequency_platform$Frequency,
          scale = c(2,1),colors = brewer.pal(1, "Dark2"),max.words = 100)
frequency_platform <- frequency_platform %>% arrange(desc(Frequency))

# REMOVE UNUSED FEATURES ----------------------------------------------------------------------
data <- data %>% select(-c(brandSlogan, countryRegion, startDate, endDate, platform))
dim(data)
#21 features

corrgram(data)
