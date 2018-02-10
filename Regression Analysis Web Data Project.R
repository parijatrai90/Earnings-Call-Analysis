########## READING DATA ##########
mf <- read.csv("All_Mutual_Funds_Final.csv", header = T)  # Mutual Funds Data
sp <- read.csv("S&P500.csv", header = T)                   # S&P500 Data
ss <- read.csv("Senti_Scores_Final.csv", header = T)      # Sentiment Score Data
##################################

########## PRE-PROCESSING OF DATA ##########
## 1. Cleaning Data for unwanted variable

ss["X"]<- NULL
ss["Unnamed..0"]<- NULL
colnames(ss)[1] <- "Company"

sp["X"]<- NULL

mf["X"]<- NULL

## 2. Since the effect of sentiment score for a quarter will be reflected in stock price/quantity in the next quarter.
## Hence we shift the quarter and the year by 1 to be able to correctly run the regression 

ss$Quarter <- ifelse(ss$Quarter==4, 1, ss$Quarter+1)
ss$Year <- ifelse(ss$Quarter==1, ss$Year+1,ss$Year)

## 3. Converting variables which should be factor

mf$Year <- as.factor(mf$Year)
mf$Quarter <- as.factor(mf$Quarter)

sp$Year <- as.factor(sp$Year)
sp$Quarter <- as.factor(sp$Quarter)

ss$Year <- as.factor(ss$Year)
ss$Quarter <- as.factor(ss$Quarter)
############################################

########## MERGING THE REQUIRED DATASETS ##########

SP_SS <- merge(x = sp, y = ss, by = c("Company", "Year","Quarter")) # Merged S&P and Sentiment Score Data
MF_SS <- merge(x = mf, y = ss, by = c("Company", "Year","Quarter")) # Merged Mutual Funds and Sentiment Score Data

###################################################

########## REGRESSION ANALYSIS ############
## 1. Conducting regression in merged data of Stock Price and Sentiment Score

regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score, data = SP_SS)
summary(regressor)

regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score*Quarter, data = SP_SS)
summary(regressor)

## 2. Conducting regression in merged data of Stock Price and Sentiment Score (filtered for Companies)

american <- subset(SP_SS, Company == "american")
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score, data = american)
summary(regressor)
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score*Quarter, data = american)
summary(regressor)

southwest <- subset(SP_SS, Company == "southwest")
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score, data = southwest)
summary(regressor)
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score*Quarter, data = southwest)
summary(regressor)

delta <- subset(SP_SS, Company == "delta")
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score, data = delta)
summary(regressor)
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score*Quarter, data = delta)
summary(regressor)

apple <- subset(SP_SS, Company == "apple")
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score, data = apple)
summary(regressor)
regressor <- lm(formula = Closing.Stock.Price ~ Sentiment_Score*Quarter, data = apple)
summary(regressor)

## 3. Conducting regression in merged data of Mutual Funds and Sentiment Score
regressor <- lm(formula = Quantity ~ Sentiment_Score, data = MF_SS)
summary(regressor)

regressor <- lm(formula = Quantity ~  Sentiment_Score*Quarter, data = MF_SS)
summary(regressor)

## 4. Conducting regression in merged data of Mutual Funds and Sentiment Score (filtered for Companies)
american2 <- subset(MF_SS, Company == "american")
regressor <- lm(formula = Quantity ~ Sentiment_Score, data = american2)
summary(regressor)
regressor <- lm(formula = Quantity ~ Sentiment_Score*Quarter, data = american2)
summary(regressor)

southwest2 <- subset(MF_SS, Company == "southwest")
regressor <- lm(formula = Quantity ~ Sentiment_Score, data = southwest2)
summary(regressor)
regressor <- lm(formula = Quantity ~ Sentiment_Score*Quarter, data = southwest2)
summary(regressor)

delta2 <- subset(MF_SS, Company == "delta")
regressor <- lm(formula = Quantity ~ Sentiment_Score, data = delta2)
summary(regressor)
regressor <- lm(formula = Quantity ~ Sentiment_Score*Quarter, data = delta2)
summary(regressor)

apple2 <- subset(MF_SS, Company == "apple")
regressor <- lm(formula = Quantity ~ Sentiment_Score, data = apple2)
summary(regressor)
regressor <- lm(formula = Quantity ~ Sentiment_Score*Quarter, data = apple2)
summary(regressor)
###########################################
