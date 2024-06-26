data <- read.csv('DA Technical Assessment Dataset.csv')
head(data)
colnames(data)
summary(data)
dim(data)
nobs <- nrow(data)

## price position
summary(data$price_position) # numeric value centered about 100
pp_na = sum(is.na(data$price_position))
pp_na/nobs # 12% missing values
head(subset(data, is.na(data$price)))
boxplot(data$price_position, main='price position', horizontal = T, cex=0.5) # narrow distribution, lots of outliers 
hist(data$price_position,100, main='price position', xlab='price position') # normally distributed?
qqnorm(data$price_position, main='normal QQ (price position) ')
qqline(data$price_position) # more outliers than expected for normally distributed data; center normally distributed, what about the outliers?


## price_position/price
plot(data$price, data$price_position, 
     cex=0.25, main='price Vs. price position', xlab='price', ylab='price position',
     xlim=c(0,125000)) # narrowing distribution 
cor.test(data$price, data$price_position) # price position is not correlated to price
q3 <-  quantile(data$price_position, 0.75,na.rm=TRUE)
q1 <- quantile(data$price_position, 0.25,na.rm=TRUE)
iqr <- IQR(data$price_position, na.rm=TRUE) 
high <- data$price_position > q3 + 1.5*iqr # outlier - outside of IQR ± 1.5*IQR
low <- data$price_position < q1 - 1.5*iqr
data$price_position_outlier <- high|low
sum(data$price_position_outlier, na.rm=T)
sum(high, na.rm=T)/nobs # 219 high outliers 
sum(low, na.rm=T)/nobs # 170 high outliers 
boxplot(data$price[which(low)], 
        data$price[which(!(low|high))], 
        data$price[which(high)],
        outline = F,
        xlab='price position', ylab='price', 
        names=c('low extreme (2%)', 'middle (96%)', 'high extreme (2%)'), 
        main = 'extreme price position Vs. price' )
wilcox.test(data$price[which(!(low|high))],data$price[which(high)]) # p-value = 0.6221
wilcox.test(data$price[which(!(low|high))],data$price[which(low)]) # p-value < 2.2e-16

## price/mileage
cor.test(data$mileage, data$price) # price and mileage are strongly negatively correlated; -0.65, p-value = 2e-16
plot(data$mileage, data$price, cex=0.1, ylab='price', xlab='mileage',main='price Vs. mileage')

## other price relationships
table(data$body_type)
table(data$fuel_type)
prop.table(table(data$fuel_type)) # 44% Diesel, 36% Petrol
# table(data$engine_size)
table(data$transmission) # 81% Automatic, 19% Manual
prop.table(table(data$year))
prop.table(table(data$colour)) # 27% Black, 20% Blue, 20% Grey, 19% White
# body_type
boxplot(data$price ~ data$body_type,
        subset = data$body_type != 'Unlisted',
        las=1, ylab='price', xlab ='', cex.axis = 0.75, outline=F,
        main='price Vs. body type') # not very interesting
# fuel_type
electricORhybrid <- !(data$fuel_type %in% c('Petrol', 'Diesel','Unlisted')) 
sum(electricORhybrid, na.rm=T)/nobs # 20% listings electric/hybrid
boxplot(data$price[which(data$fuel_type == 'Diesel')],
        data$price[which(data$fuel_type == 'Petrol')],
        data$price[which(electricORhybrid)],
        names = c('Diesel (44%)', 'Petrol (36%)', 'Electric/Hybrid (20%)'), 
        ylab='price', main = 'price Vs. fuel type'
         , outline = F
        ) # shows price Electric/Hybrid > Petrol > Diesel
# transmission
boxplot(data$price ~ data$transmission, 
        subset = data$transmission %in% c('Automatic', 'Manual'),
        names = c('Automatic (81%)', 'Manual (19%)'),
        ylab='price', xlab='', main='price Vs. transmission',
        outline = F) # shows price transmission > manual
# year
boxplot(data$price ~ data$year, ylab='price', xlab='year', main = 'price Vs. year', cex=0.5) # shows price of listing is strongly correlated with price, until classic
# colour
blackORblueORgreyORwhite <- data$colour %in% c('Black', 'Blue', 'Grey', 'White')
sum(!blackORblueORgreyORwhite, na.rm=T)/nobs # 14% listings other colour
boxplot(data$price[which(data$colour == 'Black')],
        data$price[which(data$colour == 'Blue')],
        data$price[which(data$colour == 'Grey')],
        data$price[which(data$colour == 'White')], 
        data$price[which(!blackORblueORgreyORwhite)], 
        names = c('Black (27%)', 'Blue (20%)', 'Grey (20%)', 'White (19%)', 'Other (14%)'), 
        ylab='price', main = 'price Vs. colour', cex.axis=0.8, cex = 0.5, outline = F) # indicates colour doesn't have a strong influence on price

## price/price_position
boxplot(data$price, main='price', horizontal=T, cex = 0.5)
hist(data$price, 100, main='price', xlab='price') # price very negatively skewed, what's the price position of high outliers?
iqr <- IQR(data$price, na.rm=TRUE)
q3 <-  quantile(data$price, 0.75,na.rm=TRUE)
high <- data$price > q3 + 1.5*iqr # outlier - outside of IQR ± 1.5*IQR
sum(high, na.rm=T)/nobs # 6% high price outliers
expensive <- data$price_position[which(high)]
normal <- data$price_position[which(!high)]
boxplot(normal, expensive, names = c('normal (94%)', 'expensive (6%)'), ylab='price position', outline = F, xlab = 'price', 
        main='expensive Vs. price position')  # expensive outliers have a higher price position on average
wilcox.test(normal, expensive) # p-value = 4.647e-08
# high price position - people are willing to pay the price?
# very expensive cars tend to be the ones people are willing to pay the price for

## features
features <- data[15:24]
summary(features) # features are binary 
barplot(colMeans(features), names = c(1:10), main='feature proportion', xlab='feature', ylim = c(0,1)) # features 4, 8 and 10 are 'rare'
data$rare_feature <- data$feature_4 ==1 | data$feature_8 ==1| data$feature_10==1
sum(data$rare_feature, na.rm=T)/nobs # 18% 'rare' features
# independent features
for (i in colnames(features)){
  print(i)
  print(wilcox.test(data$price_position[which(data[[i]]==0)], data$price_position[which(data[[i]]==1)]))
} # feature 4, 6, 7
colnames(features)
print(wilcox.test(data$price_position[which(data[['feature_10']]==0)], data$price_position[which(data[['feature_10']]==1)]))
control <- !(data$feature_4==1|data$feature_6==1|data$feature_7==1)
data$price_position[which(control)]
boxplot(data$price_position[which(data$feature_1==1)],
        data$price_position[which(data$feature_2==1)],
        data$price_position[which(data$feature_3==1)],
        data$price_position[which(data$feature_4==1)],
        data$price_position[which(data$feature_5==1)],
        data$price_position[which(data$feature_6==1)],
        data$price_position[which(data$feature_7==1)],
        data$price_position[which(data$feature_8==1)],
        data$price_position[which(data$feature_9==1)],
        data$price_position[which(data$feature_10==1)],
        outline=F,
        ylab = 'price position', xlab = 'feature',
        main = 'price position Vs. feature',
        names = c(1:10)
)
# feature_4
feature_4 <- data$price_position[which(data$feature_4==1)]
not_feature_4 <- data$price_position[which(!(data$feature_4==1))]
sum(data$feature_4, na.rm=T)/nobs # 8% feature 4

boxplot(feature_4, not_feature_4, 
        names=c('feature 4 (8%)', 'not feature 4 (92%)'), 
        main='feature 4 Vs. price position',
        outline = F,
        ylab = 'price position')
options(scipen = 2)
wilcox.test(feature_4, not_feature_4) # p-value = 8.472e-08
# total features
data$feature_total <- rowSums(features)
boxplot(data$price_position ~ data$feature_total, outline=F,
        ylab = 'price position', xlab = 'total features',
        main = 'price position Vs. total features')
# outlier features
outlier_features <- subset(data, data$price_position_outlier==T)[15:24]
barplot(colMeans(outlier_features), names = c(1:10), main='feature proportion', xlab='feature', ylim=c(0,1)) # not too dissimilar