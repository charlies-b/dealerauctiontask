data <- read.csv('DA Technical Assessment Dataset.csv')
head(data)
colnames(data)
summary(data)
dim(data)
nobs <- nrow(data)

# price position
summary(data$price_position) # numeric value centered about 100
pp_na = sum(is.na(data$price_position))
pp_na/nobs # 12% missing values
boxplot(data$price_position, main='price_position') # narrow distribution, lots of outliers 
hist(data$price_position,100, main='price_position', xlab='price_position') # normally distributed?
qqnorm(data$price_position, main='normal QQ - price_position ')
qqline(data$price_position) # more outliers than expected for normally distributed data; center normally distributed, what about the outliers?

# 1 price_position/price
cor.test(data$price, data$price_position) # price position is not correlated to price
q3 <-  quantile(data$price_position, 0.75,na.rm=TRUE)
q1 <- quantile(data$price_position, 0.25,na.rm=TRUE)
iqr <- IQR(data$price_position, na.rm=TRUE) 
high <- data$price_position > q3 + 1.5*iqr # outlier - outside of IQR ± 1.5*IQR
low <- data$price_position < q1 - 1.5*iqr
sum(high, na.rm='TRUE') # 219 high outliers 
sum(low, na.rm='TRUE') # 170 high outliers 
boxplot(data$price[which(low)], data$price[which(!low, !high)], data$price[which(high)], xlab='price_position', ylab='price', names=c('low extreme', 'middle', 'high extreme'), main = 'price of price_position outliers')

# 2 price/mileage
cor.test(data$mileage, data$price) # price and mileage are strongly negatively correleated; -0.65, p-value = 2e-16
plot(data$mileage, data$price, cex=0.1, ylab='price', xlab='mileage',main='price Vs. mileage')

# 3 price/categorical relationships
table(data$body_type)
table(data$fuel_type)
prop.table(table(data$fuel_type)) # 44% Diesel, 36% Petrol
# table(data$engine_size)
table(data$transmission) # 81% Automatic, 19% Manual
prop.table(table(data$year))
prop.table(table(data$colour)) # 27% Black, 20% Blue, 20% Grey, 19% White

# boxplot(data$price ~ data$area)
# boxplot(data$price ~ data$body_type,las=2, ylab='price', xlab ='', cex.axis = 0.75, main='price Vs. body_type')
# boxplot(data$price ~ data$fuel_type, subset = data$fuel_type  %in% c('Diesel', 'Petrol', 'Electric'), ylab='price', xlab = '', main = 'price Vs. fuel_type')
boxplot(data$price ~ data$year, ylab='price', xlab='year', main = 'price Vs. year')

electricORhybrid <- !(data$fuel_type %in% c('Petrol', 'Diesel','Unlisted'))
sum(electricORhybrid, na.rm=T)/nobs # 20% listings electric/hybrid
boxplot(data$price[which(data$fuel_type == 'Diesel')],
        data$price[which(data$fuel_type == 'Petrol')],
        data$price[which(electricORhybrid)],
        names = c('Diesel (44%)', 'Petrol (36%)', 'Electric/Hybrid (20%)'), 
        ylab='price', main = 'price Vs. fuel_type')

boxplot(data$price ~ data$transmission, 
        subset = data$transmission %in% c('Automatic', 'Manual'),
        names = c('Automatic (81%)', 'Manual (19%)'),
        ylab='price', xlab='', main='price Vs. transmission')

blackORblueORgreyORwhite <- data$colour %in% c('Black', 'Blue', 'Grey', 'White')
sum(!blackORblueORgreyORwhite, na.rm=T)/nobs # 14% listings other colour
boxplot(data$price[which(data$colour == 'Black')],
        data$price[which(data$colour == 'Blue')],
        data$price[which(data$colour == 'Grey')],
        data$price[which(data$colour == 'White')], 
        data$price[which(!blackORblueORgreyORwhite)], 
        names = c('Black (27%)', 'Blue (20%)', 'Grey (20%)', 'White (19%)', 'Other (14%)'), 
        ylab='price', main = 'price Vs. colour', cex.axis=0.8)

boxplot(data$price_position ~ data$fuel_type)
