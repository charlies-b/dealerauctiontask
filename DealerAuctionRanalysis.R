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
hist(data$price_position,100, main='price_position') # normally distributed?
qqnorm(data$price_position, main='price_position (normal QQ)')
qqline(data$price_position) # more outliers than expected for normally distributed data; center normally distributed, what about the outliers?

# relationships - price/price_position
cor.test(data$price, data$price_position) # price position is not correlated to price
q3 <-  quantile(data$price_position, 0.75,na.rm=TRUE)
q1 <- quantile(data$price_position, 0.25,na.rm=TRUE)
iqr <- IQR(data$price_position, na.rm=TRUE) 
high <- data$price_position > q3 + 1.5*iqr # outlier - outside of IQR ± 1.5*IQR
low <- data$price_position < q1 - 1.5*iqr
sum(high, na.rm='TRUE') # 219 high outliers 
sum(low, na.rm='TRUE') # 170 high outliers 
boxplot(data$price[which(low)], data$price[which(!low, !high)], data$price[which(high)], xlab='price_position', ylab='price', names=c('low extreme', 'middle', 'high extreme'), main = 'price of price_position outliers')