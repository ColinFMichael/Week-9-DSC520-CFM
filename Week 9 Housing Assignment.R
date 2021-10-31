# Assignment: Week 9 Housing Data
# Name: Michael, Colin
# Date: 2021-10-27

#b i.) I changed the column Sale Price to Accounting format. This is to put the data into dollar signs.
      # I also had to get rid of the blank spaces in 'sale_warning'
      # I also removed the spaces in SalesDate and SalePrice


setwd("/Users/colinmichael/Desktop/Data Science/DSC520")

library(readxl)
housing_df <- read_xlsx("week-7-housing.xlsx")

housing_df

sqft_lm <-  lm(SalePrice ~ square_feet_total_living, data=housing_df) 

otherpredictors_lm <- lm(SalePrice ~ square_feet_total_living + bedrooms + year_built, data = housing_df)

# I believe that new homes and homes with more bedrooms are desirable in houses, thus will likely be
# positively correlated with salesprice
summary(sqft_lm)
summary(otherpredictors_lm)
#iii.)
#The R^2 and Adjusted R of the first variable is .2066 and the R^2 and Adjusted R of the second variable is .2189 and .2187
#An R^2 of .2066 shows that 20.6% of the variation in Sale Price is explained by Square Feet
#An R^2 of .2189 shows that 21.89% of the variation in Sale Price is explained by Square Feet, Bedrooms, and Year Built
#Adding new variables slightly increased the amount of variation explained.

#iv) The beta parameters are: square feet: 178.4, bedrooms: -125.5, year built: 2564
#These indicate that square footage and year built are positively correlated to sale price, year built more so, and bedrooms are negatively correlated to sales price.

#v.)
predict(sqft_lm, housing_df, interval = 'confidence')
predict(otherpredictors_lm, housing_df, interval = 'confidence')
#The results indicate that the first value, fit, is the predicted Sale Price for each input, and 
#95% of the values lie between the lower and upper values

#vi.) ANOVA
one.way <- aov(SalePrice ~ square_feet_total_living, data = housing_df)

two.way <- aov(SalePrice ~ square_feet_total_living + bedrooms + year_built, data = housing_df)

summary(one.way)
summary(two.way)
#Doing the two-way ANOVA increases the f-value of Sq Ft, increasing the likelihood that sq ft explains the variation in Sale Price.

#vii.) 

library(car)
cbind(outlierTest(sqft_lm))
cbind((outlierTest(otherpredictors_lm)))

#viii.)
standard_res <- rstandard(sqft_lm)
standard_res

multipereg_res <- rstandard(otherpredictors_lm)
multipereg_res

final_data_first <- cbind(housing_df$SalePrice, standard_res)
f1<-data.frame(final_data_first)
f1


final_data_second <- cbind(housing_df$SalePrice, multipereg_res)
f2<-data.frame(final_data_second)
f2

library(sqldf)
res1 <- sqldf("Select * from f1 where standard_res > 2 or standard_res < -2")
res2 <- sqldf("Select * from f2 where multipereg_res > 2 or multipereg_res < -2")

res1
res2

#ix)
sum(resid(sqft_lm)^2)
sum(resid(otherpredictors_lm)^2)
#x) The sum of residuals is smaller in the second model, which controls for other variables. So the second model is a better fit.

#xi).
hats <- as.data.frame(hatvalues(sqft_lm))
plot(hatvalues(sqft_lm), type = 'h')

hats <- as.data.frame(hatvalues(otherpredictors_lm))
plot(hatvalues(otherpredictors_lm), type = 'h')

plot(cooks.distance(sqft_lm))
plot(cooks.distance(otherpredictors_lm))

#xii.) 
durbinWatsonTest(sqft_lm)
durbinWatsonTest(otherpredictors_lm)
#P Values are less than .05, .543 and .557 test statistic, we can reject null hypothesis and assume independence.

#xiii.)
plot(sqft_lm)
plot(otherpredictors_lm)
#Plots show linearity, condition is met.

#xiv.)
library(ggplot2)

ggplot(data = housing_df, aes(x = sqft_lm$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

library(ggplot2)

ggplot(data = housing_df, aes(x = otherpredictors_lm$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#Histograms show normal distribution, no major abnormalities.

#xv) Overall, this is an unbiased regression model. This tells us that the sample is a good and accurate representation of the entire population.



