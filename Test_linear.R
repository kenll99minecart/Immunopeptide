# sample data frame
df <- data.frame( x= c(1,2,3,4,5),
				y= c(1,5,8,15,26))

# fit linear model
linear_model <- lm(y ~ x^2, data=df)

# view summary of linear model
summary(linear_model)
