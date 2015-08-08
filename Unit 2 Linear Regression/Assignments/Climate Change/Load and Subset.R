# loading the dataset
climatechange = read.csv("climate_change.csv")

# studying the dataset
str(climatechange)
summary(climatechange)

# subsetting into training and test
climatechange_train = subset(climatechange, Year < 2007)
climatechange_test = subset(climatechange, Year > 2006)

# training data set
str(climatechange_train)
summary(climatechange_train)

# test data set
str(climatechange_test)
summary(climatechange_test)
