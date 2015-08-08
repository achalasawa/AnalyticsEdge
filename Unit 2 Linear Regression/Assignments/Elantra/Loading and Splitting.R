# Loading Dataset

elantra = read.csv("elantra.csv")
elantratrain = subset(elantra,Year < 2013)
elantratest = subset(elantra,Year > 2012)