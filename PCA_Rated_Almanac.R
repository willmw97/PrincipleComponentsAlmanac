#Author: Marshal Will
#******************************************************************
#These data are taken from the Places Rated Almanac, 
#by Richard Boyer and David Savageau, 
#copyrighted and published by Rand McNally.
#******************************************************************
#Program: This program uses Principle Components to help determine which 
#factors can predict city rating factors are better 
#******************************************************************
library(corrplot)
library(dplyr)

library(vegan)
library(ade4)
library(MASS)
places <- read.csv("Places.csv")

#Using princomp() fow n>p
str(places)
# n=329; p=13
head(places)
# Will need to scale looking at the different measurments for variables.
# For Housing and Crime, the lower the score the better.
# For all else, the higher the score the better.

# Not using latitude or longitude, population or state. 
places_subset = places[,2:10]
str(places_subset)
#n=329; p=9
# Use PCA to identify the major components of variation in the ratings amongst cities.

# Doing PCA without scaling:
places_subset.pca = princomp(places_subset)
places_subset.pca
summary(places_subset.pca)
places_subset.pca$loadings

# shows why scaling is important
places_subset.pca = princomp(places_subset, cor = TRUE) #without logged variables
summary(places_subset.pca)

# shows the number of principle components retained
# eigen value > 1 --> retain PCs or when propor. ~90%
summary(places_subset.pca)
#Retain 3 PCs

#Creates a scree plot
plot(places_subset.pca, type = "bar", main = "Scree Plot for Places Rated Almanac for Several Major U.S. Cities")
abline(h=1, lty=2,col="red",lwd=5)

# show PCA loadings
places_subset.pca$loadings

# PC1 is giving weight to all dimensions, measuring the overall 9 rating criteria for cities
places_subset
# log1p function computes the natural logarithm of a given number or set of numbers.
log_places_subset <- places_subset%>%
  mutate(log_housing = log1p(Housing))%>%
  mutate(log_hlthcare = log1p(HlthCare)) %>%
  mutate(log_arts = log1p(Arts)) 
str(log_places_subset)
str(log_places_subset[, -c(2, 3, 7)]) #without Housing, HlthCare, and Arts

log_places_subset.pca1 = princomp(log_places_subset[, -c(2, 3, 7)], cor = TRUE) #it doesn't have City variable
summary(log_places_subset.pca1)
#Create a scree plot
plot(log_places_subset.pca1, type = "bar", main = "Scree Plot for Places Rated Almanac for Several Major U.S. Cities")
abline(h=1, lty=2,col="red",lwd=5)

#loadings to interpret
log_places_subset.pca1$loadings[,1:3]

plot(log_places_subset.pca1$scores, main = "Plot of PC1 vs PC2 for Places Data")

abline(h=0, v=0, lty=2,col="red",lwd=1)

biplot(log_places_subset.pca1, cex=0.7)

pairs(log_places_subset.pca1$scores[,1:3], panel = function(x,y) {text(x,y,1:329)})

cor(log_places_subset)
#Outliers
outlying = c(195, 168, 270, 179, 213, 43, 314, 65, 234, 237, 277, 76, 322, 79, 157,
             279, 116, 292, 299, 87, 230, 312)
log_places_subset[outlying,]
log_places_subset[-outlying,]
#Check data with outlying
summary(log_places_subset)
#Check data of outlying
summary(log_places_subset[outlying,])
#Check data without outliying
summary(log_places_subset[-outlying,])

log_places_subset.pca.no.outliers = princomp(log_places_subset[-outlying,2:10], cor = TRUE)
summary(log_places_subset.pca.no.outliers)
log_places_subset.pca.no.outliers$loadings
