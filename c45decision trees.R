#read dataset
c45csv <- read.csv("C:/Users/bunya/Desktop/c45csv.csv")
View(c45csv)
#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
aggregate(gperg ~ class, c45csv, FUN=mean)
aggregate(possesperc ~ class, c45csv, FUN=mean)
aggregate(passperc ~ class, c45csv, FUN=mean)
aggregate(aerials ~ class, c45csv, FUN=mean)
#Control any variable is absent 
anyNA(c45csv)
#provide some visuals for the beginning
plot(aerials~class,c45csv)
plot(gperg~class,c45csv)
plot(possesperc~class,c45csv)
plot(passperc~class,c45csv)
#Indices for the training data set are determined and created.
library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=c45csv$class, p= 0.7, list=FALSE)
education<-c45csv[educ_indic,]
summary(educ_indic)
test<-c45csv[-educ_indic,]
summary(test$class)
library(RWeka)
c45_model <- J48(class~., data = education)
c45_model
predictclass<- predict(c45_model, newdata = test[,-5])
cm <-confusionMatrix(data=predictclass, reference= test[,5])
cm

