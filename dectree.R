#read dataset
football <- read.csv("C:/Users/bunya/Desktop/koyle.csv")
View(football)
#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
aggregate(gperg ~ class, football, FUN=mean)
aggregate(poss ~ class, football, FUN=mean)
aggregate(pass ~ class, football, FUN=mean)
aggregate(gapg ~ class, football, FUN=mean)
#Control any variable is absent 
anyNA(football)
#provide some visuals for the beginning
plot(gapg~class,football)
plot(gperg~class,football)
plot(poss~class,football)
plot(pass~class,football)
#Indices for the training data set are determined and created.


#For 70 percent training data 

library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
c45_model70 <- J48(class~., data = train_Data)
c45_model70
predictclass70<- predict(c45_model70, newdata = test[,-4])
cm70 <-confusionMatrix(data=predictclass70, reference= test[,4])
cm70
summary(c45_model70)


#For 80 percent training data 

library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
c45_model80 <- J48(class~., data = train_Data)
c45_model80
predictclass80<- predict(c45_model80, newdata = test[,-4])
cm80 <-confusionMatrix(data=predictclass80, reference= test[,4])
cm80
summary(c45_model80)





#For 90 percent training data 

library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
c45_model90 <- J48(class~., data = train_Data)
c45_model90
predictclass90<- predict(c45_model90, newdata = test[,-4])
cm90 <-confusionMatrix(data=predictclass90, reference= test[,4])
cm90
summary(c45_model90)






###### 

# Now we go to other applications 
library(plyr)
library(caret)
#Lets Apply Gini algorithm for 70 percent
library(rpart)
#Since the gini algorithm is applied,
#the split value  is choosen as "gini" and 
#the decision tree can be obtained by show(gini) 


#DO IT FOR 70% 

library(plyr)
library(caret)
#Lets Apply Gini algorithm for 70 percent
library(rpart)
#Since the gini algorithm is applied,
#the split value  is choosen as "gini" and 
#the decision tree can be obtained by show(gini) 

set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]

gini70 <- rpart(class~., data = train_Data, method = "class", minsplit = 4 , parms = list(split ="gini"))
show(gini70)

#To see the classification rules obtained with the Gini algorithm in the tree structure, 
#it is necessary to load the rpart.plot package.
library(rpart.plot)
prp(gini70)

#To check the accuracy, let's create a confusion matrix
#by comparing the data obtained from the estimate with the test data.
predictGini70<- predict(gini70,test,type="class")
# if below not works use this cmgini70<-table(test$class,predictGini70)
cmgini70 <-confusionMatrix(data=predictGini70, reference= test[,4])
cmgini70
#Accuracy is calculated from the obtained confusion matrix
# another idiot way to calculate paste ("Accuracy = ",(accuracygini<-(10+6+13+2someval)/sum(cmgini)))


#GINI DO IT FOR 80% 
set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]

gini80 <- rpart(class~., data = train_Data, method = "class", minsplit = 4 , parms = list(split ="gini"))
show(gini80)

#To see the classification rules obtained with the Gini algorithm in the tree structure, 
#it is necessary to load the rpart.plot package.
library(rpart.plot)
prp(gini80)

#To check the accuracy, let's create a confusion matrix
#by comparing the data obtained from the estimate with the test data.
predictGini80<- predict(gini80,test,type="class")
# if below not works use this cmgini70<-table(test$class,predictGini70)
cmgini80 <-confusionMatrix(data=predictGini80, reference= test[,4])
cmgini80



#GINI DO IT FOR 90% 

set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]

gini90 <- rpart(class~., data = train_Data, method = "class", minsplit = 4 , parms = list(split ="gini"))
show(gini90)

#To see the classification rules obtained with the Gini algorithm in the tree structure, 
#it is necessary to load the rpart.plot package.
library(rpart.plot)
prp(gini90)

#To check the accuracy, let's create a confusion matrix
#by comparing the data obtained from the estimate with the test data.
predictGini90<- predict(gini90,test,type="class")
# if below not works use this cmgini70<-table(test$class,predictGini70)
cmgini90 <-confusionMatrix(data=predictGini90, reference= test[,4])
cmgini90







####### RANDOM FORESTTTTTTTTT#####

# RANDOM FOREST FOR 70

set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]

library(randomForest)
rf70 <- randomForest(class~., data=train_Data)
show(rf70)
#confusion matrix for randomforest
predictrf70 <- predict(rf70, test, type = "class")
predictrf70<- predict(rf70,test,type="class")
# if below not works use this cmgini70<-table(test$class,predictGini70)
cmrf70 <-confusionMatrix(data=predictrf70, reference= test[,4])
cmrf70






# bulsshitconfmatRF<-table(test$class, predictRF)
#bullshit for need show(confmatRF)
# bullshit for in case of need paste("Accuracy = ", (accuracyrf<-(16+14+26+12)/sum(confmatRF))





#RANDOM FOREST FOR 80 
set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]

library(randomForest)
rf80 <- randomForest(class~., data=train_Data)
show(rf80)
#confusion matrix for randomforest
predictrf80 <- predict(rf80, test, type = "class")
predictrf80<- predict(rf80,test,type="class")
# if below not works use this cmgini70<-table(test$class,predictGini70)
cmrf80 <-confusionMatrix(data=predictrf80, reference= test[,4])
cmrf80










#RANDOM FOREST FOR 90 
set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]

library(randomForest)
rf90 <- randomForest(class~., data=train_Data)
show(rf90)
#confusion matrix for randomforest
predictrf90 <- predict(rf90, test, type = "class")
predictrf90<- predict(rf90,test,type="class")
# if below not works use this cmgini90<-table(test$class,predictGini70)
cmrf90 <-confusionMatrix(data=predictrf90, reference= test[,4])
cmrf90







##### REGRESSION TREEESSSS#######


# %70 REGRESYON TREES


set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]
library(tree)
regtree70 <- tree(class ~., data= train_Data)
show(regtree70)
text(regtree70,pretty=0)
show(regtree70)
plot(regtree70)
predictregtree70 <- predict(regtree70, test, type = "class")
predictregtree70<- predict(regtree70,test,type="class")

cmregtree70 <-confusionMatrix(data=predictregtree70, reference= test[,4])
cmregtree70


# REGTREE80 
set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]
library(tree)
regtree80 <- tree(class ~., data= train_Data)
show(regtree80)
text(regtree80,pretty=0)
show(regtree80)
plot(regtree80)
predictregtree80 <- predict(regtree80, test, type = "class")
predictregtree80<- predict(regtree80,test,type="class")

cmregtree80 <-confusionMatrix(data=predictregtree80, reference= test[,4])
cmregtree80










# REGTREE90 
set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]
library(tree)
regtree90 <- tree(class ~., data= train_Data)
show(regtree90)
text(regtree90,pretty=0)
show(regtree90)
plot(regtree90)
predictregtree90 <- predict(regtree90, test, type = "class")
predictregtree90<- predict(regtree90,test,type="class")

cmregtree90 <-confusionMatrix(data=predictregtree90, reference= test[,4])
cmregtree90











#OneR  70 lik

set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]
library(OneR)
birRmodel <- OneR(formula = class~., data = train_Data, verbose = TRUE)
summary(birRmodel)
plot(birRmodel)
prediksion70<- predict(birRmodel, test)
eval_model(prediksion70, test)
summary(birRmodel)
plot(birRmodel)




#OneR  80 lik

set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]
library(OneR)
birRmodel80 <- OneR(formula = class~., data = train_Data, verbose = TRUE)
summary(birRmodel80)
plot(birRmodel80)
prediksion80<- predict(birRmodel80, test)
eval_model(prediksion80, test)
summary(birRmodel80)
plot(birRmodel)



#ONER 90 
set.seed(1)
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
test<-football[-educ_indic,]
library(OneR)
birRmodel90 <- OneR(formula = class~., data = train_Data, verbose = TRUE)
summary(birRmodel90)
plot(birRmodel90)
prediksion90<- predict(birRmodel90, test)
eval_model(prediksion90, test)
summary(birRmodel90)
plot(birRmodel90)









#LOGISTIC MODELS TREES 

#70 PERCENT
library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
Lmt_70<- LMT(class~., data = train_Data)
Lmt_70
predictlmt70<- predict(Lmt_70, newdata = test[,-4])
Lmt_70 <-confusionMatrix(data=predictlmt70, reference= test[,4])
Lmt_70
summary(Lmt_70)


# LMT 80

library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
Lmt_80<- LMT(class~., data = train_Data)
Lmt_80
predictlmt80<- predict(Lmt_80, newdata = test[,-4])
Lmt_80 <-confusionMatrix(data=predictlmt80, reference= test[,4])
Lmt_80
summary(Lmt_80)






#LMT 90

library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
Lmt_90<- LMT(class~., data = train_Data)
Lmt_90
predictlmt90<- predict(Lmt_90, newdata = test[,-4])
Lmt_90 <-confusionMatrix(data=predictlmt80, reference= test[,4])
Lmt_90
summary(Lmt_90)


### Decision Stump 77000

library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.7, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
Decs_70<- DecisionStump(class~., data = train_Data)
Decs_70
predictdecs70<- predict(Decs_70, newdata = test[,-4])
Decs_70 <-confusionMatrix(data=predictdecs70, reference= test[,4])
Decs_70
summary(Decs_70)




#DECSTUMP 80


library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.8, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
Decs_80<- DecisionStump(class~., data = train_Data)
Decs_80
predictdecs80<- predict(Decs_80, newdata = test[,-4])
Decs_80 <-confusionMatrix(data=predictdecs80, reference= test[,4])
Decs_80
summary(Decs_80)


#Decision Stump 90
library(caret) #if it is old install recipes
set.seed(1) #set.seed is the recommended way to specify seeds.
educ_indic <-createDataPartition(y=football$class, p= 0.9, list=FALSE)
train_Data<-football[educ_indic,]
summary(educ_indic)
test<-football[-educ_indic,]
summary(test$class)
library(RWeka)
Decs_90<- DecisionStump(class~., data = train_Data)
Decs_90
predictdecs90<- predict(Decs_90, newdata = test[,-4])
Decs_90 <-confusionMatrix(data=predictdecs90, reference= test[,4])
Decs_90
summary(Decs_90)




