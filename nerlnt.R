# author: bunyamin fuat yildiz artificial neural network example
# open the package MASS.
library(MASS)
#the datasets in the mass package transferred to the system with the command ' data' 
data("Pima.tr")
data("Pima.te")
#to cheack  and obtain the first several rows of a matrix or data frame using 'head'
head(Pima.tr)
head(Pima.te)
#get summary inf about the datasets
summary(Pima.tr)
summary(Pima.te)

#Data Preprocessing

#issue 1 - ann only works with numeric values. some ann only work with binary datasets
# if the qualitative data is sick and healthy; 
# or if there is or there is not, this data should change to numerical value such as 1 and 0.
# issue 2- two data normalization
#If the normalization is used, the difference between the largest and the smallest data will be scaled
#and the performance of the model will increase.in this context clusterSim package is useful

library(clusterSim)
# The data is normalized except for the type column.
#The parameter type = "n4" ensures that the attribute fields are normalized according to the min-max normalization rule.


Pima.tr[! names(Pima.tr) %in% c("type")] <- lapply(1, function(x) clusterSim::data.Normalization(Pima.tr[!names(Pima.tr)%in% c("type")], type = "n4",
normalization = "column"))[[1]]


Pima.te[! names(Pima.te) %in% c("type")] <- lapply(1, function(x) clusterSim::data.Normalization(Pima.te[!names(Pima.te)%in% c("type")], type = "n4",
normalization = "column"))[[1]]

#people who are sick are coded as 1 instead of yes and grouped together so we have 2 groups
Pima.tr$sick = ifelse(Pima.tr$type=="Yes",1,0)
Pima.tr$healthy = ifelse(Pima.tr$type=="No",1,0)
#Thus, sick and healthy classes will be recorded in separate qualification areas. 
#This distinction will work to describe the classes in the data during the training of the dataset.


## by using as.formula we determine the output according to 
formula=as.formula(sick+healthy~npreg + glu +bp+skin+bmi+ped+age)
#When the code is examined, the tilde operator has sick and healthy classes that contain the targets that the artificial neural network must reach,

#The qualification areas to be used for ann's training are ranked after work.
# As the application is used for the training of the network, except for the type attribute field in the Pima.tr data set, other fields except the type attribute field should be given in the data parameter.


data = as.matrix(Pima.tr[,!(names(Pima.tr) %in%  c("type"))])

#he parameter must be TRUE when the # error function is equal to the negative log-likelihood function. default is FALSE.
# ANN Learning algorithm Backpropagation, agin error function sse, agin activation function is determined as logistic function, single interlayer and 15 noron.

# Train ANN
library(neuralnet)
ann_model <- NULL
ann_model <- neuralnet( formula = as.formula( sick + healthy ~ npreg + glu + bp + skin + bmi +ped +age),
                        data = as.matrix(Pima.tr[, ! (names(Pima.tr) %in% c("type"))]),
                        hidden = c(15), err.fct = "sse", learningrate = 0.01, algorithm = "backprop",
                        act.fct = "logistic", linear.output = FALSE, stepmax = 1e6)

# When ANN training is completed, we can access the error rate and number of steps with the help of the following codes
ann_model_error <- ann_model$result.matrix["error", 1]
ann_model_steps <- ann_model$result.matrix["steps",1]

predictions <- NULL
predictions<- neuralnet::compute( x = ann_model, covariate = Pima.te[, ! (names(Pima.te) %in% c("type"))], rep = 1)$net.result


# Output values are in two columns. Network output values are the probability that each sample in the test dataset belongs to the classes of the network.
#he first column is sick, since we assign as #formula (sick + healthy).
# The following code block is used to categorize the results given by the network as a percentage.
#If a sample is over 50% of a class, that sample will be tagged as that class

# Compute function is used to test the network. With ANN x parameter which is trained for this function
# The test data set is given with the covariate parameter. In the light of this information, the output of the network after the test will be assigned to a variable called "predictions".
# The net.result output of the compute function is used for performance evaluation.
#After , the prediction success of the network can be seen with the table () function.

categoric_prediction <- NULL
for (i in 1:nrow(predictions)){ if (which.max(predictions[i,])==1) 
  categoric_prediction[i]<-
  "Yes" else categoric_prediction[i]<-"No"}

#table fonksiyonu ile kategorik olarak ANN sonuclari gorulebilir
table(categoric_prediction, Pima.te$type)

# ConfusionMatrix in caret package provides more advanced statistics for performance evaluation.
# The classifier used in this function with the estimates data parameter, according to which classes the researcher hypothesizes
# and hence the class of the statistical values to be given is adjusted with the positive parameters.

#there is confusion matrix 
library(caret)
#The ConfusionMatrix function requires a class to be referenced in predictive results and noble values for categorical comparison. 
#For this reason, the Yes class which refers to the patient's illness is taken as reference both in the estimation results and in the actual values.

categoric_prediction <- as.factor(categoric_prediction)
categoric_prediction <- relevel(categoric_prediction, ref= "Yes")

Pima.te$type <- as.factor(Pima.te$type)
Pima.te$type <- relevel(Pima.te$type, ref="Yes")


cmatrix<- NULL
cmatrix<-confusionMatrix(data = categoric_prediction, reference = Pima.te$type, positive = "Yes" )
cmatrix

#Visually express it man
fourfoldplot(cmatrix$table, color =c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")
  
plot(ann_model)


