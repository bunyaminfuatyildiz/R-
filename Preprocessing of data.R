#Data preprocessing.
#download hepatitis.names and hepatitis.data from https://archive.ics.uci.edu/ml/machine-learning-databases/hepatitis/ 
#Using the readtable() command and adding dataset headers.
# clean memo
rm(list =ls())
# Write the directory that contains your data.
setwd("c:/Users/bunya/Desktop/datas")
#Import the data into the hepatitis object.
hepatitis <- read.table("C:/Users/bunya/Desktop/datas/hepatitis.data", header=FALSE, na.strings="?")
View(hepatitis)
colnames(hepatitis)<- c("class", "age", "sex", "steroid", "antivirals", "fatigue", "malaise", "anorexia", "liverbig", "liverfirm", "splenpalpable", "spiders", "ascites", "varices", "bilirubin", "alkphos", "sgot", "albumin", "protime", "histology")




##Clearing missing data.
##The process of deleting lost data in variable protime.
hepatitis1<- hepatitis[!is.na(hepatitis$protime),]
#If you want to delete missing data of more than one variable, you can add with &
#hepatitis1<- hepatitis[!is.na(hepatitis$protime) &!is.na(hepatitis$liverfirm),]


#Filling in the missing data with any constants.
#copy hepatitis to hepatitis_work, and fill all NAs with 3.
hepatitis_work <- hepatitis
hepatitis_work$protime[is.na(hepatitis_work$protime)<-3]




#To fill in the data of the River Big incomplete according to average method.
hepatitis_work <- hepatitis
hepatitis_work$liverbig[is.na(hepatitis_work$liverbig)]<-round(mean(hepatitis_work, na.rm = TRUE))



##Let's write a GENERAL function that allows us to replace lost data with an average
mis_dat_aver = function(x){
  x <- as.numeric(as.character(x))
  #each colum transformed  to numeric value.
  x[is.na(x)] = median(x, na.rm = TRUE) #Replace missing data with column average values.
  x # show x
}
hepatitis_work = data.frame(apply(hepatitis,2,mis_dat_aver))



#MIN-MAX NORMALIZATION
newmin=0
newmax=1
#Missing data of sgot variable is cleared and  is assigned to hepatitis_work.
hepatitis_work <- hepatitis
hepatitis_work$sgot[is.na(hepatitis_work$sgot)]<-round(mean(hepatitis_work$sgot, na.rm = TRUE))
#find min and max values?
min = min(hepatitis_work$sgot)
max = max(hepatitis_work$sgot)
x<- hepatitis_work$sgot[i]
nx = ((x - min)/max - min)) * (newmax -newmin)+newmin
hepatitis_work$sgot[i]<-nx
}


#ZSKOR normalization
newmin=0
newmax=1
hepatitis_work <- hepatitis
hepatitis_work$sgot[is.na(hepatitis_work$sgot)]<-round(mean(hepatitis_work$sgot, na.rm = TRUE))

std_dev = sd(hepatitis_work$sgot)
mea= mean(hepatitis_work$sgot)
for i in 1: length(hepatitis_work$sgot)){
  x<- hepatitis_work$sgot[i]
  nx = (x - mea)/std_dev
  hepatitis_work$sgot[i]<-nx
  }



###Normalization with decimal scaling.

#number of digits of the largest number
digits = 3 
#typical cleaning made on data
hepatitis_work <- hepatitis
hepatitis_work$sgot[is.na(hepatitis_work$sgot)]<-round(mean(hepatitis_work$sgot, na.rm = TRUE))

for (i in 1:length(hepatitis_work$sgot)){x<-hepatitis_work$sgot[i]}
nx = x / 10^digits
hepatitis_work$sgot[i] <- nx 
}






##### BOX-COX NORMALIZATION
lambda = -2
for i in 1:length(hepatitis_work$sgot)){
  x<- hepatitis_work$sgot[i]
  nx = ((x^lambda)- 1)/lambda
  hepatitis_work$sgot[i] <- nx
}

