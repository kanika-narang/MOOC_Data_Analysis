
#Problem Statement 2: Classifying LoE of the student so that instructor can understand the education level of student and hence improve the course based on students education level.


student_data_dataverse<- read.csv('/home/kanika/Documents/3rd_Sem/github/Data_Analytics_Project/MOOC_Data_Analysis/DATA/cleaned data/big_student_clear_second_version.csv')
student_data_dataverse<- student_data_dataverse[-c(1,2,3)]
source('helper_functions.R')  
library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2) 

#data preparation for classification
# column certified is an integer column so need to convert it to factor 
class(student_data_dataverse$certified)
student_data_dataverse['certified']<- as.factor(student_data_dataverse$certified)
class(student_data_dataverse$certified)


# Since we have calculated the column age so YOB not needed
# hence dropping YOB also column last_event_DI contains 166085
student_data_dataverse<- student_data_dataverse[-11]

# removing th rows with NA in column last_event_DI
row_no<- which(is.na(student_data_dataverse$last_event_DI))
student_data_dataverse <- student_data_dataverse[- row_no, ]

# Since we can see that all the NA's in grade column is because these students have not been certified
# this could because of many reasons like student din't appeared for certification.
sum(is.na(student_data_dataverse$grade))
sum(is.na(student_data_dataverse$grade) & student_data_dataverse$certified ==0)
# Thus replacing NA's in grade column with 0.
student_data_dataverse$grade<- ifelse(is.na(student_data_dataverse$grade),0,student_data_dataverse$grade)

# converting userid_DI to character because random forest does not support
# factor with more than 53 categories.
student_data_dataverse$userid_DI <- as.character(student_data_dataverse$userid_DI)

# converting start_time_DI to date
student_data_dataverse$start_time_DI<- as.Date(student_data_dataverse$start_time_DI)

# converting last_event_DI to date
student_data_dataverse$last_event_DI<- as.Date(student_data_dataverse$last_event_DI)



# i am spliting data into training and testing with 60-40 rule
#Create data for training
sample.ind = sample(2,  
                    nrow(student_data_dataverse),
                    replace = T,
                    prob = c(0.6,0.4))
data.dev = student_data_dataverse[sample.ind==1,]  
data.val = student_data_dataverse[sample.ind==2,]  

# looking at the split percentage for the certified students
# Original Data
table(student_data_dataverse$institute)/nrow(student_data_dataverse)
# Training Data
table(data.dev$institute)/nrow(data.dev) 
# Testing Data
table(data.val$institute)/nrow(data.val)

#Fit Random Forest Model
rf = randomForest(viewed ~ .,  
                  ntree = 100,
                  data = data.dev[,-5])
plot(rf)  
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           main=" Variable Importance")
#Variable Importance
var.imp = data.frame(importance(rf,  
                                type=2))

# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$IncNodePurity,decreasing = T),])

# Predicting response variable
data.dev$predicted.response = predict(rf , data.dev)

# Create Confusion Matrix
print(  
  confusionMatrix(data = data.dev$predicted.response,
                   reference = data.dev$viewed,
                  positive='1'
                  
                  ))

# Now predicting on test data
# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
print(  
  confusionMatrix(data=data.val$predicted.response,  
                  reference=data.val$viewed,
                  positive='1'))
# retrying.........................................................................
# converting viewed to factor
student_data_dataverse$viewed<- as.factor(student_data_dataverse$viewed)

# i am spliting data into training and testing with 60-40 rule
#Create data for training
sample1.ind = sample(2,  
                    nrow(student_data_dataverse),
                    replace = T,
                    prob = c(0.6,0.4))
data1.dev = student_data_dataverse[sample1.ind==1,]  
data1.val = student_data_dataverse[sample1.ind==2,]  



#Fit Random Forest Model
rf1 = randomForest(viewed ~ .,  
                  ntree = 100,
                  data = data1.dev[,-5])
plot(rf1)  
print(rf1)

# Variable Importance
varImpPlot(rf1,  
           sort = T,
           main=" Variable Importance")
#Variable Importance
var.imp = data.frame(importance(rf1,  
                                type=2))

# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
data.dev$predicted.response = predict(rf1 , data.dev)

# Create Confusion Matrix
print(  
  confusionMatrix(data = data.dev$predicted.response,
                  reference = data.dev$viewed,
                  positive='1'
                  
  ))

# Now predicting on test data
# Predicting response variable
data1.val$predicted.response <- predict(rf1 ,data1.val)

# Create Confusion Matrix
print(  
  confusionMatrix(data=data1.val$predicted.response,  
                  reference=data1.val$viewed,
                  positive='1'))
T1<-getTree(rf1, 10, labelVar = TRUE)
plot(T1)