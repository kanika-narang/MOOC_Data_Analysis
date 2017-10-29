student_data<- read.csv('/home/kanika/Documents/3rd_Sem/github/Data_Analytics_Project/MOOC_Data_Analysis/DATA/appendix_new.csv')

# To split the column Instructor
library(splitstackshape)
new_student<-cSplit(student_data, "Instructors", ",", "long")
student_data<-new_student

# get drop_outs
drop_outs<- student_data$Participants..Course.Content.Accessed.- student_data$Certified - student_data$Audited....50..Course.Content.Accessed.
student_data<- cbind(student_data,drop_outs)

#writing in csv
write.csv(student_data, file = "/home/kanika/Documents/3rd_Sem/github/Data_Analytics_Project/MOOC_Data_Analysis/DATA/kaggle_student_clear.csv")
