
big_student_data<- read.csv('/home/kanika/Documents/3rd_Sem/github/Data_Analytics_Project/MOOC_Data_Analysis/DATA/HMXPC13_DI_v2_5-14-14_1.csv')

# spliting column called course_id into 4 columns called Institute, course_id,semester,year.
library(tidyr)
big_student_data<-separate(data = big_student_data, col = course_id, into = c("institute", "course_id","year_sem"), sep = "/")
#Now sepearting year_sem into year and semester
big_student_data<-separate(data = big_student_data, col = year_sem, into = c("year", "semester"), sep = "_")


# converting the  start_time_DI to date formatc
start_time_DI<- as.Date(big_student_data$start_time_DI,format="%d/%m/%y")
big_student_data[["start_time_DI"]]=start_time_DI
str(big_student_data$start_time_DI)

# converting the last_event_DI to date format
last_event_DI<- as.Date(big_student_data$last_event_DI,format="%d/%m/%y")
big_student_data[["last_event_DI"]]=last_event_DI

# adding column month containing month from the column start_time_DI
month=format(big_student_data$start_time_DI,"%m")
big_student_data<-cbind(big_student_data,month)
big_student_data$month <- as.character(big_student_data$month)

#removing the column roles because it is completely blank
big_student_data<- big_student_data[,-21]

#removing the column registered because it contains 1 in each row.
big_student_data<- big_student_data[,-6]

# Retrieving the course number where semester is missing and filling it  
# on the basis of months of 'start_time_DI'
x<- subset(big_student_data,big_student_data$semester=="")
course_id_with_no_semester<- unique(x$course_id)
couse_cs50x<-subset(big_student_data,big_student_data$course_id == "CS50x")



# function to fill the value of semester
sem2<-function(ele)
{
  if(!(is.na(ele["semester"])) & (ele["semester"])=="")
  {
    if(as.character(ele["month"]) %in% c("03","04","05"))
    {
      ele["semester"]<- "Spring"
    }
  else if(as.character(ele["month"]) %in% c("06","07","08"))
  {
    ele["semester"]<- "Summer"
  }else {
    ele["semester"]<- "Fall"
  }
  }else
  {
    ele["semester"]<- (ele["semester"])
  }
}


#filling the value of semester
big_student_data$semester<-apply(big_student_data,1,sem2)

# removing column month
big_student_data<- big_student_data[-24]

#writing in csv
write.csv(big_student_data, file = "/home/kanika/Documents/3rd_Sem/github/Data_Analytics_Project/MOOC_Data_Analysis/DATA/big_student_clear_last_version.csv")

# replacing NA with 0 in column nevents
big_student_data$nevents<- ifelse(is.na(big_student_data$nevents),0,big_student_data$nevents)

# to check if there is some rows with ndays_act as Na but nevents>0
which(is.na(big_student_data$ndays_act) & big_student_data$nevents>0)

#  replacing Na's in ndays_act with 0 since they signifies that student 
#  had no interaction with the course after registration
big_student_data$ndays_act<- ifelse(is.na(big_student_data$ndays_act),0,big_student_data$ndays_act)

#  replacing Na's in incomplete_flag with 0 
big_student_data$incomplete_flag<- ifelse(is.na(big_student_data$incomplete_flag),0,big_student_data$incomplete_flag)

#  replacing Na's in nchapers with 0 
big_student_data$nchapters<- ifelse(is.na(big_student_data$nchapters),0,big_student_data$nchapters)

#replacing NA's from the column nplay_video with the maximum of 'nevents', 'nchapters'
#and 'ndays_act'
big_student_data$nplay_video<- ifelse(is.na(big_student_data$nplay_video),max(big_student_data$nevents,big_student_data$nchapters,big_student_data$ndays_act),big_student_data$nplay_video)

# getting the row numbers where nevents is not NA
row_no<- which(!is.na(big_student_data$nevents))
#getting subset of dataframe for the above rows
nevents_ava<- big_student_data[row_no,c("course_id","nevents")]

# getting average values of nevents grouped by course_id
agree_nevents<-aggregate(x=nevents_ava,FUN=mean,by=list(course_id=nevents_ava$course_id))


#filling the value of nevents
big_student_data$nevents <- ifelse(is.na(big_student_data$nevents),nevents_ava$nevents[match(big_student_data$course_id, nevents_ava$course_id)],big_student_data$nevents)


# Deriving new column age from yob
age<- big_student_data$year - big_student_data$YoB
big_student_data<-cbind(big_student_data,age)

# replacing grade value to 1 where it is greater than 1
big_student_data$grade[which(big_student_data$grade>1)]<-1



