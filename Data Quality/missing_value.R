library(readr)
edx <- read_csv("Z:/IIITB/Data analysis project/edx.csv")
View(edx)

#Missing value Treatment

#For each column find number of missing values
for (i in 1:20)
{
print(paste(i,"th column",colnames( edx )[i] ," has",sum(is.na(edx[i]))," missing values"))
}

#For each column find %age of missing values
for (i in 1:20)
{
  print(paste(i,"th column",colnames( edx )[i] ," has",(sum(is.na(edx[i]))/NROW(edx))*100,"% missing values"))
}

#for LoE_DI we find count of each unique value
print(table(edx['LoE_DI']))

#for YOB we find count of each unique value
print(table(edx['YoB']))

#Find records which have both YoB and LoE_DI missing
print(paste("Number of recrds with both Year of Birth and highest degree missing is: ",(sum(is.na(edx['LoE_DI']) && is.na(edx['YoB']) ))))
