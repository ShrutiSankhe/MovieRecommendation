#ML - Project
# set working directory
setwd("/Users/shruti/Machine Learning/Final Project")

initial_time = date()

# Install and loading many packages at once
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("VIM","FastKNN","SpatialTools","ModelMetrics","reshape2")

ipak(packages)

# load the required libraries
library(reshape2)
library(SpatialTools)

#load the input file
infile <- read.csv("ratings.csv", header = TRUE)  

#Validate the file
dim(infile)
head(infile) 

#convert multiple rows to multiple columns
in_df = reshape(infile[1:3], direction="wide", idvar="userId", timevar="movieId")
dim(in_df) # 671 9067

#split the file as test and train
set.seed(100)
idx <- sample(nrow(in_df),0.8*nrow(in_df))
train_df=in_df[idx,]; dim(train_df) # 536 9067
test_df=in_df[-idx,]; dim(test_df) # 135 9067

# write CSV to local machine
#write.csv(train_df, file = "C:/Users/skandhasamy/Desktop/Machine Learning/Project/train_new1.csv", row.names = TRUE)
#write.csv(test_df, file = "C:/Users/skandhasamy/Desktop/Machine Learning/Project/test_new1.csv", row.names = TRUE)

# exclude movie id column
train_df <- train_df[-1]; dim(train_df)
test_df <- test_df[-1]; dim(test_df)

#normalize the test and train dataset
# Calculating the Mean and Standard Deviation vectors for training dataset

train_meansSdX = matrix(0,dim(train_df)[1],2)

for (i in 1:nrow(train_df)){
  train_meansSdX[i,1] = mean(as.double(train_df[i,]),na.rm=TRUE)
}

for (i in 1:nrow(train_df)){
  train_meansSdX[i,2] = sd(train_df[i,],na.rm=TRUE)
}

# Normalizing the data, subtract mean from the values and divide by standard deviation
train_df <- train_df - train_meansSdX[,1]
train_df <- train_df / train_meansSdX[,2]

# Calculating the Mean and Standard Deviation vectors for test dataset

test_meansSdX = matrix(0,dim(test_df)[1],2)

for (i in 1:nrow(test_df)){
  test_meansSdX[i,1] = mean(as.double(test_df[i,]),na.rm=TRUE)
}

for (i in 1:nrow(test_df)){
  test_meansSdX[i,2] = sd(test_df[i,],na.rm=TRUE)
}

# Normalizing the test data, subtract mean from the values and divide by standard deviation
test_df <- test_df - test_meansSdX[,1]
test_df <- test_df / test_meansSdX[,2]

#Knn algorithm
#initialize the variables

K=6 # no of nearest neighbours to consider
res=matrix(c("i", "j", "actual","predict"),1,4); dim(res) # to store result of prediction
res

n = nrow(test_df)
#n = 2
for (i in 1:n)
{
  c = which(!is.na(test_df[i,]));  # all the movies user i rated from test
#length(c)

for (j in c) # for every movie j rated by user i in test set
{
  x=which(!is.na(train_df[,j]))# all users rated the movie j in training set
  x1 <- as.matrix(test_df[i,]) 
  x2 <- as.matrix(train_df[x,])
  smallestIndex <- order(as.matrix(dist(rbind(x1,x2), upper = TRUE))[1,]) # order nearest neighbours
  smallestIndex = smallestIndex[-1]-1 # excluede first row as it is X1 and subtract 1 from other rows to deduce correct index
  pred = mean(x2[smallestIndex[1:K],j]) # mean of first K neighbour ratings
  
  # De-normalize the data
  pred = (pred * test_meansSdX[i,2]) + test_meansSdX[i,1]
  
  # Denormalize the actual value
  actual = (x1[1,j] * test_meansSdX[i,2]) + test_meansSdX[i,1]
  
  res1 = c(i,j,actual,pred); # round to nearest 0.5
  res= rbind(res,res1) # write result into file
  #print(c(j, res1))
}
}

res2 <- na.omit(res[-1,])
dim (res)
length(which(!is.na(test_df[1:n,])))

#Mean square error
results = rmse(as.double(res2[,3]),as.double(res2[,4]))

end_time = date()

text_result = cbind(initial_time , "(" , as.character(results) , ")" , end_time)

write.csv(res, file = paste(K,"-shruti-res.csv"), row.names = TRUE)
write.csv(text_result, file = paste(K,"-shruti-text_result.csv"), row.names = TRUE)

