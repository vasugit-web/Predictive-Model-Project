car_info <- read.csv(url("http://mlr.cs.umass.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.names"), header=FALSE)
car_Info_dataset = read.csv(url("http://mlr.cs.umass.edu/ml/machine-learning-databases/autos/imports-85.data"), header=FALSE)

#str(car_Info_dataset)
##Filter(function(car_Info_dataset) !any(car_Info_dataset=="?"), df)
##elect(df, -contains("PERMISSIONS"))
#install.packages("dbplyr")
library(party)
library(dbplyr)
library(dplyr)
car_Info_dataset_scrubbed = select(car_Info_dataset, -contains("?"))

#View(car_Info_dataset_scrubbed)
car_Info_dataset_scrubbed = na.omit(car_Info_dataset_scrubbed)
plot(car_Info_dataset_scrubbed$V1, car_Info_dataset_scrubbed$V2)# + car_Info_dataset_scrubbed$V3 + car_Info_dataset_scrubbed$V4 + car_Info_dataset_scrubbed$V5 + car_Info_dataset_scrubbed$V6 + car_Info_dataset_scrubbed$V7)
#meanCarMPG = mean(car_Info_dataset_scrubbed$V1 , na.rm=T)

#Cabline(h=meanCarMPG)

model_car_Info_dataset = lm(car_Info_dataset_scrubbed$V1 ~ car_Info_dataset_scrubbed$V2 + car_Info_dataset_scrubbed$V3 + car_Info_dataset_scrubbed$V4 + car_Info_dataset_scrubbed$V5 + car_Info_dataset_scrubbed$V6 + car_Info_dataset_scrubbed$V7)
summary(model_car_Info_dataset)
abline(model_car_Info_dataset,col = "red")

plot(model_car_Info_dataset)

cis = car_Info_dataset_scrubbed
plot(car_Info_dataset_scrubbed$V1,car_Info_dataset_scrubbed$V2)
#meanCarMPG = mean(car_Info_dataset_scrubbed$V1)
#meanCarMP
#abline(h=meanCarMPG)
model_car_Info_dataset_2 = lm(car_Info_dataset_scrubbed$V1 ~ car_Info_dataset_scrubbed$V2 + car_Info_dataset_scrubbed$V3)
summary(model_car_Info_dataset_2)

abline(model_car_Info_dataset_2,col = "red")
plot(model_car_Info_dataset_2)

termplot(model_car_Info_dataset_2)






###http://mlr.cs.umass.edu/ml/machine-learning-databases/car/car.data
car_Eval_dataset = read.csv(url("http://mlr.cs.umass.edu/ml/machine-learning-databases/car/car.data"), header=FALSE)

set.seed(1728)
ind <- sample(2, nrow(car_Eval_dataset), replace=T, prob=c(0.7, 0.3))
car_Eval_dataset.train <- car_Eval_dataset[ind==1, ]
car_Eval_dataset.test <- car_Eval_dataset[ind==2, ]


car_Eval_dataset.formula <- car_Eval_dataset$V1 ~ car_Eval_dataset$V2 + car_Eval_dataset$V3 + car_Eval_dataset$V4 + car_Eval_dataset$V5 + car_Eval_dataset$V6 + car_Eval_dataset$V7
car_Eval_dataset.ctree <- ctree(car_Eval_dataset.formula, data=car_Eval_dataset.train)
car_Eval_dataset.ctree

plot(car_Eval_dataset.ctree)
pred <- predict(car_Eval_dataset.ctree, newdata = car_Eval_dataset.test)

table(pred, car_Eval_dataset$V2,dnn = c("pred", "V1"))







      