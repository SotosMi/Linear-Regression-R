# Linear Reggression

#MSE example

library("Metrics")
data("mtcars")

model<-lm(mpg~disp+hp,data = mtcars)

#Calculate MSE
mse(mtcars$mpg,predict(model,mtcars))

#MODEL FITTING HW4
# Split mtcars 75% , 25% seed 321
# mpg dependent var , wt independent

attach(mtcars)
size<-0.75*nrow(mtcars)
set.seed(321)

train_ind<-sample(nrow(mtcars),size=size)
train<-mtcars[train_ind, ]
test<-mtcars[-train_ind, ]

model<-lm(train$mpg ~ train$wt)

model$coefficients

#use coeffs to predict the values of mpg of the test set

yp<-model$coefficients[[2]]*test$wt+model$coefficients[[1]]
yp

#mse
mes<-mean((yp-test$mpg)^2)
mes

#same
mse(test$mpg,yp)


#MY sOLUTION
sample_size <- floor(0.75 * nrow(mtcars)) # choosing the 75% of mtcars
set.seed(321) # setting the seed to 321
training <- sample(seq_len(nrow(mtcars)), size = sample_size) 

train <- mtcars[training, ] #training set
test <- mtcars[-training, ] # test set

dim(train)

#carry out linear regression, using lm(), on the training set between wt,
#used as the independent variable, and  mpg, used as the dependent variable
model2 <-lm(mpg ~  wt, data = train)
model2

#using the model for prediction of the test set

prediction <- predict(model2 ,  newdata = test )
prediction


#Calculating the mean square error

MSR <- mean((test$mpg - prediction)^2)
MSR

#Exercise HW6
#Use the Auto dataset from the ISLR package to perform linear regression
#with mpg as the response and displacement as the predictor. 

#to calculate analytically the parameters (a,b) of the linear regression model 
#f(x) = a + b*x by using the normal equations

library(ISLR)
attach(Auto)

X<-Auto$displacement #(predictor/INDEPENDENT)
Y<-Auto$mpg #response/Dependent


A <- rbind(c(length(X),sum(X)), 
           c(sum(X), sum(X^2)))


B <- c(sum(Y),sum(X*Y))

solve(A,B)


model3 = lm(mpg ~ displacement,Auto) #lm(y~x,data) 
# y = dependent/response , x = independent/predictor
summary(model3)



  
