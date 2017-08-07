#Logistic Regression
train <- read.csv('mnist_train.csv', header = FALSE)
test <- read.csv('mnist_test.csv', header = FALSE)

test_0_1 = test[,1:2115]
test_0_1 = data.frame(test_0_1)
test_3_5 = test[,2116:4017]
test_3_5 = data.frame(test_3_5)

train_0_1 = train[,1:12665]
train_0_1 = data.frame(train_0_1)
train_3_5 = train[,12666:24217]
train_3_5 = data.frame(train_3_5)

true_label_test_0_1 = test_0_1[785,]
true_label_test_0_1 = data.frame(true_label_test_0_1)
test_0_1 = test_0_1[1:784,]

true_label_test_3_5 = test_3_5[785,]
true_label_test_3_5 = data.frame(true_label_test_3_5)
test_3_5 = test_3_5[1:784,]

true_label_train_0_1 = train_0_1[785,]
true_label_train_0_1 = data.frame(true_label_train_0_1)

train_0_1 = train_0_1[1:784,]

true_label_train_3_5 = train_3_5[785,]
true_label_train_3_5 = data.frame(true_label_train_3_5)
train_3_5 = train_3_5[1:784,]
#View (train_3_5)

rotate <- function(x) t(apply(x, 2, rev))

#Test test_0_1 it is a 0
test_0_matrix = test_0_1[,1]

test_0_matrix = matrix(test_0_matrix, nrow=28,ncol=28)

test_0_matrix = rotate(test_0_matrix)
image(test_0_matrix, col = grey.colors(255))

#Test train_0_1 is a 1
test_1_matrix = train_0_1[,10003]
test_1_matrix = matrix(test_1_matrix, nrow=28,ncol=28)
test_1_matrix = rotate(test_1_matrix)
image(test_1_matrix, col = grey.colors(255))


#test3_5 number is a 3
test_3_matrix = test_3_5[,2]
test_3_matrix = matrix(test_3_matrix, nrow=28,ncol=28)

test_3_matrix = rotate(test_3_matrix)
image(test_3_matrix, col = grey.colors(255))

#train 3_5 number is a 5
test_5_matrix = train_3_5[,10000]
test_5_matrix = matrix(test_5_matrix, nrow=28,ncol=28)

test_5_matrix = rotate(test_5_matrix)
image(test_5_matrix, col = grey.colors(255))


train <- read.csv('mnist_train.csv', header = FALSE)
test <- read.csv('mnist_test.csv', header = FALSE)

test_0_1 = test[,1:2115]
test_0_1 = data.frame(test_0_1)

train_0_1 = train[,1:12665]
train_0_1 = data.frame(train_0_1)

true_label_test_0_1 = test_0_1[785,]
true_label_test_0_1 = data.frame(true_label_test_0_1)
test_0_1 = test_0_1[1:784,]

true_label_train_0_1 = train_0_1[785,]
true_label_train_0_1 = data.frame(true_label_train_0_1)

train_0_1 = train_0_1[1:784,]

sig = function(h) {
  #sigmoid function
  return(1 / (1 + exp(-1 * h)))
}

grad = function(trainx, trainy, theta) {
  #partial dirivitave of cost function
  #transpose train x
  #put trainx and theta through the sigmoid function and subtract trainy
  #find the dot product of the two calculations
  grad = (t(trainx) %*% (sig(trainx %*% theta) - trainy))
  
  return (grad)
}

gradient_decent = function(trainx, trainy, theta, threshold, iterations, alpha) {
  #Initialize cost hist to True
  cost_hist = TRUE
  increments = 0
  #Continue to run untill convergence criteria is met
  while(increments < iterations & cost_hist == TRUE){
        # Calculate gradient partial derivitive
        cost = grad(trainx, trainy, theta)
        cost_hist_1 = as.matrix(cost)
        #Sum the absolute value of the partial drivitive of the gradient for convergence criteria
        cost_hist_1 = sum(abs(cost_hist))
        # Updata all values of theta 
        theta = theta - alpha * cost
        #Increment the increments for convergence criteria
        increments = increments  + 1
        if(cost_hist_1 < threshold){
          cost_hist = FALSE
        }
  }
  return (theta)
}

gradient_decent2 = function(trainx, trainy, theta, threshold, iterations, alpha) {
  theta_hist = TRUE
  increments = 0
  while(increments < iterations & theta_hist == TRUE){
    cost = grad(trainx, trainy, theta)
    theta_updated = theta - alpha * cost
    increments = increments  + 1
    theta_check = sum(abs(theta_updated - theta))
    #print (theta_check)
    if(theta_check < threshold){
      theta_hist = FALSE
    }
    theta = theta_updated
  }
  return (theta_updated)
}


predict = function(theta, x, predict) {
  predictions = predict
  predictions[sig(x %*% theta) >= 0.5] = 1
  return (predictions)
}

trainx = data.frame(t(train_0_1))
trainy= data.frame(t(true_label_train_0_1))
columnsInTrainx = (nrow(trainx))
rowsInTrainX = (ncol(trainx))


testx = data.frame(t(test_0_1))
testy = data.frame(t(true_label_test_0_1))
columnsInTestX = (nrow(testx))
trainx = as.matrix(trainx)
trainy = as.matrix(trainy)
#added
testy = as.matrix(testy)

trainx = cbind(rep(1,columnsInTrainx),trainx)

thetaStart = rep(0,rowsInTrainX + 1)

testx = as.matrix(testx)
testx = cbind(rep(1,columnsInTestX),testx)

#Problem 3a: threshold: .001, max_iters: 200, alpha: .0001
#Test/train tests for 0/1
thetaTest_1 = gradient_decent(trainx, trainy, thetaStart, .0001, 200, .0001)

predictansTrain_1 = predict(thetaTest_1,trainx, rep(0,dim(trainx)[1]))
print ("Problem 2 Train Accuracy 0/1: threshold: .001, max_iters: 200, alpha: .0001")
print (mean(predictansTrain_1 == trainy) * 100)
print ("Problem 2 Test Accuracy 0/1: threshold: .001, max_iters: 200, alpha: .0001")
predictansTest_1 = predict(thetaTest_1, testx, rep(0,dim(testx)[1]))
print (mean(predictansTest_1 == testy) * 100)


#Problem 3a: threshold: .01, max_iters: 400, alpha: .001
#Test/train tests for 5/3
test_3_5 = test[,2116:4017]
test_3_5 = data.frame(test_3_5)
train_3_5 = train[,12666:24217]
train_3_5 = data.frame(train_3_5)
true_label_train_3_5 = train_3_5[785,]
true_label_train_3_5 = data.frame(true_label_train_3_5)
train_3_5 = train_3_5[1:784,]
true_label_test_3_5 = test_3_5[785,]
true_label_test_3_5 = data.frame(true_label_test_3_5)
test_3_5 = test_3_5[1:784,]




trainx_3_5 = data.frame(t(train_3_5))
trainy_3_5 = data.frame(t(true_label_train_3_5))

testx_3_5 = data.frame(t(test_3_5))
testy_3_5 = data.frame(t(true_label_test_3_5))

columnsInTrainx_3_5 = (nrow(trainx_3_5))
rowsInTrainX_3_5 = (ncol(trainx_3_5))
columnsInTestX_3_5 = (nrow(testx_3_5))


trainx_3_5 = as.matrix(trainx_3_5)
trainy_3_5 = as.matrix(trainy_3_5)

testx_3_5 = as.matrix(testx_3_5)
testy_3_5 = as.matrix(testy_3_5)


trainx_3_5 <- cbind(rep(1,columnsInTrainx_3_5),trainx_3_5)

initial_theta_3_5 <- rep(0,rowsInTrainX_3_5 + 1)

testx_3_5 = cbind(rep(1,columnsInTestX_3_5),testx_3_5)

trainy_3_5 = ifelse(trainy_3_5 == 3, 0, 1)
testy_3_5 = ifelse(testy_3_5 == 3, 0, 1)

thetaTrain3_5 = gradient_decent(trainx_3_5, trainy_3_5, initial_theta_3_5, .0001, 200, .0001)

predictansTrain_3_5 = predict(thetaTrain3_5, trainx_3_5, rep(0,dim(trainx_3_5)[1]))
print ("Problem 2 Train Accuracy 3/5: threshold: .01, max_iters: 400, alpha: .001")
print (mean(predictansTrain_3_5 == trainy_3_5) * 100)

predictansTest_3_5 = predict(thetaTrain3_5,testx_3_5, rep(0,dim(testx_3_5)[1]))
print ("Problem 2 Test Accuracy 3/5: threshold: .01, max_iters: 400, alpha: .001")
print (mean(predictansTest_3_5 == testy_3_5) * 100)

#Problem 3b:

train_0_1_10_runs_acc = 0
test_0_1_10_runs_acc = 0
for (i in 1:10){
train_0_1 = train[,1:12665]
train_0_1 = data.frame(train_0_1)
train_0_1 = data.frame(t(train_0_1))
train_indexes = sample(1:nrow(train_0_1), size=0.2*nrow(train_0_1))

trainx = data.matrix(train_0_1[-train_indexes, ])
trainy = trainx[, ncol(trainx)]
trainx = (trainx[, -ncol(trainx)])

rowsInTrainXSample_0_1 = (nrow(trainx))
columnsInTrainXSample_0_1 = (ncol(trainx))

trainx = cbind(rep(1,rowsInTrainXSample_0_1),trainx)

thetaStart = rep(0,columnsInTrainXSample_0_1+1)

thetatrain = gradient_decent(trainx, trainy, thetaStart, .0001, 200, .0001)
train_0_1_10_runs_prediction = predict(thetatrain,trainx, rep(0,dim(trainx)[1]))
accuracy_train = mean(train_0_1_10_runs_prediction == trainy) * 100
cat("Train Accuracy for 1/0 run: ",i)
print (accuracy_train)
train_0_1_10_runs_acc = train_0_1_10_runs_acc + accuracy_train

test_0_1_10_runs_prediction = predict(thetatrain, testx, rep(0,dim(testx)[1]))
accuracy_test = mean(test_0_1_10_runs_prediction == testy) * 100
cat("Test Accuracy for 1/0 run: ",i)
print (accuracy_test)
test_0_1_10_runs_acc = test_0_1_10_runs_acc + accuracy_test
}

print (train_0_1_10_runs_acc / 10)
print (test_0_1_10_runs_acc / 10)


train_3_5_10_runs_acc = 0
test_3_5_10_runs_acc = 0
for (i in 1:10){
  train_3_5 = train[,12666:24217]
  train_3_5 = data.frame(train_3_5)
  train_3_5 = data.frame(t(train_3_5))
  train_indexes = sample(1:nrow(train_3_5), size=0.2*nrow(train_3_5))
  
  trainx = data.matrix(train_3_5[-train_indexes, ])
  trainy = trainx[, ncol(trainx)]
  trainx = (trainx[, -ncol(trainx)])
  
  rowsInTrainXSample_3_5 = (nrow(trainx))
  columnsInTrainXSample_3_5 = (ncol(trainx))

  trainx = cbind(rep(1,rowsInTrainXSample_3_5),trainx)
  
  thetaStart = rep(0,columnsInTrainXSample_3_5+1)
  trainy_3_5 = ifelse(trainy == 3, 0, 1)

  
  thetatrain = gradient_decent(trainx, trainy_3_5, thetaStart, .0001, 200, .0001)
  train_3_5_10_runs_prediction = predict(thetatrain, trainx, rep(0,dim(trainx)[1]))
  accuracy_train = mean(train_3_5_10_runs_prediction == trainy_3_5) * 100
  cat("Train Accuracy for 3/5 run: ",i)
  print (accuracy_train)
  train_3_5_10_runs_acc = train_3_5_10_runs_acc + accuracy_train
  
  test_3_5_10_runs_prediction = predict(thetatrain, testx_3_5, rep(0,dim(testx_3_5)[1]))
  accuracy_test = mean(test_3_5_10_runs_prediction == testy_3_5) * 100
  cat("Test Accuracy for 3/5 run: ",i)
  print (accuracy_test)
  test_3_5_10_runs_acc = test_3_5_10_runs_acc + accuracy_test
}
print (train_3_5_10_runs_acc / 10)
print (test_3_5_10_runs_acc / 10)

#Problem 4a
# Initialization: theta now initialized randomly from 0 to 1 instead of 1 like in 3b
train_3_5_10_runs_acc_4_a = 0
test_3_5_10_runs_acc_4_a = 0
for (i in 1:10){
  train_3_5 = train[,12666:24217]
  train_3_5 = data.frame(train_3_5)
  train_3_5 = data.frame(t(train_3_5))
  train_indexes = sample(1:nrow(train_3_5), size=0.2*nrow(train_3_5))
  
  trainx = data.matrix(train_3_5[-train_indexes, ])
  trainy = trainx[, ncol(trainx)]
  trainx = (trainx[, -ncol(trainx)])
  
  rowsInTrainXSample_3_5_4a = (nrow(trainx))
  columnsInTrainXSample_3_5_4a = (ncol(trainx))
  
  trainx = cbind(rep(1,rowsInTrainXSample_3_5_4a),trainx)
  random_theta = sample(0:1,1)
  thetaStart = rep(random_theta,columnsInTrainXSample_3_5_4a + 1)
  trainy_3_5 = ifelse(trainy == 3, 0, 1)
  
  
  thetatrain = gradient_decent(trainx, trainy_3_5, thetaStart, .0001, 400, .0001)
  train_3_5_10_runs_prediction = predict(thetatrain, trainx, rep(0,dim(trainx)[1]))
  accuracy_train = mean(train_3_5_10_runs_prediction == trainy_3_5) * 100
  cat("Train Accuracy for 3/5 run: ",i)
  print (accuracy_train)
  train_3_5_10_runs_acc_4_a  = train_3_5_10_runs_acc_4_a  + accuracy_train
  
  test_3_5_10_runs_prediction = predict(thetatrain, testx_3_5, rep(0,dim(testx_3_5)[1]))
  accuracy_test = mean(test_3_5_10_runs_prediction == testy_3_5) * 100
  cat("Test Accuracy for 3/5 run: ",i)
  print (accuracy_test)
  test_3_5_10_runs_acc_4_a = test_3_5_10_runs_acc_4_a + accuracy_test
}
print (train_3_5_10_runs_acc_4_a / 10)
print (test_3_5_10_runs_acc_4_a / 10)


#Problem 4b
#changed convergence criteria, threshold from .0001 to .01
train_3_5_10_runs_acc_4_b = 0
test_3_5_10_runs_acc_4_b = 0
for (i in 1:10){
  train_3_5 = train[,12666:24217]
  train_3_5 = data.frame(train_3_5)
  train_3_5 = data.frame(t(train_3_5))
  train_indexes = sample(1:nrow(train_3_5), size=0.2*nrow(train_3_5))
  
  trainx = data.matrix(train_3_5[-train_indexes, ])
  trainy = trainx[, ncol(trainx)]
  trainx = (trainx[, -ncol(trainx)])
  
  rowsInTrainXSample_3_5_4b = (nrow(trainx))
  columnsInTrainXSample_3_5_4b = (ncol(trainx))

  trainx = cbind(rep(1,rowsInTrainXSample_3_5_4b),trainx)
  thetaStart = rep(0,columnsInTrainXSample_3_5_4b + 1)
  trainy_3_5 = ifelse(trainy == 3, 0, 1)
  
  
  thetatrain = gradient_decent2(trainx, trainy_3_5, thetaStart, .0001, 200, .1)
  train_3_5_10_runs_prediction = predict(thetatrain, trainx, rep(0,dim(trainx)[1]))
  accuracy_train = mean(train_3_5_10_runs_prediction == trainy_3_5) * 100
  cat("Train Accuracy for 3/5 run: ",i)
  print (accuracy_train)
  train_3_5_10_runs_acc_4_b  = train_3_5_10_runs_acc_4_b  + accuracy_train
  
  test_3_5_10_runs_prediction = predict(thetatrain, testx_3_5, rep(0,dim(testx_3_5)[1]))
  accuracy_test = mean(test_3_5_10_runs_prediction == testy_3_5) * 100
  cat("Test Accuracy for 3/5 run: ",i)
  print (accuracy_test)
  test_3_5_10_runs_acc_4_b = test_3_5_10_runs_acc_4_b + accuracy_test
}
print (train_3_5_10_runs_acc_4_b / 10)
print (test_3_5_10_runs_acc_4_b / 10)


sizes = (seq(1,20,1))
training_accuracy_0_1 = vector("numeric", 20L)
testing_accuracy_0_1 = vector("numeric", 20L)
training_size = (seq(.95, 0, -.05))
train_acc_avg = 0
test_acc_avg = 0
for (i in 1:20){

  train_0_1 = train[,1:12665]
  train_0_1 = data.frame(train_0_1)
  train_0_1 = data.frame(t(train_0_1))
  for (l in 1:10){
    if(training_size[i] == 0){
      train_indexes = sample(train_0_1)
      
      trainx = (train_indexes)
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_0_1_5a = (nrow(trainx))
      columnsInTrainXSample_0_1_5a = (ncol(trainx))
      trainx = as.matrix(trainx)
      trainy = as.matrix(trainy)
      
      trainx = cbind(rep(1,rowsInTrainXSample_0_1_5a),trainx)
      
      thetaStart = rep(0,columnsInTrainXSample_0_1_5a+1)
  
      thetatrain = gradient_decent(trainx, trainy, thetaStart, .001, 200, .0001)
      train_0_1_10_runs_prediction = predict(thetatrain,trainx, rep(0,dim(trainx)[1]))
      accuracy_train = mean(train_0_1_10_runs_prediction == trainy) * 100
      cat("Train Accuracy for 1/0 run: ",i)
      print (accuracy_train)
      train_acc_avg = train_acc_avg + accuracy_train
      if(l == 10){
      training_accuracy_0_1[i] = train_acc_avg/10
      train_acc_avg = 0
      }
      test_0_1_10_runs_prediction = predict(thetatrain, testx, rep(0,dim(testx)[1]))
      accuracy_test = mean(test_0_1_10_runs_prediction == testy) * 100
      cat("Test Accuracy for 1/0 run: ",i)
      print (accuracy_test)
      test_acc_avg = test_acc_avg + accuracy_test
      if(l == 10){
      testing_accuracy_0_1[i] = test_acc_avg/10
      test_acc_avg = 0
      }
    }
    else{
    train_indexes = sample(1:nrow(train_0_1), size=training_size[i]*nrow(train_0_1), replace=FALSE)
    
    trainx = data.matrix(train_0_1[-train_indexes, ])
    trainy = trainx[, ncol(trainx)]
    trainx = (trainx[, -ncol(trainx)])
    rowsInTrainXSample_0_1_5a = (nrow(trainx))
    columnsInTrainXSample_0_1_5a = (ncol(trainx))

    trainx = cbind(rep(1,rowsInTrainXSample_0_1_5a),trainx)
    
    thetaStart = rep(0,columnsInTrainXSample_0_1_5a+1)
    thetatrain = gradient_decent(trainx, trainy, thetaStart, .001, 200, .0001)
    train_0_1_10_runs_prediction = predict(thetatrain,trainx, rep(0,dim(trainx)[1]))
    accuracy_train = mean(train_0_1_10_runs_prediction == trainy) * 100
    cat("Train Accuracy for 1/0 run: ",i)
    print (accuracy_train)
    train_acc_avg = train_acc_avg + accuracy_train
    if(l == 10){
      training_accuracy_0_1[i] = train_acc_avg/10
      train_acc_avg = 0
    }

    test_0_1_10_runs_prediction = predict(thetatrain, testx, rep(0,dim(testx)[1]))
    accuracy_test = mean(test_0_1_10_runs_prediction == testy) * 100
    cat("Test Accuracy for 1/0 run: ",i)
    print (accuracy_test)
    test_acc_avg = test_acc_avg + accuracy_test
    if(l == 10){
      testing_accuracy_0_1[i] = test_acc_avg/10
      test_acc_avg = 0
    }
    }
  }
}

library(ggplot2)
dfAccuracy_0_1 = data.frame(training_accuracy_0_1, testing_accuracy_0_1)
training_size_graph_seq = (seq(.05, 1, .05))
train_size_graph = data.frame(training_size_graph_seq)
#View(train_size_graph)
dfAccuracy_0_1 = dfAccuracy_0_1[seq(dim(dfAccuracy_0_1)[1],1),] 

ggplot(train_size_graph, aes(training_size_graph_seq)) + 
  geom_line(aes(y = dfAccuracy_0_1$training_accuracy_0_1, colour = "Training Accuracy")) + 
  geom_line(aes(y = dfAccuracy_0_1$testing_accuracy_0_1, colour = "Testing Accuracy"))+
  scale_x_continuous(breaks = pretty(train_size_graph$training_size_graph_seq, n = 20)) +
  xlab("Training sizes 5% to 100%") +
  ylab("Accuracy") +
  ggtitle("0/1 Learning Curve")


#problem 5a 0_3
sizes = (seq(1,20,1))
training_accuracy_3_5 = vector("numeric", 20L)
testing_accuracy_3_5 = vector("numeric", 20L)
training_size = (seq(.95, 0, -.05))
train_acc_avg_3_5 = 0
test_acc_avg_3_5 = 0
for (i in 1:20){
  train_3_5 = train[,12666:24217]
  train_3_5 = data.frame(train_3_5)
  train_3_5 = data.frame(t(train_3_5))
  for (l in 1:10){
    if(training_size[i] == 0){
      train_indexes = sample(train_3_5)
      
      trainx = (train_indexes)
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_3_5_5a = (nrow(trainx))
      columnsInTrainXSample_3_5_5a = (ncol(trainx))
      trainx = as.matrix(trainx)
      trainy = as.matrix(trainy)
      
      trainx = cbind(rep(1,owsInTrainXSample_3_5_5a),trainx)
      
      thetaStart = rep(0,columnsInTrainXSample_3_5_5a + 1)
      trainy_3_5 = ifelse(trainy == 3, 0, 1)
      thetatrain = gradient_decent(trainx, trainy_3_5, thetaStart, .0001, 200, .0001)
      train_0_1_10_runs_prediction = predict(thetatrain, trainx, rep(0,dim(trainx)[1]))
      accuracy_train = mean(train_0_1_10_runs_prediction == trainy_3_5) * 100
      cat("Train Accuracy for 3/5 run: ",i)
      print (accuracy_train)
      train_acc_avg_3_5 = train_acc_avg_3_5 + accuracy_train
      if(l == 10){
        training_accuracy_3_5[i] = train_acc_avg_3_5/10
        train_acc_avg_3_5 = 0
      }
      test_0_1_10_runs_prediction = predict(thetatrain, testx_3_5, rep(0,dim(testx_3_5)[1]))
      accuracy_test = mean(test_0_1_10_runs_prediction == testy_3_5) * 100
      cat("Test Accuracy for 3/5 run: ",i)
      print (accuracy_test)
      test_acc_avg_3_5 = test_acc_avg_3_5 + accuracy_test
      if(l == 10){
        testing_accuracy_3_5[i] = test_acc_avg_3_5/10
        test_acc_avg_3_5 = 0
      }
    }
    else{
      train_indexes = sample(1:nrow(train_3_5), size=training_size[i]*nrow(train_3_5), replace=FALSE)
      
      trainx = data.matrix(train_3_5[-train_indexes, ])
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_3_5_5a = (nrow(trainx))
      columnsInTrainXSample_3_5_5a = (ncol(trainx))

      trainx = cbind(rep(1,rowsInTrainXSample_3_5_5a),trainx)
      trainy_3_5 = ifelse(trainy == 3, 0, 1)
      thetaStart = rep(0,columnsInTrainXSample_3_5_5a + 1)
      
      thetatrain = gradient_decent(trainx, trainy_3_5, thetaStart, .0001, 200, .0001)
      train_0_1_10_runs_prediction = predict(thetatrain, trainx, rep(0,dim(trainx)[1]))
      accuracy_train = mean(train_0_1_10_runs_prediction == trainy_3_5) * 100
      cat("Train Accuracy for 3/5 run: ",i)
      print (accuracy_train)
      train_acc_avg_3_5 = train_acc_avg_3_5 + accuracy_train
      if(l == 10){
        training_accuracy_3_5[i] = train_acc_avg_3_5/10
        train_acc_avg_3_5 = 0
      }
      
      test_0_1_10_runs_prediction = predict(thetatrain, testx_3_5, rep(0,dim(testx_3_5)[1]))
      accuracy_test = mean(test_0_1_10_runs_prediction == testy_3_5) * 100
      cat("Test Accuracy for 3/5 run: ",i)
      print (accuracy_test)
      test_acc_avg_3_5= test_acc_avg_3_5 + accuracy_test
      if(l == 10){
        testing_accuracy_3_5[i] = test_acc_avg_3_5/10
        test_acc_avg_3_5= 0
      }
    }
  }
}

dfAccuracy_3_5 = data.frame(training_accuracy_3_5, testing_accuracy_3_5)
training_size_graph_seq = (seq(.05, 1, .05))
train_size_graph = data.frame(training_size_graph_seq)

dfAccuracy_3_5 = dfAccuracy_3_5[seq(dim(dfAccuracy_3_5)[1],1),] 
#View (dfAccuracy_3_5)
ggplot(train_size_graph, aes(training_size_graph_seq)) + 
  geom_line(aes(y = dfAccuracy_3_5$training_accuracy_3_5, colour = "Training Accuracy")) + 
  geom_line(aes(y = dfAccuracy_3_5$testing_accuracy_3_5, colour = "Testing Accuracy"))+
  scale_x_continuous(breaks = pretty(train_size_graph$training_size_graph_seq, n = 20))+ 
  xlab("Training sizes 5% to 100%") +
  ylab("Accuracy") +
  ggtitle("3/5 Learning Curve")

sizes = (seq(1,20,1))
training_accuracy_0_1_5b = vector("numeric", 20L)
testing_accuracy_0_1_5b = vector("numeric", 20L)
training_size = (seq(.95, 0, -.05))
train_acc_avg__01_5b = 0
test_acc_avg__01_5b = 0
for (i in 1:20){
  
  train_0_1 = train[,1:12665]
  train_0_1 = data.frame(train_0_1)
  train_0_1 = data.frame(t(train_0_1))
  for (l in 1:10){
    if(training_size[i] == 0){
      train_indexes = sample(train_0_1, replace=FALSE)
      
      trainx = (train_indexes)
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_0_1_5a = (nrow(trainx))
      columnsInTrainXSample_0_1_5a = (ncol(trainx))
      trainx = as.matrix(trainx)
      trainy = as.matrix(trainy)
      
      trainx = cbind(rep(1,rowsInTrainXSample_0_1_5a),trainx)
      
      thetaStart = rep(0,columnsInTrainXSample_0_1_5a+1)
      
      thetatrain = gradient_decent(trainx, trainy, thetaStart, .001, 200, .0001)

      accuracy_train = ((t(-trainy) %*% log(sig(trainx %*% thetatrain)) - t(1 - trainy) %*% log(1 - sig(trainx %*% thetatrain))) / length(trainy))
      accuracy_train = accuracy_train * 100
      train_acc_avg__01_5b = train_acc_avg__01_5b + accuracy_train
      if(l == 10){
        training_accuracy_0_1_5b[i] = train_acc_avg__01_5b/10
        train_acc_avg__01_5b = 0
      }
      accuracy_test = ((t(-testy) %*% log(sig(testx %*% thetatrain)) - t(1 - testy) %*% log(1 - sig(testx %*% thetatrain))) / length(testy))
      accuracy_test =  accuracy_test * 100
      test_acc_avg__01_5b = test_acc_avg__01_5b + accuracy_test
      if(l == 10){
        testing_accuracy_0_1_5b[i] = test_acc_avg__01_5b/10
        test_acc_avg__01_5b = 0
      }
    }
    else{
      train_indexes = sample(1:nrow(train_0_1), size=training_size[i]*nrow(train_0_1), replace=FALSE)
      
      trainx = data.matrix(train_0_1[-train_indexes, ])
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_0_1_5a = (nrow(trainx))
      columnsInTrainXSample_0_1_5a = (ncol(trainx))
      
      trainx = cbind(rep(1,rowsInTrainXSample_0_1_5a),trainx)
      
      thetaStart = rep(0,columnsInTrainXSample_0_1_5a+1)
      thetatrain = gradient_decent(trainx, trainy, thetaStart, .001, 200, .0001)
      accuracy_train = ((t(-trainy) %*% log(sig(trainx %*% thetatrain)) - t(1 - trainy) %*% log(1 - sig(trainx %*% thetatrain))) / length(trainy))

      accuracy_train = accuracy_train * 100
      train_acc_avg__01_5b = train_acc_avg__01_5b + accuracy_train
      if(l == 10){
        training_accuracy_0_1_5b[i] = train_acc_avg__01_5b/10
        ttrain_acc_avg__01_5b = 0
      }
      
      accuracy_test = ((t(-testy) %*% log(sig(testx %*% thetatrain)) - t(1 - testy) %*% log(1 - sig(testx %*% thetatrain))) / length(testy))
      accuracy_test= accuracy_test * 100
      test_acc_avg__01_5b = test_acc_avg__01_5b + accuracy_test
      if(l == 10){
        testing_accuracy_0_1_5b[i] = test_acc_avg__01_5b/10
        test_acc_avg__01_5b = 0
      }
    }
  }
}

library(ggplot2)
dfAccuracy_0_1 = data.frame(training_accuracy_0_1_5b/100,testing_accuracy_0_1_5b/100)
training_size_graph_seq = (seq(.05, 1, .05))
train_size_graph = data.frame(training_size_graph_seq)
#View(train_size_graph)
dfAccuracy_0_1 = dfAccuracy_0_1[seq(dim(dfAccuracy_0_1)[1],1),] 
View (dfAccuracy_0_1)
ggplot(train_size_graph, aes(training_size_graph_seq)) + 
  geom_line(aes(y = dfAccuracy_0_1$training_accuracy_0_1_5b, colour = "Training Loss")) + 
  geom_line(aes(y = dfAccuracy_0_1$testing_accuracy_0_1_5b, colour = "Testing Loss"))+
  scale_x_continuous(breaks = pretty(train_size_graph$training_size_graph_seq, n = 20)) +
  xlab("Training sizes 5% to 100%") +
  ylab("Loss") +
  ggtitle("0/1 Learning Curve")


sizes = (seq(1,20,1))
training_accuracy_3_5_5b = vector("numeric", 20L)
testing_accuracy_3_5_5b = vector("numeric", 20L)
training_size = (seq(.95, 0, -.05))
train_acc_avg_3_5_5b = 0
test_acc_avg_3_5_5b = 0
for (i in 1:20){
  train_3_5 = train[,12666:24217]
  train_3_5 = data.frame(train_3_5)
  train_3_5 = data.frame(t(train_3_5))
  for (l in 1:10){
    if(training_size[i] == 0){
      train_indexes = sample(train_3_5, replace =FALSE)
      
      trainx = (train_indexes)
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_3_5_5a = (nrow(trainx))
      columnsInTrainXSample_3_5_5a = (ncol(trainx))
      trainx = as.matrix(trainx)
      trainy = as.matrix(trainy)
      
      trainx = cbind(rep(1,owsInTrainXSample_3_5_5a),trainx)
      
      thetaStart = rep(0,columnsInTrainXSample_3_5_5a + 1)
      trainy_3_5 = ifelse(trainy == 3, 0, 1)
      thetatrain = gradient_decent(trainx, trainy_3_5, thetaStart, .0001, 200, .0001)
      accuracy_train = ((t(-trainy_3_5) %*% log(sig(trainx %*% thetatrain)) - t(1 - trainy_3_5) %*% log(1 - sig(trainx %*% thetatrain))) / length(trainy_3_5))
      cat("Train Accuracy for 3/5 run: ",i)
      print (accuracy_train)
      train_acc_avg_3_5_5b = train_acc_avg_3_5_5b + accuracy_train
      if(l == 10){
        training_accuracy_3_5_5b[i] = train_acc_avg_3_5_5b/10
        train_acc_avg_3_5_5b = 0
      }
      accuracy_test = ((t(-testy_3_5) %*% log(sig(testx_3_5 %*% thetatrain)) - t(1 - testy_3_5) %*% log(1 - sig(testx_3_5 %*% thetatrain))) / length(testy_3_5))
      cat("Test Accuracy for 3/5 run: ",i)
      print (accuracy_test)
      test_acc_avg_3_5_5b = test_acc_avg_3_5_5b + accuracy_test
      if(l == 10){
        testing_accuracy_3_5_5b[i] = test_acc_avg_3_5_5b/10
        test_acc_avg_3_5_5b = 0
      }
    }
    else{
      train_indexes = sample(1:nrow(train_3_5), size=training_size[i]*nrow(train_3_5), replace=FALSE)
      
      trainx = data.matrix(train_3_5[-train_indexes, ])
      trainy = trainx[, ncol(trainx)]
      trainx = (trainx[, -ncol(trainx)])
      rowsInTrainXSample_3_5_5a = (nrow(trainx))
      columnsInTrainXSample_3_5_5a = (ncol(trainx))
      
      trainx = cbind(rep(1,rowsInTrainXSample_3_5_5a),trainx)
      trainy_3_5 = ifelse(trainy == 3, 0, 1)
      thetaStart = rep(0,columnsInTrainXSample_3_5_5a + 1)
      
      thetatrain = gradient_decent(trainx, trainy_3_5, thetaStart, .0001, 200, .0001)
      accuracy_train = ((t(-trainy_3_5) %*% log(sig(trainx %*% thetatrain)) - t(1 - trainy_3_5) %*% log(1 - sig(trainx %*% thetatrain))) / length(trainy_3_5))
      cat("Train Accuracy for 3/5 run: ",i)
      print (accuracy_train)
      train_acc_avg_3_5_5b = train_acc_avg_3_5_5b + accuracy_train
      if(l == 10){
        training_accuracy_3_5_5b[i] = train_acc_avg_3_5_5b/10
        train_acc_avg_3_5_5b = 0
      }
      accuracy_test = ((t(-testy_3_5) %*% log(sig(testx_3_5 %*% thetatrain)) - t(1 - testy_3_5) %*% log(1 - sig(testx_3_5%*% thetatrain))) / length(testy_3_5))
      cat("Test Accuracy for 3/5 run: ",i)
      print (accuracy_test)
      test_acc_avg_3_5_5b= test_acc_avg_3_5_5b + accuracy_test
      if(l == 10){
        testing_accuracy_3_5_5b[i] = test_acc_avg_3_5_5b/10
        test_acc_avg_3_5_5b= 0
      }
    }
  }
}
dfAccuracy_3_5_5b = data.frame(training_accuracy_3_5_5b, testing_accuracy_3_5_5b)
training_size_graph_seq = (seq(.05, 1, .05))
train_size_graph = data.frame(training_size_graph_seq)

dfAccuracy_3_5_5b = dfAccuracy_3_5_5b[seq(dim(dfAccuracy_3_5_5b)[1],1),] 
#View (dfAccuracy_3_5)
ggplot(train_size_graph, aes(training_size_graph_seq)) + 
  geom_line(aes(y = dfAccuracy_3_5_5b$training_accuracy_3_5_5b, colour = "Training Loss")) + 
  geom_line(aes(y = dfAccuracy_3_5_5b$testing_accuracy_3_5_5b, colour = "Testing Loss"))+
  scale_x_continuous(breaks = pretty(train_size_graph$training_size_graph_seq, n = 20))+ 
  xlab("Training sizes 5% to 100%") +
  ylab("Loss") +
  ggtitle("3/5 Learning Curve")