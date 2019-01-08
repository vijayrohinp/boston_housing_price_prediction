# To predict Medv(Y-dependent variable): median value of owner-occupied homes in $1000s for given all the X variables.

# @author: Vijay Rohin Periaiah

# To import needed libraries
library(neuralnet)

# To read the data from the given dataset
data = read.csv("boston_house_prices.csv", header=T)

# To create a copy of data as train and test datasets
datatrain = data
datatest = data

# To apply neural network, we need to scale all the variables in the same format

max = apply(data, 2, max)
min = apply(data, 2, min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))


# To create a copy of scaled data as train and test datasets
train_scaled_data = scaled
test_scaled_data = scaled

# To fit neural network with given dataset
neural_network_obj = neuralnet(MEDV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, train_scaled_data, hidden = 10 , linear.output = T )

# To plot neural network
plot(neural_network_obj)

# To perform prediction using neural network

predict_test_dataset = compute(neural_network_obj, test_scaled_data[,c(1:13)])
predict_test_dataset = (predict_test_dataset$net.result * (max(data$MEDV) - min(data$MEDV))) + min(data$MEDV)

plot(datatest$MEDV, predict_test_dataset, col='blue', pch=16, ylab = "Predicted MEDV", xlab = "Original MEDV")

abline(0,1)

# To calculate Root Mean Square Error (RMSE)
rms = (sum((datatest$MEDV - predict_test_dataset)^2) / nrow(datatest)) ^ 0.5
rms
# Here the RMS value for 10 hidden network is 1.61575
# Therefore based on the number of hidden network RMS varies.

###############################################################################
