library(ISLR)

# sigmoid function as 
sigmoid = function(x,beta){
  return(1/(1+exp(-(x %*% beta))))
}

loglikelihood = function(y,x,beta){
  sum = 0
  for(i in 1:length(y)){
    l_i = y[i]*log(sigmoid(x[i,],beta)) + (1-y[i]) * log(1-sigmoid(x[i,],beta))
    sum = sum + l_i
  }
  return(sum)
}

adjustedResponse = function(y,x,beta){
  return(solve(t(x) %*% w %*% x) %*% t(x) %*% (y-p))
}



data = Smarket[,c('Lag1','Lag2','Lag3','Lag4','Lag5','Volume')]
data = cbind(x_0 = rep(1, nrow(data)), data)
data = as.matrix(data)
y = Smarket[,c("Direction")]
y = cbind(y,logical = y %in% "Up")
y = y[,2]

#beta = matrix(runif(7),nrow = 7)
beta = matrix(rep(0,7),nrow = 7)


result = rep(0,10)
beta_result = matrix(rep(0,7),nrow=1)

for(i in 1:10){
  p = sigmoid(data, beta)
  w = diag(as.vector(p*(1-p)))
  beta = beta + adjustedResponse(y,data,beta)
  beta_result = rbind(beta_result, t(beta))
  result[i] = loglikelihood(y,data,beta)
}