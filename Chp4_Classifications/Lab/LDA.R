
library(MASS)
train = Smarket$Year<2005
lda.fit = lda(Direction~Lag1+Lag2, data= Smarket, subset= train, method="mle")
lda.pred = predict(lda.fit)
data = cbind(data,lda.pred$class) 

#======================================================================================================


data = Smarket[train,c('Lag1','Lag2','Direction')]
data_0 = data[data$Direction == "Down",c('Lag1','Lag2')]
data_1 = data[data$Direction == "Up",c('Lag1','Lag2')]
mean_0 = as.matrix(apply(data_0,2,mean), nrow=2)
mean_1 = as.matrix(apply(data_1,2,mean), nrow=2)
pi_0 = nrow(data[data$Direction=="Down",])/nrow(data)
pi_1 = nrow(data[data$Direction=="Up",])/nrow(data)
S = ((nrow(data_0)-1)*cov(data_0) + (nrow(data_1)-1)*cov(data_1))/(nrow(data)-2)
#======================================================================================================

classifier = function(x,S,mean_0,mean_1,pi_0,pi_1){
  x = as.matrix(x)
  delta_0 = x %*% solve(S) %*% mean_0 - 0.5 * t(mean_0) %*% solve(S)%*% mean_0 + log(pi_0)
  delta_1 = x %*% solve(S) %*% mean_1 - 0.5 * t(mean_1) %*% solve(S)%*% mean_1 + log(pi_1)
  
  if(delta_0 > delta_1){
    return("Down")
  }else{
    return("Up")
  }
}

predict = data.frame(predict = rep(0,998))

for(i in 1:998){
  predict[i,1] = classifier(data[i,1:2],S,mean_0,mean_1,pi_0,pi_1)
}
