library(ggplot2)

mytheme = theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank())

theta1_seq <- seq(0, 2,length.out =  1000)
theta2_seq <- seq(-0.5,3,length.out =  1000)
data = expand.grid(x = theta1_seq,y = theta2_seq,KEEP.OUT.ATTRS = FALSE)


response = function(theta1,theta2){
  return(0.5*(theta1^2-theta2)^2+0.5*(theta1-1)^2)
}

data = cbind(data, response = response(data$x,data$y))

plot = ggplot(data, aes(x = x, y = y, z=response)) + stat_contour(binwidth = 0.2)+mytheme


searching = data.frame(theta1=rep(0,100),theta2=rep(0,100),response=rep(0,100))

theta1 = 0
theta2 = 0

for(i in 1:100){
  theta1_new = theta1 - ((1/((6*(theta1^{2}))-(2*theta2)+1)))*(2*theta1^{3}-2*theta1*theta2+theta1-1)
  theta2_new = theta2 - (1)*(-(theta1^2)+theta2)
  response_value = response(theta1_new,theta2_new)
  
  theta1 = theta1_new
  theta2 = theta2_new
  
  cat("Iteration:", i,"\n")
  cat("theta1: ", theta1, "\n")
  cat("theta2: ", theta2, "\n")
  cat("Response: ", response_value,"\n")
  searching[i,1] = theta1
  searching[i,2] = theta2
  searching[i,3] = response_value
}

plot + geom_path(data=searching, aes(x=theta1,y=theta2))
