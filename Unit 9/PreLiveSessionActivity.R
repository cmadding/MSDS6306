#Simulating fake data
x_1 <- rnorm(1000,5,7) #1000 Random numbers mean 5 and standard deviation of 7
hist(x_1, col = "gray") #plot p(x)
true_error <- rnorm(1000,0,2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2

y <- true_beta_0 + true_beta_1*x_1 + true_error
hist(y) #plot p(y)
plot(x_1,y, pch=20, col= "red") #plot p(x,y)