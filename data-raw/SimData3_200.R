## code to prepare `SimData3_200` dataset goes here
set.seed(2021)
num.cov<-4
beta_true <- c(1, -1, 0, 0)
samplesize<-200

# model 3_200
z <- matrix(rnorm(samplesize*num.cov, mean=0, sd=1), nrow=num.cov)
z <- round(z, digits=3)
u <- t(as.matrix(beta_true))%*%as.matrix(z)
y <- array(dim = samplesize)
for(i in 1:length(u)){
  uu <- u[i]
  if(uu<0){
    p <- 1/(1+exp(-uu/3))
  }
  if(uu>=0){
    p <- 1/(1+exp(-3*uu))
  }
  y[i] <- rbinom(1, 1, p)
}
sum(y)
SimData <- list(t(z), y)
names(SimData) <- c("X", "Y")
SimData3_200 <- SimData
usethis::use_data(SimData3_200, overwrite = TRUE)
