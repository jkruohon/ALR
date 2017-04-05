#logreg2016C3

#1. Generate logistic regression data with one explanatory variable (= x1) and one
#group variable (= x2)

N<-15
n<-2*N+1
x1<-c(-N:N)/N
x2<-c(rep(0,round(n/3)),rep(1,n-round(n/3)))
x0<-rep(1,n)
X<-cbind(x0,x1,x2)
B<-c(log(0.3/0.7),1,-0.3)
lp<-X%*%B
p<-exp(lp)/(1+exp(lp))
U<-runif(n)
y<-as.numeric(U<p)
#plot the obervations and the true probabilities against x1
plot(x1,y)
lines(x1,p,lty=3)

#2. Compute ML estimates using Newton's method
#set starting values
p1<-sum(y)/n
B0<-c(log(p1/(1-p1)),0,0)
#iterate the following until estimates do not change
lp0<-X%*%B0
p0<-exp(lp0)/(1+exp(lp0))
dl<-t(X)%*%(y-p0)
w<-as.vector(p0*(1-p0))
d2l<--t(X)%*%diag(w)%*%X
B1<-B0-solve(d2l,dl)
B0<-B1
B1

#3. Fit the same model to the same data using the glm-function
Y<-cbind(y,1-y)
logreg<-glm(Y~x1+x2,family=binomial)
summary(logreg)

#4. Covariance matrix of the estimated coefficients
V<--solve(d2l)
vcov(logreg)

#5. Compute standard errors of the coefficient estimates
sqrt(diag(V))
sqrt(diag(vcov(logreg)))

#6. Fit the model with only the continuous explanatory variable x1
logreg1<-glm(Y~x1,family=binomial)

#7. Fit the model with only the group variable x2
logreg2<-glm(Y~x2,family=binomial)

#8. What explains the differences between the three model fits?
tapply(x1,x2,mean)

