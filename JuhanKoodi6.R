#This is where we're going through Homework 3, Juha's way:
#e1
x1<-0.7579/0.3137
x2<-0.5033/1.8
x3<-0.2873*1.179
x4<-2*(1-pnorm(1.179))

#e2a
Y0<-matrix(c(27,44,95,443),ncol=2) #Counts of ill vs healthy people in each catecholamine group
CAT0<-c(1,0) #Catecholamine yes/no
cbind(Y0,CAT0) #bind the table together and view it
logreg0<-glm(Y0~CAT0,family=binomial(link=logit))
summary(logreg0)
#Whoa didnt know you could do it like this. Use glm on a contingency table of counts. How does glm figure it all out?

Y<-matrix(c(1,17,3,7,9,15,14,5,7,257,14,52,30,107,44,27),ncol=2)
CAT<-rep(c(1,0),times=4)
AGE<-c(0,0,0,0,1,1,1,1)
ECG<-rep(c(0,0,1,1),times=2)
cbind(Y,CAT,AGE,ECG)
logreg1<-glm(Y~CAT+AGE+ECG,family=binomial(link=logit))

#e3
logreg2<-glm(Y~CAT+AGE+ECG+AGE*ECG,family=binomial(link=logit))
#Juha says don't declare one model the better than the other here, just report the results of both, and how they differ.

#e4
a1<-c(53,10,212,50,60,19,155,65,112,77,118,68,35,46,8,12)
a2<-c(6,4,52,10,14,10,54,27,33,80,46,78,6,48,8,31)
A<-cbind(a2,a1)
age<-rep(c(0:3),each=4)
high<-rep(c(0,0,1,1),times=4)
more<-rep(c(1,0),times=8)
cbind(A,age,high,more)
tapply(a1+a2,age,sum) #Oh so this displays a table.
la1<-glm(A~high+more+factor(age),family=binomial(link=logit))
la2<-glm(A~high*more+factor(age),family=binomial(link=logit)) #The interaction between education and wanting more kids
#Seems to improve the model clearly. At least it looks like that before crossvalidation.
la3<-glm(A~high*more+more*factor(age),family=binomial(link=logit))
#so here more:age1 is the effect of age for those who want more kids, whereas just age1 is the effect of age on
# those who don't. "Its joint effect with age"
moreage<-more*age #New variable
la4<-glm(A~high*more+factor(age)+moreage,family=binomial(link=logit))
#Residual deviance is apparently the same test statistic as the chi-square. (??????)

F1<-c(77,30,14,15,43,36,27,41)
F2<-c(32,36,19,27,20,37,36,118)
FAV<-cbind(F1,F2)
CLOSE<-rep(c(1,0),each=4)
FREQUENT<-c(1,1,0,0,1,1,0,0)
FAVORABLE<-rep(c(1,0),times=4)
cbind(FAV,CLOSE,FREQUENT,FAVORABLE)
sent1<-glm(FAV~CLOSE+FREQUENT+FAVORABLE,family=binomial)
sent2<-glm(FAV~FREQUENT*FAVORABLE,family=binomial)
sent3<-glm(FAV~FREQUENT+FAVORABLE,family=binomial)

#Residuals:
phat<-fitted.values(logreg1) #These are the probabilities/proportions calculated by the model.
yhat<-rowSums(Y)*phat #Using the 2x2 matrix counting the successes and failures of each covariate pattern and the fitted probabilities,
# this calculates the expected number of successes for each covariate pattern.
cbind(Y[,1],yhat) #This juxtaposes the observed and expected success counts of each covariate pattern.
Ri<-(Y[,1]-yhat)/sqrt(yhat*(1-phat)) #This is the Pearson residual. observed-expected divided by variance I think.
#Deviance Residual:
di<-sign(Y[,1]-yhat)*sqrt(2)*sqrt(Y[,1]*log(Y[,1]/yhat)+Y[,2]*log(Y[,2]/(rowSums(Y)-yhat))) 
cbind(Ri,di)
