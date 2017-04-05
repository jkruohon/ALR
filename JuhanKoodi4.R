matriisi<-matrix(ncol=2,c(46,34,470,440))
k0<-matrix(ncol=2,c(6,30,30,360))
k1<-matrix(ncol=2,c(40,4,440,80))
Y0<-matriisi
logreg1<-glm(Y0~A0,family=binomial(link="logit"))
summary(logreg1) #tulee p = .316
chisq.test(Y0) #tulee 0.3747
chisq.test(Y0, correct=FALSE) #tulee .315.  Kaikissa vähän eri laskentametodi. Kolme eri tulosta.

(Y<-matrix(c(6,30,40,4,30,360,440,80),ncol=2))
(A<-rep(c(1,0),times=2))
(K<-rep(c(1,0),each=2))
cbind(Y,A,K) #look at it
logreg2<-glm(Y~A+K,family=binomial(link="logit"))
summary(logreg2)
coef(logreg2)
vcov(logreg2)
(b1<-coef(logreg2)[2])
(OR<-exp(b1))
(V1<-vcov(logreg2)[2,2])
(se1<-sqrt(V1))
(L0<-exp(b1-1.96*se1))
(U0<-exp(b1+1.96*se1))

#Confidence interval for the probability of "success for A=0, K=1
(L<-c(1,0,1)) #prediktöörien arvot
(lp<-sum(coef(logreg2)*L)) #lasketaan lineaarinen prediktööri. se on beetakerrointen summa.
(V<-vcov(logreg2))
SE2<-t(L)%*%V%*%L
SE<-sqrt(SE2)
(U<-exp(lp+1.96*SE))
(U<-U/(1+U)) #probability's upper bound
(Lo<-exp(lp-1.96*SE))
(Lo<-Lo/(1+Lo)) #probability's lower bound
(phat<-exp(lp)/(1+exp(lp))) #probability itself (the estimated one) 
#Huomaa ettei ole symmetrinen. Saattaa liittyä jotenkin logistiseen S-käyrään.

#CMH vastaan logreg
(M<-array(rep(0,8),dim=c(2,2,2)))
(M[,,1]<-Y[1:2,])
(M[,,2]<-Y[3:4,])
mantelhaen.test(M,correct=FALSE)
mantelhaen.test(M,correct=TRUE)
#All these tests, logistic regression included, rely on large-sample chi-squared estimation

#WOW suoraan leikepöydältä importtaus!!!! WHOA!
d<-read.table("clipboard",header=T)

#miekanotto mallinnus
SW<-glm(SW~SEX+AGE+factor(FAC),data=d,family=binomial(link="logit")) #faktorointi suoraan kaavaan!!! nopeampaa.
(AG<-round(d$AGE/10))
cbind(d$AGE,AG) #sitten sovelletaan siihen factor() tuohon kohortoituun ikään niin saadaan faktori jos halutaan.
