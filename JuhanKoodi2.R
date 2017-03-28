x<-(1:100)/100
gx<-exp(x)-1/x
plot(x,gx,type="l")
lines(x,0*x)
#derivaattoja? derivaatta risteymäkohdassa?
x0<-0.5 #alkuarvausarvo

#Newtonin algoritmi
x1<-x0-(exp(x0)-1/x0)/(exp(x0)+1/x0**2) #korjattu arvaus
x0<-x1
x0<-x1
#kun x0 ei enää muutu, "The model has converged". Alkuarvauksen on kuitenkin oltava oikean merkkinen.

N<-15
n<-2*N+1
x<-c(-N:N)/N
C<-rep(1,n)
X<-cbind(C,x)
B<-c(log(0.3/0.7),1)
lp<-X%*%B #Linear predictor coefficients. They are the sum of B0 and B1. 
p<-exp(lp)/(1+exp(lp))
U<-runif(n) #nää on ne observed values. eiku eipäs.
y<-as.numeric(U < p) #Nää on ne observed values. ykkösiä ja nollia. riippuen millanen [0,1] randomarpa tuli.
plot(x,y)
lines(x,p)

#Nyt taitaa tulla taas Newtonin sovellusta. Usein newtonissa käytetään alkuarvona interceptiä.
p1<-sum(y)/n
B0<-c(log(p1/(1-p1)),0) #alkuarvaukset

#Newton's algorithm:
lp0<-X%*%B0 #Linear predictor matrix multiplied by the coefficients.
p0<-exp(lp0)/(1+exp(lp0))
dl<-t(X)%*%(y-p0)
w<-as.vector(p0*(1-p0))
d2l<--t(X)%*%diag(w)%*%X
B1<-B0-solve(d2l,dl)
B1
B0<-B1 #Tätä toistetaan kunnes arvot eivät enää muutu ("converged")

Y<-cbind(y,1-y)
logreg<-glm(Y~x,family=binomial)
summary(logreg)
coef(logreg) #Tämä ottaa sen vektorina
coef(logreg)[2] #toka kerroin.
vcov(logreg) #varianssimatriisi
solve(-d2l) #tulee näköjään varianssimatriisi näinkin
sqrt(solve(-d2l)[2,2]) #täst tulee keskivirhe
