N <- 50
Alpha <- c(rep(0,times=17),rep(-1.6,times=17),rep(1.6,times=16))
Beta <- c(0.5, 0.1)
x <- 30+4*rnorm(N)
x1 <- x-mean(x)
G <- c(rep(0,round(N/3)),rep(1,N-round(N/3)))
RandEff <- c(rep("RE1",times=17),rep("RE2",times=17),rep("RE3",times=16))
logodds <- Alpha + G*Beta[1] + x1*Beta[2]
p <- exp(logodds)/(1+exp(logodds))
U <- runif(N)
y <- as.numeric(p>U)
OurData<-data.frame(Alpha, G, x1, RandEff, logodds, p, U, y)
summary(CondLog<-clogit(y~G+x1+strata(RandEff),data=OurData))
mean(y)



#Intento dos
N<-1001
x<-rnorm(n=1001,mean=0,sd=2)
G <- c(rep(0,round(N/3)),rep(1,N-round(N/3)))
Alpha <- c(rep(0,times=333),rep(-1.6,times=334),rep(1.6,times=334))
Beta <- c(0.75, -0.25)
RandEff <- c(rep("G1",times=333),rep("G2",times=334),rep("G3",times=334))
logodds <- Alpha + G*Beta[1] + x*Beta[2]
p <- exp(logodds)/(1+exp(logodds))
U <- runif(N)
y <- as.numeric(p>U)
OurData<-data.frame(Alpha, G, x, RandEff, logodds, p, U, y)
summary(CondLog<-clogit(y~G+x+strata(RandEff),data=OurData))
mean(y)