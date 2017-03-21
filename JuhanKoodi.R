x<-c(1:500)/20
plot(x,dchisq(x,1),type="l")
lines(x,dchisq(x,2),lt=2)  #add a dashed line! lisää kuvaajaan. vapausasteiden mukainen katkoviiva.
lines(x,dchisq(x,3),lt=3)  #3 vapausasteen mukainen katkoviiva.
lines(x,dchisq(x,10),lt=4) #10 vapausasteen mukainen katkoviiva

N<-100
n<-30
p<-0.3
y<-rbinom(N,n,p)
phat<-y/n
hist(phat)
SE<-sqrt(phat*(1-phat)/n) #keskivirhe
L<-phat-1.96*SE #lower bound
U<-phat+1.96*SE #upper bound
sum((L<p)*(U>p))
plot(c(0,1),c(1,N),type="n",xlab="p",ylab="Trial") #tyhjä koordinaatisto
for(i in 1:N){
  lines(c(L[i],U[i]),c(i,i))
}
lines(c(0.3,0.3),c(1,N))  #eli tämä havainnollistaa miten usein luottamusväli osuu oikean parametrin päälle.

m<-matrix(c(46,34,470,440),ncol=2) #näin tehdään matriiseja. Tää ottaa ne arvot sarakkeittain.
chisq.test(m)
chisq.test(m,correct=FALSE) #ilman continuity correctionia
c0<-chisq.test(m,correct=FALSE)
c0$observed #näyttää havaitut arvot
c0$expected #näyttää odotetut arvot (jos nollahypoteesi pätee)
sum((c0$observed[,1]-c0$expected[,1])**2/c0$expected[,1]+(c0$observed[,2]-c0$expected[,2]**2/c0$expected[,2]))
#väärin vieläki. Juha sai arvon joka oli yli 1.

m1<-matrix(c(6,30,30,360),ncol=2)
m0<-matrix(c(40,4,440,80),ncol=2)

M<-array(rep(0,8),dim=c(2,2,2))
M[,,1]<-m1
M[,,2]<-m0
mantelhaen.test(M)
mantelhaen.test(M,correct=FALSE)

#juku tää manteal hanzel osaa analysoida kolmiulotteisen chisquare testin.