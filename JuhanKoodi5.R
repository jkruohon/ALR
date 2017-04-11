# SEX = 1 female, SEX = 0 male
# BY = year of birth - 1900
# SW = 1, if a person took the sword, SW = 0 otherwise
# AGE = age of person at the time of the promotion
# FAC 
# = 0 Humanities
# = 1 Education
# = 2 Natural Sciences
# = 3 Forestry
# = 4 Social Sciences 

#Tässä alussa Juha käy läpi tuttuja perustapoja tutkia datan rakennetta.

hist(d$AGE)
summary(d$AGE)

logreg1<-glm(SW~SEX+factor(FAC)+AGE,family=binomial(link="logit"),data=d)
summary(logreg1)

logreg2<-glm(SW~SEX+AGE,family=binomial(link="logit"),data=d) #ilman tiedekuntaa. Ei näytä tuovan suurta eroa.
#Residual deviance has to do with lack of fit.  smaller is better.
#Null deviance is deviance when only the constant term is in the model.
an1<-anova(logreg2,logreg1) #tämä vertaa kahta mallia.
1-pchisq(an1$Deviance,4)  #chisq kertymäfunktion kvantiili, upper tail only. LOWER.TAIL TOIMII YHTÄHYVIN!!

#Tehdään nyt humanismista dummy:
HUM<-as.numeric(d$FAC==0)
logreg3<-glm(SW~SEX+AGE+HUM,family=binomial(link="logit"),data=d) 
summary(logreg3) #ei edes pelkkä humanismi ole tilastollisesti merkitsevä tekijä
an2<-anova(logreg2,logreg3) #tämän raportoima deviance arvo on siis täsmälleen sama testisuure kuin chi-square df 1:llä?
1-pchisq(an2$Deviance[2],1)

#tehdään iästä luokkamuuttuja. Näin päästään tutkimaan onko iän vaikutus lineaarinen.
AG<-round(d$AGE/10) #juha ei tykkää cut()ista
logreg4<-glm(SW~SEX+factor(AG),family=binomial(link="logit"),data=d) 
summary(logreg4)
#vanhinten vanhusten p-arvo ei ole merkitsevä MUTTA SYY ON SE, että heiltä on niin vähän dataa.
an3<-anova(logreg2,logreg4) #iällä on ehkä kriittinen arvo. ehkä nuo 50-60-vuotiaat ovat taistolaissukupolvea.
b1<-coef(logreg2)[2] #logodds of gender in model 2
OR<-exp(b1) #OR of gender in model 2
SE<-sqrt(vcov(logreg2)[2,2])
L<-exp(b1-1.96*SE) #lower bound. !!!HEI AIKA NOKKELA TAPA LASKEA SE SUORAAN OR:lle!!!
U<-exp(b1+1.96*SE) #upper bound
b2<-coef(logreg2)[3]
or20<-exp(20*b2)
b0<-coef(logreg2)[1] #vakio
#manuaalisesti laskettu miekanottotodennäköisyys 1950 syntyneille miehille. Tutulla tavalla kuten jo IODSin Logregrluvussa.
p1950<-exp(b0+b2*(1999-1950))
(p1950<-p1950/(1+p1950))
#entäs vuonna 1970 syntyneet miehet
p1970<-exp(b0+b2*(1999-1970))
(p1970<-p1970/(1+p1970))

#Nyt tehdään uutta dataa:
N<-50
x<-30+4*rnorm(N)
G<-c(rep(0,round(N/3)),rep(1,N-round(N/3)))
X<-cbind(rep(1,N),G,x) #Design matrix
B<-c(log(0.25/0.85)-30*0.1,0.5,0.1)
lp<-X%*%B
p<-exp(lp)/(1+exp(lp))
N0<-100
B0<-matrix(rep(0,N0*3),ncol=3)
for(i in 1:N0){
  U<-runif(N)
  Y<-as.numeric(U<p)
  lreg<-glm(Y~G+x,family=binomial)
  B0[i,]<-coef(lreg)
}
hist(B0[,3])
plot(B0[,1],B0[,2])
plot(B0[,1],B0[,3])
plot(B0[,2],B0[,3])
#Noiden kuvioiden erilaisuus on KE 12.4 oppitunnin aihe.