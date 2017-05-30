constant <- -1.074; exposure <- -0.72; racewhite <- 0.056

(y<-matrix(ncol=2,c(14,32,11,12,93,81,52,43),dimnames=list("race/exposure"=c("white1","white0","black1","black0"),symptoms=c("yes","no"))))

(phat<-c(exp(constant+exposure+racewhite)/(1+exp(constant+exposure+racewhite)),exp(constant+racewhite)/(1+exp(constant+racewhite)),
        exp(constant+exposure)/(1+exp(constant+exposure)),exp(constant)/(1+exp(constant))))

(yhat<-matrix(ncol=2,c(phat*rowSums(y),(1-phat)*rowSums(y)),
  dimnames=list("race/exposure"=c("white1","white0","black1","black0"),symptoms=c("yes","no"))))

(PearRes<-(y-yhat)/sqrt(yhat*(1-phat)))

(x2<-(y-yhat)^2/yhat)
