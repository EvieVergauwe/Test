###ruining things
###maak dataset
set.seed(123)
I=1000
sig=10
trueDisease=as.vector(1:3%*%rmultinom(I,1,c(.6,.2,.2)))
trueMean=c(50,90,100)
blood=rnorm(I,trueMean[trueDisease],sig)
out=data.frame(disease=trueDisease,blood=round(blood,0))
colnames(out)=c("disease","blood")
write.table(out,file="hw1.dat",quote=F,row.names=F)
out

###search for number of obs per disease state (per group)
out1<-out[which(out$disease==1),]#dataset met enkel diseasestate = 1
out2<-out[which(out$disease==2),]#dataset met enkel diseasestate = 2
out3<-out[which(out$disease==3),]#dataset met enkel diseasestate = 1

N1=sum(out$disease==1)
N2=sum(out$disease==2)
N3=sum(out$disease==3)
N=N1+N2+N3
munot1=90
sigmanot1=15
munot2=90
sigmanot2=15
munot3=90
sigmanot3=15
a=2
b=2
library('MCMCpack')
M=2000 ##aantal keer
##plaats maken
mu1=1:M 
mu2=1:M 
mu3=1:M
sig2=1:M
mu1[1]=mu2[1]=mu3[1]=100
sig2[1]=225
mean1=mean(out1$blood)#mean group1
mean2=mean(out2$blood)
mean3=mean(out3$blood)
sig2=var(out$blood)#variance across all groups

for (m in 2:M)
{
  v1=1/(N1/sig2[m-1]+1/sigmanot1) 
  c1=(N1*mean1/sig2[m-1]+munot1/sigmanot1) 
  mu1[m]=rnorm(1,v1*c1,sqrt(v1)) 
  v2=1/(N2/sig2[m-1]+1/sigmanot2) 
  c2=(N2*mean2/sig2[m-1]+munot2/sigmanot2) 
  mu2[m]=rnorm(1,v2*c2,sqrt(v2)) 
  v3=1/(N3/sig2[m-1]+1/sigmanot3) 
  c3=(N3*mean3/sig2[m-1]+munot3/sigmanot3) 
  mu3[m]=rnorm(1,v3*c3,sqrt(v3))  
  SSE=sum((out1$blood-mu1[m])^2)+sum((out2$blood-mu2[m])^2)+sum((out3$blood-mu3[m])^2)
  sig2[m]=rinvgamma(1,sum(N)/2+a,SSE/2+b)
}
matplot(typ='l',mu1,lty=1, ylim=c(0,150))
matplot(typ='l',mu2,lty=1, ylim=c(0,150))
matplot(typ='l',mu3,lty=1, ylim=c(0,150))
plot(sqrt(sig2),typ='l')
