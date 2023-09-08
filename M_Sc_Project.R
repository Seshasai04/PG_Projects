#Data
data<-read.csv(file.choose())
Pat<-data$patient.no
Age<-data$Age
TLC<-data$TLC
SGOT<-data$SGOT
PC<-data$Platlets.Count
BP<-data$BP
#Membership function
mf   <- function(x, a, b, c) {
  p1 <- (x - a) / (b - a)
  p2 <- (c - x) / (c - b)
  p3 <- p1
  p <- pmax(0, pmin(p1, p2, p3))
  return(p)
}

Ac<-mf(Age,2,9,16)
Ay<-mf(Age,15,30,45)
Ao<-mf(Age,44,65,90)
Tl<-mf(TLC,3500,3750,4000)
Tm<-mf(TLC,3900,7450,11000)
Th<-mf(TLC,10000,12500,15000)
Sl<-mf(SGOT,10,25,40)
Sm<-mf(SGOT,35,42,50)
Sh<-mf(SGOT,45,50,55)
Pl<-mf(PC,3500,80000,150000)
Pm<-mf(PC,140000,295000,450000)
Ph<-mf(PC,440000,455000,470000)
Bl<-mf(BP,120,127,134)
Bm<-mf(BP,127,144,161)
Bh<-mf(BP,154,163,172)

val<-function(x){
  mx<-max(x)
  cl1<-which.max(x)
  smx<-max(x[x!=mx])
  cl2<-which.max(x[x!=mx])
  if(smx!=0){
    df<-cbind(mx,cl1,smx,cl2)
  } else{
    df<-cbind(mx,cl1,smx,NA)
  }
  df<-cbind(df)
}
vl<-function(x){
  mx<-max(x)
  cl1<-which.max(x)
  smx<-max(x[x!=mx])
  cl2<-which.max(x[x!=mx])
  if(cl2==cl1){cl2=ifelse(cl1==1,2,ifelse(cl1==2,1,3))}
  if(smx!=0){
    df<-cbind(mx,cl1,smx,cl2)
  } else{
    df<-cbind(mx,cl1,smx,NA)
  }
  df<-cbind(df)
}

a<-t(apply(cbind(Ac,Ay,Ao),1,FUN = val))
a[,4]<-ifelse(is.na(a[,4])&a[,2]==1,2,ifelse(is.na(a[,4])&a[,2]==2,1,ifelse(is.na(a[,4])&a[,2]==3,2,NA)))
t<-t(apply(cbind(Tl,Tm,Th),1,FUN = vl))
t[,4]<-ifelse(is.na(t[,4])&t[,2]==1,2,1)
s<-t(apply(cbind(Sl,Sm,Sh),1,FUN = vl))
s[,4]<-ifelse(is.na(s[,4])&s[,2]==3,2,3)
p<-t(apply(cbind(Pl,Pm,Ph),1,FUN = vl))
p[,4]<-ifelse(is.na(p[,4])&p[,2]==1,2,1)
b<-t(apply(cbind(Bl,Bm,Bh),1,FUN = vl))
b[,4]<-ifelse(is.na(b[,4])&b[,2]==1,2,1)
A<-data.frame(A=Pat,B=a[,1],C=a[,2],D=a[,3],E=a[,4])
T<-data.frame(A=Pat,B=t[,1],C=t[,2],D=t[,3],E=t[,4])
S<-data.frame(A=Pat,B=s[,1],C=s[,2],D=s[,3],E=s[,4])
P<-data.frame(A=Pat,B=p[,1],C=p[,2],D=p[,3],E=p[,4])
B<-data.frame(A=Pat,B=b[,1],C=b[,2],D=b[,3],E=b[,4])

#subsetting
library(sets)
ac<-subset(A,C == 1 | E == 1)
ay<-subset(A,C == 2 | E == 2)
ao<-subset(A,C == 3 | E == 3)
tl<-subset(T,C == 1 | E == 1)
tm<-subset(T,C == 2 | E == 2)
th<-subset(T,C == 3 | E == 3)
sl<-subset(S,C == 1 | E == 1)
sm<-subset(S,C == 2 | E == 2)
sh<-subset(S,C == 3 | E == 3)
pl<-subset(P,C == 1 | E == 1)
pm<-subset(P,C == 2 | E == 2)
ph<-subset(P,C == 3 | E == 3)
bl<-subset(B,C == 1 | E == 1)
bm<-subset(B,C == 2 | E == 2)
bh<-subset(B,C == 3 | E == 3)

#Parameters

#AGE
Ac.25<-rbind(subset(ac,B>=0.25 & C ==1),subset(ac,D>=0.25 & E==1))$A
Ac.5<-rbind(subset(ac,B>=0.5 & C ==1),subset(ac,D>=0.5 & E==1))$A
Ac.75<-rbind(subset(ac,B>=0.75 & C ==1),subset(ac,D>=0.75 & E==1))$A

Ay.2<-rbind(subset(ay,B>=0.2 & C ==2),subset(ay,D>=0.2 & E==2))$A
Ay.4<-rbind(subset(ay,B>=0.4 & C ==2),subset(ay,D>=0.4 & E==2))$A
Ay.6<-rbind(subset(ay,B>=0.6 & C ==2),subset(ay,D>=0.6 & E==2))$A
Ay.8<-rbind(subset(ay,B>=0.8 & C ==2),subset(ay,D>=0.8 & E==2))$A

Ao.2<-rbind(subset(ao,B>=0.2 & C ==3),subset(ao,D>=0.2 & E==3))$A
Ao.4<-rbind(subset(ao,B>=0.4 & C ==3),subset(ao,D>=0.4 & E==3))$A
Ao.6<-rbind(subset(ao,B>=0.6 & C ==3),subset(ao,D>=0.6 & E==3))$A
Ao.8<-rbind(subset(ao,B>=0.8 & C ==3),subset(ao,D>=0.8 & E==3))$A

#TLC
Tl.2<-rbind(subset(tl,B>=0.2 & C ==1),subset(tl,D>=0.2 & E==1))$A
Tl.4<-rbind(subset(tl,B>=0.4 & C ==1),subset(tl,D>=0.4 & E==1))$A
Tl.6<-rbind(subset(tl,B>=0.6 & C ==1),subset(tl,D>=0.6 & E==1))$A
Tl.8<-rbind(subset(tl,B>=0.8 & C ==1),subset(tl,D>=0.8 & E==1))$A

Tm.2<-rbind(subset(tm,B>=0.2 & C ==2),subset(tm,D>=0.2 & E==2))$A
Tm.4<-rbind(subset(tm,B>=0.4 & C ==2),subset(tm,D>=0.4 & E==2))$A

Th.2<-rbind(subset(th,B>=0.2 & C ==3),subset(th,D>=0.2 & E==3))$A

#SGOT
Sl.25<-rbind(subset(sl,B>=0.25 & C ==1),subset(sl,D>=0.25 & E==1))$A
Sl.5<-rbind(subset(sl,B>=0.5 & C ==1),subset(sl,D>=0.5 & E==1))$A

Sm.25<-rbind(subset(sm,B>=0.25 & C ==2),subset(sm,D>=0.25 & E==2))$A
Sm.5<-rbind(subset(sm,B>=0.5 & C ==2),subset(sm,D>=0.5 & E==2))$A
Sm.75<-rbind(subset(sm,B>=0.75 & C ==2),subset(sm,D>=0.75 & E==2))$A

Sh.2<-rbind(subset(sh,B>=0.2 & C ==3),subset(sh,D>=0.2 & E==3))$A
Sh.4<-rbind(subset(sh,B>=0.4 & C ==3),subset(sh,D>=0.4 & E==3))$A
Sh.6<-rbind(subset(sh,B>=0.6 & C ==3),subset(sh,D>=0.6 & E==3))$A

#PC
Pl.25<-rbind(subset(pl,B>=0.2 & C ==1),subset(pl,D>=0.2 & E==1))$A
Pl.5<-rbind(subset(pl,B>=0.5 & C ==1),subset(pl,D>=0.5 & E==1))$A
Pl.75<-rbind(subset(pl,B>=0.75 & C ==1),subset(pl,D>=0.75 & E==1))$A

Pm.25<-rbind(subset(pm,B>=0.25 & C ==2),subset(pm,D>=0.25 & E==2))$A

#BP
Bl.25<-rbind(subset(bl,B>=0.25 & C ==1),subset(bl,D>=0.25 & E==1))$A
Bl.5<-rbind(subset(bl,B>=0.5 & C ==1),subset(bl,D>=0.5 & E==1))$A
Bl.75<-rbind(subset(bl,B>=0.75 & C ==1),subset(bl,D>=0.75 & E==1))$A

Bm.25<-rbind(subset(bm,B>=0.25 & C ==2),subset(bm,D>=0.25 & E==2))$A
Bm.5<-rbind(subset(bm,B>=0.5 & C ==2),subset(bm,D>=0.5 & E==2))$A

#Rule
rule<-function(a,b,c,d,e){
  intersect(intersect(intersect(intersect(a,b),c),d),e)
}
rule1<-rule(Ao.4,Tl.4,Sh.6,Pl.75,Bl.25)
rule2<-rule(Ac.25,Tl.2,Sh.2,Pl.25,Bl.25)
rule3<-rule(Ay.6,Tl.2,Sh.2,Pl.25,Bl.25)
rule4<-rule(Ao.6,Tl.2,Sh.2,Pl.25,Bl.25)
rule5<-rule(Ac.25,Tl.2,Sm.25,Pl.25,Bl.25)
rule6<-rule(Ao.6,Tl.2,Sh.2,Pl.25,Bl.25)
rule7<-rule(Ao.2,Tl.2,Sh.6,Pl.25,Bl.25)
rule8<-rule(Ao.6,Tl.2,Sh.6,Pl.5,Bl.25)
rule9<-rule(Ao.2,Tl.2,Sm.25,Pl.25,Bl.25)
rule10<-rule(Ay.2,Tl.2,Sh.2,Pl.25,Bl.25)
rule11<-rule(Ao.2,Tl.2,Sh.2,Pl.25,Bl.5)
rule12<-rule(Ac.25,Tl.2,Sm.5,Pl.25,Bl.5)
rule13<-rule(Ao.4,Tl.2,Sh.2,Pl.25,Bl.75)
rule14<-rule(Ao.2,Tl.8,Sh.2,Pl.25,Bl.25)
rule15<-rule(Ay.6,Tl.2,Sh.2,Pl.25,Bl.25)
rule16<-rule(Ay.2,Tl.2,Sh.6,Pl.25,Bl.25)
rule17<-rule(Ac.5,Tl.2,Sh.2,Pl.25,Bl.25)

rule1
rule2
rule3
rule4
rule5
rule6
rule7
rule8
rule9
rule10
rule11
rule12
rule13
rule14
rule15
rule16
rule17
