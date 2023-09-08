#ho= Avg annual  rainfall among station is equal
#h1: Avg annual  rainfall among station is not equal

getwd()
setwd("C:/Users/renugopal/Desktop/Sai")
Data<-read.csv(file="Post_hoc.csv",T)
data <- data.frame(group = rep(c("Nungambakkam", "Saidapet", "Meenambakkam"), each = 365),values = c(Data$Nungambakkam,Data$Saidapet,Data$Meenambakkam))
model <- aov(values~group, data=data)
model
summary(model)

#conculsion = We reject the hypothesis and we say that in between station are not equal 
#post hoc test (" to compare pairwise test that their is any significant difference in avg annual rainfall  )


TukeyHSD(model, conf.level=.95)  

# TukeyHSD = we can compare multiple station ( like 2~1,3~1,3~2 ) a
# we conclude that there is  significant different in each of the given 3 station as alpha percentage=0.05% 

