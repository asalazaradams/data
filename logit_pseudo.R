nels_small=read.csv("http://www.principlesofeconometrics.com/poe5/data/csv/nels_small.csv")
nels=read.csv("http://www.principlesofeconometrics.com/poe5/data/csv/nels.csv")


nels$college=ifelse(nels$psechoice >= 3, c(1), c(0)) 


model.logit1=glm(college~grades+faminc+female+black+hscath+famsiz+parcoll, family=binomial(link=logit), data=nels)
summary(model.logit1)

model.logit1$fitted.values
nels$college.fit=ifelse(model.logit1$fitted.values>.5,1,0)
match=ifelse(nels$college == nels$college.fit,1, 0) 
pseudo.R2=sum(match)/length(match)
pseudo.R2

model.logit2=glm(college~hscath, family=binomial(link ="logit"), data=nels)
summary(model.logit2)

model.logit2$fitted.values
nels$college.fit=ifelse(model.logit2$fitted.values>.5,1,0)
match=ifelse(nels$college == nels$college.fit,1, 0) 
pseudo.R2=sum(match)/length(match)
pseudo.R2
