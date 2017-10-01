# Useful-Codes-for-Data-Analysis-in-R
* Converting Zeros to Missing Values

This set of codes makes Data Analysis easier and more efficent

############# Converting Zeros to Missing Values #########################
for (i in names(pima))
  pima[,i][pima[,i]==0]=NA
lapply(pima,function(x) sum(is.na(x)))
is.na(pima)
# or 
pima2$diastolic[pima2$diastolic==0]<-NA
############# Converting Zeros to Missing Values #########################




############# Using With Command ####################################
word.list <- list(letters[1:4], letters[1:5], letters[1:2], letters[1:6])
n.obs <- sapply(word.list, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(word.list, "[", i = seq.max))
c(1:2)[1:4]
############# Using With Command ####################################



################### Variance-Covariance Matrix  ########################################
mu = c(1,2,-1)
a = c(1,-2,1)
V = matrix(0,3,3)
V[row(V) >= col(V)] = c(1,-.4,.5,3,2,5)
V[row(V) < col(V)] = V[row(V) > col(V)]
V
(expvalofU = t(a) %*% mu)
(varofU = t(a) %*% V %*% a)
################### Variance-Covariance Matrix  ########################################




################### Density Plot of Each Variables  ########################################
par(mfrow = c(3,3))
lapply(1:8,function(k) plot(density(pima[,k],na.rm = T),main = names(pima)[k]))
################### Density Plot of Each Variables  ########################################




################### Beer data Plot  ########################################
Beer=read.csv(file.choose())
beer.lm = lm(Calories ~ Alcohol, data = Beer)
plot(Calories ~ Alcohol, data = Beer,xlim = c(2,8),ylim = c(0,300))
abline(beer.lm, lwd = 2.5,col = "blue")
Alc.new = seq(2,8,length = 1000)
beer.pred = predict(beer.lm,data.frame(Alcohol = Alc.new),
                    interval = "confidence",level = .95)
lines(Alc.new,beer.pred[,2],lwd = 2,
      col = "red",lty = 3)
lines(Alc.new,beer.pred[,3],lwd = 2,
      col = "red",lty = 3)
beer.pred = predict(beer.lm,
                    data.frame(Alcohol = Alc.new),
                    interval = "prediction",level = .95)
lines(Alc.new,beer.pred[,2],lwd = 2,
      col = "green",lty = 2)
lines(Alc.new,beer.pred[,3],lwd = 2,
      col = "green",lty = 2)
legend("topleft",c("95% CI","95% PI"),col = c("red","green"),
       lty = c(3,2),lwd = c(2,2),bty = "n")
title("Beer Data Regression With 95% CIs and PIs",cex = 1.5)
################### Beer data Plot  ########################################


################### Outliers and Residuals  ########################################
residuals(stack.lm)
resid(stack.lm)
rstandard(stack.lm)
rstudent(stack.lm)
cooks.distance(stack.lm)
cbind(e.i=resid(stack.lm),r.i=rstandard(stack.lm),t.i=rstudent(stack.lm),
  h.i=lm.influence(stack.lm)$hat,D.i=cooks.distance(stack.lm))
par(mfrow=c(2,2))
plot(stack.lm)

lm(log(stack.loss) ~ . + I(Water.Temp^2),data=stackloss)
influence.measures(stack.lm)
termplot(stack.lm, partial.resid=T,col.res='blue',se = F)

stack.lm2 = lm(stack.loss ~ . + I(Water.Temp^2) ,data=stackloss)
termplot(stack.lm2, partial.resid = T,col.res = 'blue',se = T)
stack.lm2 = lm(stack.loss~Air.Flow+poly(Water.Temp,2)+Acid.Conc.,data=stackloss)
termplot(stack.lm2, partial.resid=TRUE,col.res='blue',se=T)

################### Outliers and Residuals  ########################################



################### apply function ########################################
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 1, mean)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))
################### apply function########################################



############  Correlation Matrix from covariance Matrix ###############
load("D:/Google Drive/nps/my Data Analysis/labs/lab 3/HW03Ex3.RData")
V
mu
sdvec=sqrt(diag(V))
RHO=sweep(V,1,sdvec,"/")
RHO = sweep(RHO,2,sdvec,"/")
round(RHO,3)
# or
RHO=V/outer(sdvec,sdvec)
# or
RHO=cov2cor(V)
round(RHO,3)
############  Correlation Matrix from covariance Matrix ###############




############# Example of ANOVA #########################
wear = c(9.16,13.29,12.07,11.97,13.31,12.32,11.78,
         11.95,15.15,14.75,14.79,15.48,13.47,13.06,
         11.47,9.54,11.26,13.66,11.18,15.03,14.86,
         11.35,8.73,10.00,9.75,11.71,12.45,12.38)
Treat = factor(rep(LETTERS[1:4],each = 7))
aovlist = aov(wear ~ Treat)
summary(aovlist)
par(mfrow = c(2,2))
boxplot(wear ~ Treat,col = 2:5, main = "Boxplots of Wear")
stripchart(wear ~ Treat,vertical = T,pch = 19,col = 2:5,
           main = "Stripchart of Wear")
meanvec = tapply(wear,Treat,mean)
sdvec = tapply(wear,Treat,sd)
plot(meanvec,sdvec,xlab = "Mean",ylab = "SD",
     pch = LETTERS[1:4],cex = .8,font = 2)
title("Plot of Treatment Means vs SDs")
qqnorm(aovlist$residuals, main = "Normal QQ-Plot of Residuals") 
qqline(aovlist$residuals)

# Boxplots show differences in mean levels that the ANOVA test confirms are “real” in a statistical sense
# Are variances unequal?  Treatment C seems to have an usually large variance, but remember that the sample sizes are small
# Stripcharts show the samples in more detail
# No obvious relationship between SD and mean of treatment
# QQ-plot seems what you might expect to see if the data came from a normal distribution


# The Levene Test (Faraway, p. 228) is a nonparametric test for equality of variances and
# has good performance even with non-normal data

levene.test(wear,Treat,location = "trim.mean", trim = .25)
levene.test(wear,Treat,location = "trim.mean", trim = .25,bootstrap = T)

# Large p-values in both cases suggest weak evidence against the null hypothesis that the variances are equal
############# Example ANOVA #########################



################### Faraway example coagulation ########################################
coagulation
coag = aov(coag ~ diet, data = coagulation)
summary(coag)
par(mfrow = c(2,2))
boxplot(coag ~ diet,data = coagulation,col=c(1,2,3,4),main='Boxplot of Coagulation')
stripchart(coag ~ diet,vertical = T,pch = 19,col = 2:5,data = coagulation,
           main = "Stripchart of Wear")
meanvec = tapply(coagulation$coag,coagulation$diet,mean)
sdvec = tapply(coagulation$coag,coagulation$diet,sd)
plot(meanvec,sdvec,xlab = "Mean",ylab = "SD",
     pch = LETTERS[1:4],cex = .8,font = 2)
title("Plot of Treatment Means vs SDs")
qqnorm(coag$residuals, main = "Normal QQ-Plot of Residuals") 
qqline(coag$residuals)

tukeycoag=TukeyHSD(coag)
plot(tukeycoag)
################### Faraway example coagulation ########################################



