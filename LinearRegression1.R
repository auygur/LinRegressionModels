rm(list=ls())
cpiu<-read.csv(file="cpiu-long.csv", header=TRUE, sep=",")
cpiu <-cpiu[-1:-2,] #removing first two row
cpiu <-cpiu[,-1] #removing first column

### a) Type of this data is list 
typeof(cpiu) #prints a "list"

cpiu<-na.omit(cpiu) #removing na items
names(cpiu)<-c("Year","Month","CPI") #naming columns

### b) Constructing 2 vectors and printing sizes - 1244 each
year.month.vec<-unlist(cpiu[1])
cpi.vec.ym<-unlist(cpiu[3])
cat("year vector size:",length(year.month.vec)); cat("\ncpi vector size: ",length(cpi.vec.ym))  

### c) Plotting 2 vectors constructed above. I see multiple points because data contains 
###    12 months per year. So I should expect to see 12 points per year.
plot(year.month.vec,cpi.vec.ym)
### Taking averages for each year and creating 2 new vectors. Then plotting these 2 new vectors.
cpi.vec <-aggregate(CPI~Year, FUN=mean, data=cpiu)[,2]
year.vec <-aggregate(CPI~Year, FUN=mean, data=cpiu)[,1]
plot(year.vec,cpi.vec)

### d) Since the plot looks like exponential, I decided to run exponential regression. The type of
### variable is count (as opposed to continious, binary etc.) so log linear models would work. 
### coefficient of this model is log(y)=ax+b where a=0.03310565 and b= -61.22897056
### x = year and y= cpi respectively. 
### This means every year we need to increase the salary by (1-exp(0.03310565)) 
### per year (roughly %3.366)

exponential.model<- lm(log(cpi.vec)~(year.vec))
coef<-coef(exponential.model)[2]
intercept<-coef(exponential.model)[1]

plot(year.vec,log(cpi.vec))
abline(exponential.model)

### Summary shows that regression model explains the data with an adjusted 
### R-squared: 0.9407 this is pretty accurate.
### Years from 1910s to 1930s seem to deviate from the model the most, 
### some of it may be caused by the Great Depression.
summary(exponential.model)

### e) We found earlier that coefficient is 0.03310565 which means roughly %3.366 [exp(coef)] 
### inflation rate. We need to increase a person's salary with this amount ever year. 
### A person making 100k should receive 103,366 after 1st year and 139,244 after 10th year.
salary=100000
salary1=salary *exp(coef)
salary10=salary*exp(10*coef)

### f) I see the sign of great depression since cpi dropped in those years. 
### However I dont see any sign for the recent recession.

### g) I created a new subset of cpiu starting from 1940s. I applied the same model and got new coefficients:
### a= 0.03995968 b=-74.83999 where log(y)=ax+b  [y = cpi and x= year]. Based on new model, salary is now 
### $104,077 for the first year and $149,122 for the 10th year.

cpiu_new<-subset(cpiu,cpiu[,1]>1940)
cpinew.vec <-aggregate(CPI~Year, FUN=mean, data=cpiu_new)[,2]
yearnew.vec <-aggregate(CPI~Year, FUN=mean, data=cpiu_new)[,1]
plot(yearnew.vec,log(cpinew.vec))
exponential.model2<- lm(log(cpinew.vec)~(yearnew.vec))
coef2<-coef(exponential.model2)[2]
intercept2<-coef(exponential.model2)[1]
summary(exponential.model2)
abline(exponential.model2)
newsalary1=salary *exp(coef2)
newsalary10=salary*exp(10*coef2)
