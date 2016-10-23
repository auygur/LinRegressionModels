### Linear Regressions HW4####
setwd("XXX") ### Set the directory
rm(list=ls())
housing<-read.csv(file="Housing.csv", header=TRUE,sep = ",")
housing<-housing[,-1]  ##getting rid of first column


### Transforming Yes and No's to 1s and 0s
c <-c(6:10,12)
for (i in c){
  housing[,i]<-ifelse(housing[,i]=="yes",1,0)
}


### a) Creating Average prices for apts with different # of bedrooms
average.vec <- aggregate(price~bedrooms, FUN=mean, data=housing)
avg.train<- average.vec[0:5,]
avg.train.bd<-avg.train[,1]
avg.train.price<-avg.train[,2]

plot(avg.train.bd,avg.train.price)

### Applying Exponential model only gives 0.877 R-square 
# exp.model<-lm(log(avg.train.price)~(avg.train.bd))
# exp.coef<-coef(exp.model)[2]
# exp.intercept<-coef(exp.model)[1]
# plot(avg.train.bd,log(avg.train.price))
# abline(exp.model)
# summary(exp.model)

### Applying Log model gives 0.953 R-square 
### Basically Log model is y= b + a ln(x) where b is intercept, a is coefficient, x is # of bedroom and y is price
### in this model, coef is 30006.33 and intercept is 36558.38
log.model<-lm(avg.train.price~log(avg.train.bd))
log.coef<-coef(log.model)[2]
log.intercept<-coef(log.model)[1]
plot(log(avg.train.bd),avg.train.price)
abline(log.model)
summary(log.model)

### Based on Log model, the increase is $7,182.507
### I dont think this is a good model because housing price depends on more than 1 variable.
extrabedroom<-log.intercept+log.coef*log(6)
increase<-extrabedroom - avg.train.price[5]
increase
### b) Not really. This plot shows that there are higher priced apartments with less lot size. Linear fit
### only explains the data with 0.33 R-squared.There are more to lotsize for calculating prices.

average.vec2 <- aggregate(price~lotsize, FUN=mean, data=housing)
plot(average.vec2)
lin.fit <-lm(average.vec2$price~average.vec2$lotsize)
abline(lin.fit)
summary(lin.fit)


### c) We can check that by comparing mean of houses with full basements and mean of houses 
### without fullbasements. It shows that houses with full basements are generally higher priced.
A <-mean(housing$price[housing$fullbase==0])<mean(housing$price[housing$fullbase==1])
cat("Mean housing prices for apartments with fully furnished basaments is higher than the ones without fully furnished basement:",A)

### d)  Created a new data called housing2bd1bath. Lin model explains it with 0.32 R-squared.

housing2bd1bath <-housing[housing$bedrooms==2&housing$bathrms==1,]
plot(housing2bd1bath$lotsize,housing2bd1bath$price)
lin.model<-lm(housing2bd1bath$price~housing2bd1bath$lotsize)
abline(lin.model)
summary(lin.model)


### e) B0 is intercept and B1 is coef
lin.intercept<-coef(lin.model)[1]
lin.coef<-coef(lin.model)[2]
price_hat=lin.intercept+lin.coef*housing2bd1bath$lotsize
price_dif=housing2bd1bath$price-price_hat

### price difference is max for the apt index #97 in my vector housing2bd1bath
### which is apt with price $101,000 and lot size 8,880
### my equation gives me price_hat = $68,395 making a price_diff= $32,605
which(price_dif==max(price_dif)) 
housing2bd1bath$price[97]; housing2bd1bath$lotsize[97]; price_hat[97];price_dif[97]

## price difference is min for the apt index #45 in my vector housing2bd1bath
### which is apt with price $35,000 and lot size 7,700
### my equation gives me price_hat = $63,627 making a price_diff= -$28,627
which(price_dif==min(price_dif))
housing2bd1bath$price[45]; housing2bd1bath$lotsize[45]; price_hat[45];price_dif[45]

### f) Housing model using all parameters now explains the data with higher precision. 
### R-square is now 0.5315. I notice that bedroom and Bathrooms returned coefficient of "NA".
### I am going to manually replace it with 0.

housing.model<-lm(housing2bd1bath$price~., data=housing2bd1bath)
summary(housing.model)
housing.intercept<-coef(housing.model)[1]
housing.coef<-c()

for (i in 1:11){
  housing.coef[i]<-coef(housing.model)[i+1]
}

housing.coef[2]=0; housing.coef[3]=0 ##replacing NA s with 0.

price_hat2<-housing.intercept+housing.coef[1]*housing2bd1bath[,2]+housing.coef[2]*housing2bd1bath[,3]+
  housing.coef[3]*housing2bd1bath[,4]+housing.coef[4]*housing2bd1bath[,5]+housing.coef[5]*housing2bd1bath[,6]+
  housing.coef[6]*housing2bd1bath[,7]+housing.coef[7]*housing2bd1bath[,8]+housing.coef[8]*housing2bd1bath[,9]+
  housing.coef[9]*housing2bd1bath[,10]+housing.coef[10]*housing2bd1bath[,11]+housing.coef[11]*housing2bd1bath[,12]
price_dif2<-housing2bd1bath$price-price_hat2  


### price difference in the new model is max for the same apt, it is index #97 in my vector housing2bd1bath
### which is apt with price $101,000 and lot size 8,880
### my new model gives me price_hat2 = $68,530 making a price_diff2= $32,470
which(price_dif2==max(price_dif2)) 
housing2bd1bath$price[97]; housing2bd1bath$lotsize[97]; price_hat2[97];price_dif2[97]
which(price_dif2==max(price_dif2)) 

### Yes slightly better. Previous max difference was $32,605 new one is $32,470. This is 0.4% improvement.
### I was expecting more improvement for this particular apartment because my R-squared improved 65%. (.53 vs .32)

(price_dif2[97]/price_dif[97]-1)*100 # checking the improvement of price differences for two models in part e and f.
(0.53/0.32-1)*100 ## checking the improvement of R-squareds for two models in part e and f.

### g) Removing variables: bedroom, bathroom, stories, driveway and garageplace yields better R-square.
### New R-squared is 0.5376, whereas previous one was 0.5315
c2 <- c(1,2,7:10,12) 
housingnew2bd1bath <-housing2bd1bath[,c2]
housing.model2<-lm(housingnew2bd1bath$price~., data=housingnew2bd1bath)

summary(housing.model2)
housing2.intercept<-coef(housing.model2)[1]
housing2.coef<-c()

for (i in 1:6){
  housing2.coef[i]<-coef(housing.model2)[i+1]
}

price_hat3=housing2.intercept+housing2.coef[1]*housingnew2bd1bath[,2]+housing2.coef[2]*housingnew2bd1bath[,3]+
  housing2.coef[3]*housingnew2bd1bath[,4]+housing2.coef[4]*housingnew2bd1bath[,5]+housing2.coef[5]*housingnew2bd1bath[,6]+
  housing2.coef[6]*housingnew2bd1bath[,7]
price_dif3<-housingnew2bd1bath$price-price_hat3 

### As you can see my new max price dif3 is : $32,455 -this is sligtly better compared to model in f -which yielded
### $32,470 - this is about 
cat("Max price difference in part e: ",max(price_dif),"and max price difference in part g: ",max(price_dif3),
"my new model improves part e with: %", -1*(max(price_dif3)/max(price_dif)-1)*100)
cat("Max price difference in part f: ",max(price_dif2),"and max price difference in part g: ",max(price_dif3),
    "my new model improves part f with: %", -1*(max(price_dif3)/max(price_dif2)-1)*100)


