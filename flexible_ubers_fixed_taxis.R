############################################################
# R Code for 'Flexible Ubers and Fixed Taxis: The Effect of Fuel Prices on Car Services'
# Compiled by Thomas J. Weinandy
# December 7, 2019

############################################################
#################### Table of Contents ##################### 
############################################################
# Part 1: Load Packages & Data.......................line 18
# Part 2: Descriptive Statatistics & Plots...........line 39
# Part 3: OLS Regression............................line 114
# Part 4: SUR Estimation............................line 154
# Part 5: Nonlinear.................................line 183
# Citation..........................................line 235
############################################################
############################################################

############## Part 1: Load Packages and Data ##############

#install.packages("systemfit")
library(systemfit)
#install.packages('devtools')
library(devtools)
#install.packages('broom')
library(broom)
#install.packages('plm')
library(lmtest)

### Set working directory
setwd("~/Desktop")

DATA <- read.csv(file="nyc_taxi_tnc_data.csv", header=TRUE, sep=",")
DATA1 <- DATA[c(2,7,12,14:75)] #baseline data
DATA2 <- read.csv(file="car_service_by_type_data.csv", header=TRUE, sep=",")
# Note that DATA2 an expanded dataset that includes all days while DATA is only business days



######### Part 2: Descriptive Statatistics & Plots #########

summary(DATA[3:19])
summary(DATA2[3:11])
# Note many terms in DATA are in natural log form.
# To convert, use summary(exp(DATA$ln_gas))

### Store variables for plots
ts_taxi <- ts(exp(DATA$ln_taxi)/1000, start=2015, frequency=250)
ts_yellow <- ts(DATA2$yellow/1000, start=2015, frequency=365)
ts_green <- ts(DATA2$green/1000, start=2015, frequency=365)
ts_tnc <- ts(exp(DATA$ln_tnc)/1000, start=2015, frequency=250)
ts_gett <- ts(DATA2$gett/1000, start=2015, frequency=365)
ts_juno <- ts(DATA2$juno/1000, start=2015, frequency=365)
ts_lyft <- ts(DATA2$lyft/1000, start=2015, frequency=365)
ts_uber <- ts(DATA2$uber/1000, start=2015, frequency=365)
ts_via <- ts(DATA2$via/1000, start=2015, frequency=365)
ts_gas <- ts(exp(DATA$ln_gas), start=2015, frequency=250)
ts_rain <- ts(DATA$rain, start=2015, frequency=250)
ts_snow <- ts(DATA$snow, start=2015, frequency=250)
ts_temp <- ts(DATA$temp, start=2015, frequency=250)
ts_collisions <- ts(exp(DATA$ln_collisions), start=2015, frequency=250)
ts_stocks <- ts(exp(DATA$ln_stocks), start=2015, frequency=250)

options(scipen=5)
par(family="serif", font=2) 

# taxis v tnc
par(family="serif", font=2,cex.axis=.8,cex.lab=1)
ts.plot(ts_taxi, ts_tnc, gpars = list(cex.axis=2,col = c("grey", "black"), xaxt="n"), main="Daily Car Service Trips",ylab="Number of trips ('000)",xlab="")
axis(side=2, at=c(0,200,400,600,800)) 
axis(side=1,at=c(2015,2016,2017,2018,2019))
legend(2015, 800, legend=c("Taxis", "TNCs"),col=cbind("grey", "black"), lty=1, cex=0.8, box.lty=0)

# TNC by type (all days)
apps <- cbind(ts_uber,ts_lyft,ts_via,ts_juno,ts_gett)
apps_colors <- cbind("black", "dark grey", "black", "grey", "dark grey")
ts.plot(apps, gpars = list(col = apps_colors, xaxt="n"), ylab="TNC trips by type ('000)")
axis(side=2, at=c(0,200,400,600,800)) 
axis(side=1,at=c(2015,2016,2017,2018,2019))
legend(2015, 650, legend=c("Uber", "Lyft", "Via", "Juno", "Gett"),
       col=apps_colors, lty=1, cex=0.8, box.lty=0, title="TNCs (ranked by type)")

# Taxis by type (all days)
tax <- cbind(ts_yellow, ts_green)
tax_colors <- cbind("black", "dark grey")
ts.plot(tax, gpars = list(col = tax_colors, xaxt="n"), ylab="Number of trips ('000)")
axis(side=2, at=c(0,100,200,300,400,500)) 
axis(side=1,at=c(2015,2016,2017,2018,2019))
legend(2018, 540, legend=c("Yellow taxis", "Green taxis"),
       col=tax_colors, lty=1, cex=0.8, box.lty=0, title="Taxis (ranked by type)")

ts.plot(ts_temp,gpars=list(xaxt="n"),ylab="Temperature relative to previous week (F)")
axis(side=1, at=c(2015,2016,2017,2018,2019))

ts.plot(ts_collisions,gpars=list(xaxt="n"),ylab="Daily Vehicle Collisions in New York City")
axis(side=1, at=c(2015,2016,2017,2018,2019))

ts.plot(ts_gas,gpars=list(xaxt="n"),ylab="Daily NY Harbor Gas Prices (USD/gallon)")
axis(side=1, at=c(2015,2016,2017,2018,2019))

ts.plot(ts_stocks,gpars=list(xaxt="n"),ylab="Daily shares traded on NYSE ('000,000)")
axis(side=1, at=c(2015,2016,2017,2018,2019))

ts.plot(ts_rain,gpars=list(xaxt="n"),ylab="Daily rain (in)")
axis(side=1, at=c(2015,2016,2017,2018,2019))

ts.plot(ts_snow,gpars=list(xaxt="n"),ylab="Daily snow (in)")
axis(side=1, at=c(2015,2016,2017,2018,2019))

par(family="serif", font=2,cex.axis=.8,cex.lab=1)
hist(DATA$diff_gas, xlab='Δ fuel price ($0.01/gal intervals)', breaks=40, freq=F, ylab='Density of observations (%)', main='')



######### Part 3: OLS Regression #########

## OLS baseline
r1 <- lm(DATA$ln_taxi ~ ., data = DATA1)
bptest(r1)
print(summary(r1, robust=TRUE), digits=6)
r2 <- lm(DATA$ln_tnc ~ ., data = DATA1)
bptest(r2)
print(summary(r2, robust=TRUE), digits=6)
cor(resid(r1),resid(r2))

## OLS w stocks
r3 <- lm(DATA$ln_taxi ~ DATA$ln_stocks + ., data = DATA1)
bptest(r3)
print(summary(r3, robust=TRUE), digits=6)
r4 <- lm(DATA$ln_tnc ~ DATA$ln_stocks + ., data = DATA1)
bptest(r4)
print(summary(r4, robust=TRUE), digits=6)
cor(resid(r3),resid(r4))

## OLS w lag
r5 <- lm(DATA$ln_taxi ~ + DATA$lag_ln_taxi + ., data = DATA1)
bptest(r5)
print(summary(r5, robust=TRUE), digits=6)
r6 <- lm(DATA$ln_tnc ~ + DATA$lag_ln_tnc +., data = DATA1)
bptest(r6)
print(summary(r6, robust=TRUE), digits=6)
cor(resid(r5),resid(r6))

## OLS w lag & stocks
r7 <- lm(DATA$ln_taxi ~ DATA$ln_stocks + DATA$lag_ln_taxi + ., data = DATA1)
bptest(r7)
print(summary(r7, robust=TRUE), digits=6)
r8 <- lm(DATA$ln_tnc ~ DATA$ln_stocks + DATA$lag_ln_tnc +., data = DATA1)
bptest(r8)
print(summary(r8, robust=TRUE), digits=6)
cor(resid(r7),resid(r8))



################## Part 4: SUR Estimation ##################
attach(DATA1)
pred_ <- cbind(trend, ln_gas, ln_collisions, int_rain_collisions, int_snow_collisions,
               rain, snow, holiday, temp, day.f2, day.f3, day.f4, day.f5, week.f2, 
               week.f3, week.f4, week.f5, week.f6, week.f7, week.f8, week.f9, week.f10,
               week.f11, week.f12, week.f13, week.f14, week.f15, week.f16, week.f17, 
               week.f18, week.f19, week.f20, week.f21, week.f22, week.f23, week.f24, 
               week.f25, week.f26, week.f27, week.f28, week.f29, week.f30, week.f31, 
               week.f32, week.f33, week.f34, week.f35, week.f36, week.f37, week.f38, 
               week.f39, week.f40, week.f41, week.f42, week.f43, week.f44, week.f45, 
               week.f46, week.f47, week.f48, week.f49, week.f50, week.f51, week.f52) 
# baseline predictors, week.f53 becuase it is made null from lag

# SUR w lag
taxiA <- (DATA$ln_taxi ~ DATA$lag_ln_taxi + pred_)
tncA <- (DATA$ln_tnc ~ DATA$lag_ln_tnc + pred_)
surA <- list(taxiA=taxiA, tncA=tncA)   
SURA <- systemfit(surA, method ='SUR')
print(summary(SURA, robust=TRUE), digits=6)

# SUR w lag & stocks
taxiB <- (DATA$ln_taxi ~ DATA$lag_ln_taxi + DATA$ln_stocks + pred_)
tncB <- (DATA$ln_tnc ~ DATA$lag_ln_tnc + DATA$ln_stocks + pred_)
surB <- list(taxiB=taxiB, tncB=tncB)   
SURB <- systemfit(surB, method ='SUR')
print(summary(SURB, robust=TRUE), digits=6)



##################### Part 5: Nonlinear ####################

gas_dummy <- cbind(DATA$big_gas, DATA$big_gas*DATA$ln_gas)

## OLS baseline (nonlinear)
r9 <- lm(DATA$ln_taxi ~ gas_dummy + ., data = DATA1)
bptest(r9)
print(summary(r9, robust=TRUE), digits=6)
r10 <- lm(DATA$ln_tnc ~ gas_dummy +., data = DATA1)
bptest(r10)
print(summary(r10, robust=TRUE), digits=6)

## OLS w stocks (nonlinear)
r11 <- lm(DATA$ln_taxi ~ DATA$ln_stocks + gas_dummy +., data = DATA1)
bptest(r11)
print(summary(r11, robust=TRUE), digits=6)
r12 <- lm(DATA$ln_tnc ~ DATA$ln_stocks + gas_dummy +., data = DATA1)
bptest(r12)
print(summary(r12, robust=TRUE), digits=6)

## OLS w lag (nonlinear)
r13 <- lm(DATA$ln_taxi ~ + DATA$lag_ln_taxi + gas_dummy +., data = DATA1)
bptest(r13)
print(summary(r13, robust=TRUE), digits=6)
r14 <- lm(DATA$ln_tnc ~ + DATA$lag_ln_tnc + gas_dummy +., data = DATA1)
bptest(r14)
print(summary(r14, robust=TRUE), digits=6)

## OLS w lag & stocks (nonlinear)
r15 <- lm(DATA$ln_taxi ~ DATA$ln_stocks + DATA$lag_ln_taxi + gas_dummy +., data = DATA1)
bptest(r15)
print(summary(r15, robust=TRUE), digits=6)
r16 <- lm(DATA$ln_tnc ~ DATA$ln_stocks + DATA$lag_ln_tnc + gas_dummy +., data = DATA1)
bptest(r16)
print(summary(r16, robust=TRUE), digits=6)

# SUR w lag
taxiC <- (DATA$ln_taxi ~ DATA$lag_ln_taxi + gas_dummy + pred_)
tncC <- (DATA$ln_tnc ~ DATA$lag_ln_tnc + gas_dummy + pred_)
surC <- list(taxiC=taxiC, tncC=tncC)   
SURC <- systemfit(surC, method ='SUR')
print(summary(SURC, robust=TRUE), digits=6)

# SUR w lag & stocks
taxiD <- (DATA$ln_taxi ~ DATA$lag_ln_taxi + DATA$ln_stocks + gas_dummy + pred_)
tncD <- (DATA$ln_tnc ~ DATA$lag_ln_tnc + DATA$ln_stocks + gas_dummy + pred_)
surD <- list(taxiD=taxiD, tncD=tncD)   
SURD <- systemfit(surD, method ='SUR')
print(summary(SURD, robust=TRUE), digits=6)

############################################################

cat('Please cite code as: Weinandy, Thomas J. (2019) "Flexible Ubers and Fixed Taxis: The Effect of Fuel Prices on Car Services" (Version 1.0) [R Code] https://github.com/tomweinandy/flexible_ubers_fixed_taxis')

############################################################
######################### The End ########################## 
############################################################