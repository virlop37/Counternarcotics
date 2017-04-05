# Georgetown University
# SEST 704 Intelligence Analytics
# Class 7: Causality Demos
# 1 March 2017
# Reference:
# https://www.r-bloggers.com/chicken-or-the-egg-granger-causality-for-the-masses/
# http://www.ucbbiostat.com

#####################
# Granger Causality #
#####################

# Initial setup
rm(list=ls()) # Clean slate, clears variables
graphics.off() # Clears plots
install.packages("lmtest")
library(lmtest)
library (ggplot2)
# Exercise 1: Reproduce published demo
setwd("C:/Users/virlo/Documents/SEST701")
dir()
populism<-read.csv("france.csv")
summary(populism)

#GRANGER CAUSALITY
par(mfrow=c(2,1),mar=c(2,2,2,2))
plot(populism$populist_vote, main="Populist share of vote")
plot(populism$unemployment_youth_male, main = "Youth male unemployment")
derivada_voto<-diff(populism$populist_vote)
derivada_desempleo<-diff(populism$unemployment_youth_male)
plot.ts(derivada_voto, main="Delta Voto")
plot.ts(derivada_desempleo, main="Delta desempleo")
print(grangertest(derivada_voto~derivada_desempleo))
print(grangertest(populism$populist_vote~populism$unemployment_youth_male))
print(grangertest(populism$unemployment_youth_male~populism$populist_vote))
print(grangertest(populism$unemployment_youth_male~populism$populist_vote, order=1))

#GRANGER CAUSALITY SECOND TRY
rm(list = ls())
graphics.off()
install.packages("lmtest")
library("lmtest")
install.packages("ggplot2")
library("ggplot2")
setwd("C:/Users/virlo/Documents/SEST701")
egypt<-read.csv("Egypt.csv")
par(mfrow=c(2,1),mar=c(2,2,2,2))
plot(egypt$Number_terroristattacks,main="Number of terrorist attacks")
plot(egypt$Youthmale_unemployment, main= "Youth male unemployment rate")
dattacks<-diff(egypt$Number_terroristattacks)
dunemployment<-diff(egypt$Youthmale_unemployment)
plot.ts(dattacks, main="Delta attacks")
plot.ts(dunemployment, main="Delta unemployment")
#Does Youth male unemployment Granger cause attacks? Using derivative data
print(grangertest(dattacks~dunemployment, order=1))
#Do attacks cause Youth male unemployment?
print(grangertest(dunemployment~dattacks, order=1))

#Does Youth Male unemployment cause attacks? Using original data
print(grangertest(egypt$Number_terroristattacks~egypt$Youthmale_unemployment, order=1))
#Do attacks cause youth male unemployment?
print(grangertest(egypt$Youthmale_unemployment~egypt$Number_terroristattacks, order=1))
summary(egypt)

##GRANGER TEST DRUG PRICES AND DRUG DEMAND (BECKER)##
rm(list=ls())
graphics.off()
install.packages("lmtest")
library("lmtest")
install.packages("ggplot2")
library("ggplot2")
setwd("C:/Users/virlo/Documents/SEST701")
deathsandprices<-read.csv("deathsandprices.csv")
par(mfrow=c(2,1),mar=c(2,2,2,2))
# Testing Becker's hypothesis that prices don't matter when demand is inelastic by using number of deaths as proxy for demand since there are more observations#
plot(deathsandprices$Heroin_deaths, main="Number of heroine deaths")
plot(deathsandprices$Heroin_price, main="Price of heroine per gram")
plot(deathsandprices$Cocaine_deaths, main="Number of cocaine deaths")
plot(deathsandprices$Cocaine_price, main="Price of cocaine per gram")
plot(deathsandprices$Psychostimulants_deaths, main="Psychostimulants Deaths")
plot(deathsandprices$Methamphetamine_price, main="Methamphetamine price")
#Do heroin prices Granger cause heroin deaths?#
print(grangertest(deathsandprices$Heroin_deaths~deathsandprices$Heroin_price,order=1))
#Do heroin deaths granger cause heroin prices?
print(grangertest(deathsandprices$Heroin_price~deathsandprices$Heroin_deaths,order=1))
#Do cocaine prices Granger cause Cocaine deaths?#
print(grangertest(deathsandprices$Cocaine_deaths~deathsandprices$Cocaine_price,order=1))
#Do cocaine deaths Granger cause cocaine prices?#
print(grangertest(deathsandprices$Cocaine_price~deathsandprices$Cocaine_deaths,order=1))
#Do methamphetamine prices Granger cause psychostimulant deaths?#
print(grangertest(deathsandprices$Psychostimulants_deaths~deathsandprices$Methamphetamine_price,order=1))
#Do psychostimulant deaths Granger cause methamphetamine prices?#
print(grangertest(deathsandprices$Methamphetamine_price~deathsandprices$Psychostimulants_deaths,order=1))

#Testing Becker hypothesis that prices don't matter when demand is inelastic by using actual users#
usersandprices<-read.csv("usersandprices.csv")
plot(usersandprices$Cocaine_users,main="Number of cocaine users")
plot(usersandprices$Cocaine_price,main="Cocaine price per gram")
plot(usersandprices$Methamphethamine_users,main="Number of methampethamine users")
plot(usersandprices$Methamphetamine_price, main="Methamphetamine price per gram")
plot(usersandprices$Heroin_users,main="Number of heroin users")
plot(usersandprices$Heroin_price,main="Heroin price per gram")
plot(usersandprices$Marijuana_users, main="Number of marijuana users")
plot(usersandprices$Marijuana_price, main="Marijuana price")
#Do Cocaine prices Granger cause the number of cocaine users?
print(grangertest(usersandprices$Cocaine_users~usersandprices$Cocaine_price,order=1))
#Do cocaine users Granger cause cocaine prices?
print(grangertest(usersandprices$Cocaine_price~usersandprices$Cocaine_users,order=1))
#Do methamphetamine prices Granger cause methamphetamine users?
print(grangertest(usersandprices$Methamphethamine_users~usersandprices$Methamphetamine_price,order=1))
#Do methamphetamine users Granger cause methamphetamine prices?
print(grangertest(usersandprices$Methamphetamine_price~usersandprices$Methamphethamine_users,order=1))
#Do heroin prices Granger cause heroin users?
print(grangertest(usersandprices$Heroin_users~usersandprices$Heroin_price,order=1))
#Do heroine users Granger cause heroin prices?
print(grangertest(usersandprices$Heroin_price~usersandprices$Heroin_users,order=1))
#Do marijuana prices Granger cause Marijuana users?
print(grangertest(usersandprices$Marijuana_users~usersandprices$Marijuana_price,order=1))
#Do marijuana users Granger cause Marijuana prices?
print(grangertest(usersandprices$Marijuana_price~usersandprices$Marijuana_users,order=1))

#Testing the impact on users of counter-demand measures, such as arrests due to drug abuse#
arrerstsandusers<-read.csv("arrestsandusers.csv")

plot(arrerstsandusers$Users_alldrugs,main="Number of users of all drugs")
plot(arrerstsandusers$Arrests_drugabuse,main = "Arrests of drug abusers")
#Do arrests of abusers Granger cause the number of users of drugs?
print(grangertest(arrerstsandusers$Users_alldrugs~arrerstsandusers$Arrests_drugabuse,order=1))
#Do the number of drug users granger cause the number of drug related arrests?
print(grangertest(arrerstsandusers$Arrests_drugabuse~arrerstsandusers$Users_alldrugs,order=1))


#Testing the impact of other counter-supply measures, such as erradication, on number of users (although would think that the relationship should be taken into account in the test on prices??)
usersanderradication<-read.csv("usersanderradication.csv")
plot(usersanderradication$Cocaine_users,main="Number of cocaine users")
plot(usersanderradication$Colombia_coca_leaf_erradicated_manual, main="Colombia Coca Leaf Erradicated")
#Does erradication granger cause cocaine users?
print(grangertest(usersanderradication$Cocaine_users~usersanderradication$Colombia_coca_leaf_erradicated_manual,order=1))
#Do cocaine users Granger cause erradication?
print(grangertest(usersanderradication$Colombia_coca_leaf_erradicated_manual~usersanderradication$Cocaine_users,order=1))

plot(usersanderradication$Marijuana_users,main="Marijuana users")
plot(usersanderradication$Erradicated_domestic_cannabis,main="erradicated marijuana")
#Does Erradication granger cause Marijuana users?
print(grangertest(usersanderradication$Marijuana_users~usersanderradication$Erradicated_domestic_cannabis,order=1))
#Do Marijuana users Granger cause erradication?
print(grangertest(usersanderradication$Erradicated_domestic_cannabis~usersanderradication$Marijuana_users,order=1))


#Testing the impact on users of counter-demand measures, such as arrests, but using deaths as a proxy for demand since there is more data#
arrestsanddeaths<-read.csv("arrestsanddeaths.csv")

plot(arrestsanddeaths$Drug_induced_deaths,main="Number of drug induced deaths")
plot(arrestsanddeaths$Drug_abuse_arrests, main="number of arrests for drug abuse")
#Do arrersts for drug abuse granger cause drug induce deaths?
print(grangertest(arrestsanddeaths$Drug_induced_deaths~arrestsanddeaths$Drug_abuse_arrests,order=1))
#Do drug indiced deaths granger cause arrersts for drug abuse?
print(grangertest(arrestsanddeaths$Drug_abuse_arrests~arrestsanddeaths$Drug_induced_deaths,order=1))


#MULTIVARIATE REGRESSIONS PER DRUG AND IN TOTAL#
cocainemodel<-read.csv("cocainemodel.csv")
install.packages("foreign")
library("foreign")
multivariateregressioncocaine<-lm(cocainemodel$Cocaine_deaths~cocainemodel$Seizures_cocain_kg+cocainemodel$Colombia_Coca_Leaf_Erradicated_aerial+cocainemodel$Drug_suppliers_arrests+cocainemodel$Drug_users_arrests+cocainemodel$Above_the_influence)
summary(multivariateregressioncocaine<-lm(cocainemodel$Cocaine_deaths~cocainemodel$Seizures_cocain_kg+cocainemodel$Colombia_Coca_Leaf_Erradicated_aerial+cocainemodel$Drug_suppliers_arrests+cocainemodel$Drug_users_arrests+cocainemodel$Above_the_influence))

heroinmodel<-read.csv("heroinmodel.csv")
multivariateregressionheroin<-lm(heroinmodel$Heroin_deaths~heroinmodel$Heroin_seizures+heroinmodel$Poppy_erradication+heroinmodel$Drug_suppliers_arrests+heroinmodel$Drug_abuse_arrests+heroinmodel$Above_the_influence_campaign)
summary(multivariateregressionheroin<-lm(heroinmodel$Heroin_deaths~heroinmodel$Heroin_seizures+heroinmodel$Poppy_erradication+heroinmodel$Drug_suppliers_arrests+heroinmodel$Drug_abuse_arrests+heroinmodel$Above_the_influence_campaign))

methamphetaminemodel<-read.csv("Methamphetaminemodel.csv")
multivariateregressionmetamp<-lm(methamphetaminemodel$Psychostimulants_deaths~methamphetaminemodel$Methamphetamine_seizures+methamphetaminemodel$Poppy_Erradicated+methamphetaminemodel$Drug_suppliers_arrests+methamphetaminemodel$Drug_abuse_arrests+methamphetaminemodel$Above_the_influence)
summary(multivariateregressionmetamp<-lm(methamphetaminemodel$Psychostimulants_deaths~methamphetaminemodel$Methamphetamine_seizures+methamphetaminemodel$Poppy_Erradicated+methamphetaminemodel$Drug_suppliers_arrests+methamphetaminemodel$Drug_abuse_arrests+methamphetaminemodel$Above_the_influence))

modeltotal<-read.csv("modeltotal.csv")
multivariateregmodeltotal<-lm(modeltotal$Drug_induced_deaths~modeltotal$Drug_abuse_arrests+modeltotal$Drug_sales_manufacturing_arrests+modeltotal$Cocaine_seizures+modeltotal$Heroin_seizures+modeltotal$Cannabis_seizures+modeltotal$Methamphetamine_seizures+modeltotal$Erradicated_domesticcannabis_totalplants+modeltotal$Mexico_opium_poppy_erradicated+modeltotal$Prevention_campaigns)
summary(multivariateregmodeltotal<-lm(modeltotal$Drug_induced_deaths~modeltotal$Drug_abuse_arrests+modeltotal$Drug_sales_manufacturing_arrests+modeltotal$Cocaine_seizures+modeltotal$Heroin_seizures+modeltotal$Cannabis_seizures+modeltotal$Methamphetamine_seizures+modeltotal$Erradicated_domesticcannabis_totalplants+modeltotal$Mexico_opium_poppy_erradicated+modeltotal$Prevention_campaigns))


######GRANGER CAUSALITY DRUG PROJECT#####
rm(list = ls())
graphics.off()
install.packages("lmtest")
library("lmtest")
install.packages("ggplot2")
library("ggplot2")
setwd("C:/Users/virlo/Documents/SEST701")
cocaineconsumptionandprices<-read.csv("consumptionandprices.csv")
par(mfrow=c(2,1),mar=c(2,2,2,2))
#Testing Becker's view that when demand is inelastic, higher prices do not curtail consumption#
plot(cocaineconsumptionandprices$cocaine_consumption, main="Cocaine Consumption (Metric Tons)")
plot(cocaineconsumptionandprices$cocaine_prices, main="Cocaine prices (2012 dollars)")
#Do cocaine prices Granger cause cocaine consumption?
print(grangertest(cocaineconsumptionandprices$cocaine_consumption~cocaineconsumptionandprices$cocaine_prices,order=1))
#Does cocaine consumption Granger cause cocaine prices?
print(grangertest(cocaineconsumptionandprices$cocaine_prices~cocaineconsumptionandprices$cocaine_consumption,order=1))
cocaineconsumptionandpx<-read.csv("cocaineconsumptionandpx.csv")
heroinconsumptionandprice<-read.csv("heroinconsumptionandprices.csv")

plot(cocaineconsumptionandpx$cocaine_consumption, main="Cocaine consumption (Grams)")
plot(cocaineconsumptionandpx$cocaine_prices, main="Cocaine price per pure gram (2012 Dollars)")
#COCAINE#
#Do cocaine prices Granger cause cocaine consumption?
print(grangertest(cocaineconsumptionandpx$cocaine_consumption~cocaineconsumptionandpx$cocaine_prices,order=1))
#Does cocaine consumption Granger cause cocaine prices?
print(grangertest(cocaineconsumptionandpx$cocaine_prices~cocaineconsumptionandpx$cocaine_consumption,order=1))

#HEROIN#
plot(heroinconsumptionandprice$heroin_consumption,main="Heroin consumption (Grams)")
plot(heroinconsumptionandprice$heroin_prices,main ="Heroin price per pure gram (2012 Dollars)")

#Do heroin prices Granger cause heroin consumption?
print(grangertest(heroinconsumptionandprice$heroin_consumption~heroinconsumptionandprice$heroin_prices,order=1))
#Does heroin consumption Granger cause heroin prices?
print(grangertest(heroinconsumptionandprice$heroin_prices~heroinconsumptionandprice$heroin_consumption,order=1))

#METHAMPHETAMINE#
methaconsumptionandprice<-read.csv("methaconsumptionandprice.csv")
plot(methaconsumptionandprice$methamphethamine_consumption,main="Methamphetamine consumption (Grams)")
plot(methaconsumptionandprice$methamphethamine_price,main="Methamphetamine price per pure gram (2012 Dollars)")

#Do methamphetamine prices Granger cause methamphetamine consumption?
print(grangertest(methaconsumptionandprice$methamphethamine_consumption~methaconsumptionandprice$methamphethamine_price,order=1))
#Does methamphetamine consumption Granger cause methamphetamine prices?
print(grangertest(methaconsumptionandprice$methamphethamine_price~methaconsumptionandprice$methamphethamine_consumption,order=1))

#MARIJUANA#
marijuanaconsumptionandpx<-read.csv("marijuanaconsumptionandprice2.csv")
plot(marijuanaconsumptionandpx$marijuana_consumption,main="Marijuana consumption (Grams)")
plot(marijuanaconsumptionandpx$marijuana_price, main ="Marijuana Price per bulk gram (2012 Dollars)")

#Do Marijuana prices Granger cause marijuana consumption?
print(grangertest(marijuanaconsumptionandpx$marijuana_consumption~marijuanaconsumptionandpx$marijuana_price,order=1))
#Does marijuana consumption granger cause marijuana prices?
print(grangertest(marijuanaconsumptionandpx$marijuana_price~marijuanaconsumptionandpx$marijuana_consumption,order=1))

#TESTING THE UNINTENDED CONSEQUENCES OF CS MEASURES (PRODUCT SUBSTITUTION)#
cSunintended1<-read.csv("CSunintended.csv")

plot(cSunintended1$Mexico_poppy_eradication,main="Mexico Opium Poppy Eradicated (Hectares)")
plot(cSunintended1$Colombia_coca_cultivation,main = "Colombia Net Coca Cultivation (Hectares)")
#Does poppy eradication Granger cause coca cultivation?
print(grangertest(cSunintended1$Colombia_coca_cultivation~cSunintended1$Mexico_poppy_eradication,order=1))
#Does coca cultivation Granger cause poppy eradication?
print(grangertest(cSunintended1$Mexico_poppy_eradication~cSunintended1$Colombia_coca_cultivation,order=1))


#TESTING THE HYPOTHESIS THAT THE LEGALIZATION OF MARIJUANA CAUSED MEXICAN DRUG CARTELS TO SHIFT TO HEROIN##



#TESTING THE TOTAL MODEL WITH REGRESSION#
install.packages("foreign")
library("foreign")
totalmodel<-read.csv("totalmodel.csv")
multivariateregressiontotalmodel<-lm(totalmodel$Drug_users~totalmodel$Drug_users_arrests+totalmodel$Drug_sellers_arrests+totalmodel$Drug_treatment_patients+totalmodel$Drug_prevention+totalmodel$Cocaine_seizures+totalmodel$Heroin_seizures+totalmodel$Methamphetamine_seizures)
summary(multivariateregressiontotalmodel<-lm(totalmodel$Drug_users~totalmodel$Drug_users_arrests+totalmodel$Drug_sellers_arrests+totalmodel$Drug_treatment_patients+totalmodel$Drug_prevention+totalmodel$Cocaine_seizures+totalmodel$Heroin_seizures+totalmodel$Methamphetamine_seizures))




#plot the time series
par(mfrow=c(2,1),mar=c(2,2,2,2)) # Two rows, one column, sets margins
plot(chicken, main="Chicken") # Plot time series
plot(egg, main="Egg")

dchick <- diff(chicken) # Calculate derivative
ddegg <- diff(egg)
plot.ts(dchick,main="Delta Chicken")
plot.ts(degg,main = "Delta Egg")

# Do eggs Granger cause chickens? Using derivative data, as in the example
print(grangertest(dchick ~ degg, order=4))

# Do chickens Granger cause eggs, at lag 4? Using derivative data, as in the example
print(grangertest(degg ~ dchick, order=4))

# Do eggs Granger cause chickens? Using original data
print(grangertest(chicken ~ egg, order=4))

# Do chickens Granger cause eggs? Using original data
print(grangertest(egg ~ chicken, order=4))

# Exercise 2: Does pollution cause the stock market to dip?
# Load data
df<-read.csv("pollutant_SP500.csv", header = TRUE)
attach(df) # Lets you use column names as variables. Otherwise, you'd have to say df$Anti, which is messier

# plot the time series
par(mfrow=c(2,1),mar=c(2,2,2,2))
plot.ts(Particulate,main="Particulate")
plot.ts(SP500,main="S&P 500")

print(grangertest(Particulate ~ SP500, order=6))
print(grangertest(SP500 ~ Particulate, order=6))

# Exercise 3: Use same method for terrorism example: Do cleric statements Granger cause bombings?
# Load data
df<-read.csv("clerics_data.csv", header = TRUE)
attach(df) # Lets you use column names as variables. Otherwise, you'd have to say df$Anti, which is messier

# plot the time series
par(mfrow=c(3,1),mar=c(2,2,2,2))
plot.ts(Pro,main="Pro")
plot.ts(Anti,main="Anti")
plot.ts(Bombings,main="Bombings")

print(grangertest(Bombings ~ Pro, order=1))
print(grangertest(Bombings ~ Anti, order=1))
print(grangertest(Pro ~ Bombings, order=1))
print(grangertest(Anti ~ Bombings, order=1))

##################################
# Pearl Causality: Berkeley Demo #
##################################

# Exercise 4
# 1. Setting the seed
set.seed(252)

#2. Setting the number of observations
n<- 5000

#3. Simulating the unmeasured confounders; formulas are given
U.W1<- runif(n, min=0, max=1)
U.W2<- rbinom(n, size=1, prob=0.5)
U.A<- rnorm(n, mean=-3, sd=1)
U.Y<- rnorm(n, mean=0, sd=0.3)

#4. Given the random input, deterministically evaluate the structural equations F
# These structural equations are also given

W1 = as.numeric( U.W1 < 0.35)
W2 = W1 + 2*U.W2
A = as.numeric( 1+W1+2*W2 + U.A > 0)
Y = 1 +2.5*A + 3*W1 - 0.25*A*W1 + U.Y
X<- data.frame(W1, W2, A, Y)
print(head(X))
print(summary(X))

# 1. intervene to set A=a and generate the counterfactual outcomes Y.a
Y.1<- 1 +2.5*1 + 3*W1 - 0.25*1*W1 + U.Y
Y.0<- 1 +2.5*0 + 3*W1 - 0.25*0*W1 + U.Y

# 2. add these columns to the dataframe
X<- data.frame(X, Y.1, Y.0)
head(X)

# Given input of the background factors U, the structural equations are deterministic.
#4. Evaluation: next line is the average happiness given that a person drinks butterbeer minus the average happiness given that a person does not drink butterbeer.
Psi.F<- mean(Y.1 - Y.0)
Psi.F





      



