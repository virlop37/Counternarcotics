

####GRANGER TESTS WITH SEIZURES AND PRICES####

rm(list=ls())
graphics.off()
install.packages("lmtest")
library("lmtest")
install.packages("ggplot2")
library("ggplot2")
par(mfrow=c(2,1),mar=c(2,2,2,2))
setwd("C:/Users/virlo/Documents/SEST701")


#COCAINE#
cocaine<-read.csv("seizurescocaine.csv")

#Do cocaine seizures Granger-cause cocaine prices?
print(grangertest(cocaine$cocaine_prices_per_dose~cocaine$cocaine_seizures,order=1))
#Do cocaine prices Granger cause cocaine seizures?
print(grangertest(cocaine$cocaine_seizures~cocaine$cocaine_prices_per_dose,order=1))

#HEROIN#
heroin<-read.csv("seizuresheroin.csv")

#Do heroin seizures Granger-cause heroin prices?
print(grangertest(heroin$heroin_price_per_dose~heroin$heroin_seizures,order=1))
#Do heroin prices Granger-cause heroin seizures?
print(grangertest(heroin$heroin_seizures~heroin$heroin_price_per_dose,order=1))

#METHAMPHETAMINE#
methamphetamine<-read.csv("seizuresmethamphetamine.csv")

#Do methamphetamine seizures Granger-cause methamphetamine prices? at 1000mg per dose
print(grangertest(methamphetamine$methamphetamine_price_per_dose~methamphetamine$methamphetamine_seizures,order=1))
#Do methamphetamine prices Granger cause methamphetamine seizures?
print(grangertest(methamphetamine$methamphetamine_seizures~methamphetamine$methamphetamine_price_per_dose,order=1))

#Do methamphetamine seizures Granger-cause methamphetamine prices? at 500mg per dose
print(grangertest(methamphetamine$methamphetamine_price_per_dose2~methamphetamine$methamphetamine_seizures,order=1))
#Do methamphetamine prices Granger-cause methamphetamine seizures?
print(grangertest(methamphetamine$methamphetamine_seizures~methamphetamine$methamphetamine_price_per_dose2,order=1))

#Do methamphetamine seizures Granger cause methamphetamine prices? at 200mg per dose
print(grangertest(methamphetamine$methamphetamine_price_per_dose3~methamphetamine$methamphetamine_seizures,order=1))
#Do methamphetamine prices Granger-cause methamphetamine seizures?
print(grangertest(methamphetamine$methamphetamine_seizures~methamphetamine$methamphetamine_price_per_dose3,order=1))

#Do methamphetamine seizures Granger cause methamphetamine prices? at 5000mg per dose 
print(grangertest(methamphetamine$methamphetamine_price_per_dose_4~methamphetamine$methamphetamine_seizures,order=1))
#Do methamphetamine prices Granger cause methamphetamine seizures?
print(grangertest(methamphetamine$methamphetamine_seizures~methamphetamine$methamphetamine_price_per_dose_4,order=1))

#Do methamphetamine seizures Granger cause methamphetamine prices? at 20mg per dose
print(grangertest(methamphetamine$methamphetamine_price_per_dose_5~methamphetamine$methamphetamine_seizures,order=1))
#Do methamphetamine prices granger cause methamphetamine seizures?
print(grangertest(methamphetamine$methamphetamine_seizures~methamphetamine$methamphetamine_price_per_dose_5,order=1))

#MARIJUANA#
marijuana<-read.csv("seizuresmarijuana.csv")

#Do marijuana seizures Granger cause marijuana prices?
print(grangertest(marijuana$marijuana_price_per_cigarette~marijuana$marijuana_seizures,order=1))
#Do marijuana prices Granger cause marijuana seizures?
print(grangertest(marijuana$marijuana_seizures~marijuana$marijuana_price_per_cigarette,order=1))

###GRANGER TESTS OF UNINTENDED CONSEQUENCES OF SEIZURES###

#COCAINE#
unintendedcocaine<-read.csv("potprodcocaine.csv")

#Do cocaine seizures Granger cause potential coca production?
print(grangertest(unintendedcocaine$colombia_potential_coca_production~unintendedcocaine$cocaine_seizures,order=1))
#Does coca potential production Granger cause seizures?
print(grangertest(unintendedcocaine$cocaine_seizures~unintendedcocaine$colombia_potential_coca_production,order=1))

#HETROIN#
unintendedheroin<-read.csv("potprodheroin.csv")

#Do heroin seizures Granger-cause poppy potential production?
print(grangertest(unintendedheroin$Mexican_heroin_potential_production~unintendedheroin$heroin_seizures,order=1))
#Does poppy potential production Granger cause seizures?
print(grangertest(unintendedheroin$heroin_seizures~unintendedheroin$Mexican_heroin_potential_production,order=1))


