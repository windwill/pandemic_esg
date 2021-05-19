# This file includes some R codes that are used when conducting the CIA sponsored research
# project "Scenario Generation for Pandemics"
# The code is for eductional purpose only. It is provided 'as is' without warranty of any kind, 
# either express or implied, warranties of fitness for a purpose, or the warranty of non-infringement. 
# Although the authors try their best to test the tool, they make no warranty that
# (1) it will meet your requirements;
# (2) it will be secure or error-free;
# (3) the results that may be obtained from the use of the code will be effective, accurate or reliable;
# (4) any errors in the tool will be corrected.

# The CIA, the project oversight group and the author assume no responsibility for errors or omissions 
# in the code or related documentation. In no event shall the sponsor and the authors be liable to you 
# or any third parties for any damages of any kind arising out of or in connection with the use of the code. 

# You may contact Kailan Shang at klshang81@gmail.com for any questions about the code.

# R is a open-source statistical software and can be downloaded at www.r-project.org
# The codes have been tested using R4.0.4

############################################################################
# This is the data summary example in Section 2.2.1
############################################################################

# Set working directory
setwd("C:/dsge/form/pc")
# Load historical pandemic data
df<-read.csv("pandemic_data.csv")

# Descriptive Statistics
#install.packages("pastecs")
library(pastecs)
options(scipen=100)
options(digits=2)
stat.desc(df)
write.csv(stat.desc(df),'descriptive_stat_all.csv')

# Frequency Graph 
start_yr = -1050
end_yr = 2050
diff<-50
breaks <- seq(start_yr,end_yr,diff)
breaks_1 <- seq(start_yr,end_yr-diff,diff)
breaks_2 <- seq(start_yr+diff,end_yr,diff)
tags = list()
for (i in c(1:length(breaks_1))){
	if (breaks_1[i]<0){
		tags <- c(tags, paste0(-breaks_1[i],"BC"))
	}else{
		tags <- c(tags, paste0(breaks_1[i]))
	}
}
grouped_pandemics <- cut(df$Start_Year, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
summary(grouped_pandemics)
plot(grouped_pandemics,
     main="Pandemic/Epidemic Frequency", 
     xlab="Year",
	 ylab="Count",
     border="white", 
     col="blue",
	 las=2,
	 cex.sub=1
)

# Draw frequency map for pandemic/epidemic events
install.packages("ggplot2")
install.packages("maps")
install.packages("dplyr")
library(ggplot2)
require(maps)
library("dplyr")

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") 

regions <- list()
for (i in df$Country_Region) {
	regions<-c(regions,strsplit(i,', ',fixed=TRUE))
}
region <- unlist(regions)

freq_map <-as.data.frame(table(region))

freq_map <- left_join(freq_map, world_map, by = "region")
ggplot(freq_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Freq ), color = "white")
  # +  scale_fill_viridis_c(option = "C")

# Boxplot of rates
boxplot(df$Case_Fatality_Rate, df$Mortality_Rate, df$Infection_Rate,
# main = "Multiple boxplots",
at = c(1,2,3),
names = c("Case Fatality Rate", "Mortality Rate","Infection Rate"),
las = 1,
# col = c("blue","blue","blue"),
# border = "white",
horizontal = FALSE,
notch = FALSE
)

# Histogram of duration
hist(df$Duration, 
     main="Histogram of Pandemic/Epidemic Duration", 
     xlab="Duration", 
     border="white", 
     col="blue",
     xlim=c(0,30),
     las=1, 
     breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,50,200))


############################################################################
# This is the data summary example in Section 2.2.2
############################################################################
# Data filtering
df_pandemic <- with(df, df[ Pandemic=="Yes", ])
stat.desc(df_pandemic)
write.csv(stat.desc(df_pandemic),'descriptive_stat_pandemic.csv')

# Frequency Graph 
start_yr = 500
end_yr = 2050
diff<-50
breaks <- seq(start_yr,end_yr,diff)
breaks_1 <- seq(start_yr,end_yr-diff,diff)
breaks_2 <- seq(start_yr+diff,end_yr,diff)
tags = list()
for (i in c(1:length(breaks_1))){
	if (breaks_1[i]<0){
		tags <- c(tags, paste0(-breaks_1[i],"BC"))
	}else{
		tags <- c(tags, paste0(breaks_1[i]))
	}
}
grouped_pandemics <- cut(df_pandemic$Start_Year, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
summary(grouped_pandemics)
plot(grouped_pandemics,
     main="Pandemic Frequency", 
     xlab="Year",
	 ylab="Count",
     border="white", 
     col="blue",
	 las=2,
	 cex.sub=1
)

# Boxplot of rates
boxplot(df_pandemic$Case_Fatality_Rate, df_pandemic$Mortality_Rate, df_pandemic$Infection_Rate,
# main = "Multiple boxplots",
at = c(1,2,3),
names = c("Case Fatality Rate", "Mortality Rate","Infection Rate"),
las = 1,
# col = c("blue","blue","blue"),
# border = "white",
horizontal = FALSE,
notch = FALSE
)

# Histogram of duration
hist(df_pandemic$Duration, 
     main="Histogram of Pandemic Duration", 
     xlab="Duration", 
     border="white", 
     col="blue",
     xlim=c(0,30),
     las=1, 
     breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,50,200))

############################################################################
# This is the distribution fitting example in Section 3.1
############################################################################
install.packages("fitdistplus")
require(fitdistrplus)

# Frequency Data Graph (1800-2020)
start_yr = 1800
end_yr = 2020
diff<-1
breaks <- seq(start_yr,end_yr,diff)
breaks_1 <- seq(start_yr,end_yr-diff,diff)
breaks_2 <- seq(start_yr+diff,end_yr,diff)
tags = list()
for (i in c(1:length(breaks_1))){
	if (breaks_1[i]<0){
		tags <- c(tags, paste0(-breaks_1[i],"BC"))
	}else{
		tags <- c(tags, paste0(breaks_1[i]))
	}
}
grouped_pandemics <- cut(df$Start_Year, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
summary(grouped_pandemics)
plot(grouped_pandemics,
     main="Pandemic/Epidemic Frequency (1800-2020)", 
     xlab="Year",
	 ylab="Count",
     border="white", 
     col="blue",
	 las=2,
	 cex.sub=1
)

# Frequency
dist_type = "nbinom" #pois, geom, nbinom, multinom
freq_data <- as.data.frame(table(as.data.frame(grouped_pandemics)))
fit_result<-fitdist(freq_data$Freq, distr=dist_type, method="mle", discrete=TRUE) #, start=list(size=15, prob=0.5)
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE)
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

# Duration
dist_type = "lnorm" #exp, norm, lnorm, gamma, weibull
dur_data <- df$Duration
dur_data <- ifelse(dur_data<=0, 0.01, dur_data)
dur_data <- dur_data[dur_data<=50]
fit_result<-fitdist(dur_data, distr=dist_type, method="mle", discrete=FALSE) #, start=list(size=15, prob=0.5)
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE)
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

# Case Fatality Rate
dist_type = "gamma" #exp, norm, lnorm, gamma, weibull, unif
cfr_data <- df$Case_Fatality_Rate
cfr_data<-cfr_data[!is.na(cfr_data)]
cfr_data <- ifelse(cfr_data<=0, 0.00001, cfr_data)
fit_result<-fitdist(cfr_data, distr=dist_type, method="mle", discrete=FALSE)
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE, ylim=c(0,1))
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

dist_type = "beta"
cfr_data <- df$Case_Fatality_Rate
cfr_data<-cfr_data[!is.na(cfr_data)]
cfr_data <- ifelse(cfr_data<=0, 0.00001, cfr_data)
fit_result<-fitdist(cfr_data, distr=dist_type, method="mme", discrete=FALSE) #, start=list(shape1=0.4, shape2=1.3))
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE, ylim=c(0,1))
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

# Mortality Rate
dist_type = "beta" #exp, norm, lnorm, gamma, weibull, unif, beta
mr_data <- df$Mortality_Rate
mr_data<-mr_data[!is.na(mr_data)]
mr_data <- ifelse(mr_data<=0, 0.00001, mr_data)
fit_result<-fitdist(mr_data, distr=dist_type, method="mle", discrete=FALSE)
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE, ylim=c(0,1))
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

# Infection Rate
dist_type = "gamma" #exp, norm, lnorm, gamma, weibull, unif
ir_data <- df$Infection_Rate
ir_data<-ir_data[!is.na(ir_data)]
ir_data <- ifelse(ir_data<=0, 0.00001, ir_data)
fit_result<-fitdist(ir_data, distr=dist_type, method="mle", discrete=FALSE)
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE, ylim=c(0,1))
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

dist_type = "beta"
ir_data <- df$Infection_Rate
ir_data<-ir_data[!is.na(ir_data)]
ir_data <- ifelse(ir_data<=0, 0.00001, ir_data)
fit_result<-fitdist(ir_data, distr=dist_type, method="mme", discrete=FALSE)
summary(fit_result)
cdfcomp(fit_result, addlegend=FALSE)
denscomp(fit_result, addlegend=TRUE, ylim=c(0,1))
ppcomp(fit_result, addlegend=FALSE)
qqcomp(fit_result, addlegend=FALSE)

############################################################################
# This is the HMM example in Section 3.2
###########################################################################

# install.packages("HMM")
library(HMM)

# Set the initial HMM. Two states: "H" and "L". Seven outcomes (No. of pandemic/epidemic outbreak in each year)
# "a"->0, "b"->1, "c"->2, "d"->3, "e"->4, "f"->5, "g"->6 and above
# Initial state probabilities: P(H)=20%, P(L)=80%
# Trasition Probabilities: P(H|H)=70%, P(H|L)=10%, P(L|H)=30%, P(L|L)=90%
# Emission Probabilities (The probability distribution of the outcome based on the hidden state)
# State "H": P(a)=0%, P(b)=0%, P(c)=0%, P(d)=25%, P(e)=25%, P(f)=25%, P(g)=25%
# State "L": P(a)=30%, P(b)=30%, P(c)=20%, P(d)=15%, P(e)=5%, P(f)=0%, P(g)=0%
  
hmm1<-initHMM(c("H","L"),c("a","b","c","d","e","f","g"),startProbs=c(0.2,0.8),transProbs=matrix(c(0.7,0.1,0.3,0.9),2),
emissionProbs=matrix(c(0,.3,.0,.3,.0,.2,.25,.15,.25,.05,.25,0,.25,0),2))

# Replace frequency data from numerical value to letters
Obv <- replace(freq_data$Freq, freq_data$Freq>=6, "g")
Obv <- replace(Obv, Obv==0, "a")
Obv <- replace(Obv, Obv==1, "b")
Obv <- replace(Obv, Obv==2, "c")
Obv <- replace(Obv, Obv==3, "d")
Obv <- replace(Obv, Obv==4, "e")
Obv <- replace(Obv, Obv==5, "f")

# Calibrate the HMM using Baum-Welch algorithm
bw=baumWelch(hmm1, Obv, maxIterations=100)
print(bw$hmm)

# Find out the most possible path of state based on the historical data using Viterbi algorithm
viterbi = viterbi(bw$hmm,Obv)
print(viterbi)

# Simulation for one period

# Update the HMM using the calibration result. 
# Two states: "H" and "L". Seven outcomes (No. of earthquakes in each year)
# "a"->0, "b"->1, "c"->2, "d"->3, "e"->4, "f"->5, "g"->6
# Initial state probabilities: P(H)=0%, P(L)=100%
# Trasition Probabilities: P(H|H)=43%, P(H|L)=18%, P(L|H)=57%, P(L|L)=82%
# Emission Probabilities (The probability distribution of the outcome based on the hidden state)
# State "H": P(a)=0%, P(b)=0%, P(c)=0%, P(d)=0.027%, P(e)=38%, P(f)=18%, P(g)=45%
# State "L": P(a)=17%, P(b)=31%, P(c)=24%, P(d)=20.142%, P(e)=7%, P(f)=0%, P(g)=0%

# Simulation assuming the intial state as "L"
hmm2<-initHMM(c("H","L"),c("a","b","c","d","e","f","g"),startProbs=c(0,1),transProbs=matrix(c(0.43,0.18,0.57,0.82),2),
emissionProbs=matrix(c(0,.17,.0,.31,.0,.24,.00027,.20142,.38,.07,.18,0,.45,0),2))

# No. of simulation
n<-1000

# Set the random number generation seed
set.seed(123)

# Initialize the vectors storing the simulated outcomes (sr) and states (ss)
sr<-rep(NA,n)
ss<-rep(NA,n)

# Simulating the next period pandemic/epidemic frequency. 
# Two-period simulation is required as the first one is the current time period. 
for(i in 1:n){
a<-simHMM(hmm2,2)
sr[i]<-a$observation[2]
ss[i]<-a$states[2]}

# Output the simulation result
write.csv(sr,"sr_l.csv")
write.csv(ss,"ss_l.csv")

# Simulation assuming the intial state as "H"
hmm3<-initHMM(c("H","L"),c("a","b","c","d","e","f","g"),startProbs=c(1,0),transProbs=matrix(c(0.43,0.18,0.57,0.82),2),
emissionProbs=matrix(c(0,.17,.0,.31,.0,.24,.00027,.20142,.38,.07,.18,0,.45,0),2))

# No. of simulation
n<-1000

#Set the random number generation seed
set.seed(123)

# Initialize the vectors storing the simulated outcomes (sr) and states (ss)
sr1<-rep(NA,n)
ss1<-rep(NA,n)

# Simulating the next period pandemic/epidemic frequency. 
# Two-period simulation is required as the first one is the current time period. 
for(i in 1:n){
a<-simHMM(hmm3,2)
sr1[i]<-a$observation[2]
ss1[i]<-a$states[2]}

# Output the simulation result
write.csv(sr1,"sr_h.csv")
write.csv(ss1,"ss_h.csv")

############################################################################
# This is the copula example in Section 4
###########################################################################
# Copula Simulation
install.packages("copula")
library(copula)
ind<-indepCopula(dim = 2)
n<-2000
innin<- rCopula(n, ind)
plot(innin, main="Independent Copula", xlab="x", ylab="y", cex = 1)