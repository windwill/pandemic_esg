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
     ylab="Probability Density", 
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
	 ylab="Probability Density",
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
start_yr = 1801
end_yr = 2021
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
     main="Pandemic/Epidemic Frequency (1801-2020)", 
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

# EVT using GPD
install.packages("fExtremes")
library(fExtremes)

u = 7 #threshold
# MLE esitmation for the shape parameter for experience data
fitYgpd = gpdFit(freq_data$Freq, u = u, type = "mle")
fitYgpd

r = rnbinom(n=1000, size=2.7, mu=2.7)
u
x0 <- r[r > u]
fitYgpd = gpdFit(x0, u = u, type = "mle")
fitYgpd

# plot
x0 <- freq_data$Freq - u
x0 <- x0[x0>0]
plot(sort(x0), (1:length(x0)/length(x0)),xlim = c(0,10), ylim = c(0, 1.1), pch = 19,
cex = 0.5, ylab = "Cumulative Probability", xlab = "Exceedance", main = "Distribution of Exceedance(Freq-7)")
grid()
q = seq(0, 10, by = 0.1)
lines(q, pgpd(q, xi = -0.1622532 , beta = 4.0315693), col = "steelblue")
lines(q, pgpd(q, xi = -0.5130292, beta = 3.2499224), col = "green")
legend("bottomright", c("MLE","Negative Binomial"), lty=c(1,1), col=c("steelblue","green"))


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

# EVT using GPD
u = 7 #threshold
# MLE esitmation for the shape parameter for experience data
fitYgpd = gpdFit(dur_data, u = u, type = "mle")
fitYgpd

r = rlnorm(n=1000, meanlog=0.675609, sdlog=1.053685)
u
x0 <- r[r > u]
fitYgpd = gpdFit(x0, u = u, type = "mle")
fitYgpd

# plot
x0 <- dur_data - u
x0 <- x0[x0>0]
plot(sort(x0), (1:length(x0)/length(x0)),xlim = c(0,50), ylim = c(0, 1.1), pch = 19,
cex = 0.5, ylab = "Cumulative Probability", xlab = "Exceedance", main = "Distribution of Exceedance(Duration-7)")
grid()
q = seq(0, 50, by = 0.1)
lines(q, pgpd(q, xi = -0.2694795 , beta = 14.8583624), col = "steelblue")
lines(q, pgpd(q, xi = 0.2465085, beta = 4.2828102), col = "green")
legend("bottomright", c("MLE","Lognormal"), lty=c(1,1), col=c("steelblue","green"))

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
write.csv(viterbi,"viterbi.csv")

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
# This is the correlation examples in Section 4.1
###########################################################################
# Load historical economic data with annual frequency
df_econ<-read.csv("econ_data.csv")

# Correlation Matrices
cm_all <- cor(df_econ[,!(names(df_econ) %in% c('year','extreme_ind'))], use="pairwise.complete.obs")
round(cm_all, 2)

cm_extreme <- cor(df_econ[df_econ$extreme_ind == "Y",!(names(df_econ) %in% c('year','extreme_ind'))], use="pairwise.complete.obs")
round(cm_extreme, 2)

############################################################################
# This is the correlation examples in Section 4.2
###########################################################################
# Copula Simulation
install.packages("copula")
library(copula)

n<-2000
gaussian_cop <- normalCopula(0.85)
sim_cop<- rCopula(n, gaussian_cop)
plot(sim_cop, main="Gaussian Copula", xlab="x", ylab="y", cex = 1)

t_cop <- tCopula(0.85, df=5)
sim_cop<- rCopula(n, t_cop)
plot(sim_cop, main="t Copula", xlab="x", ylab="y", cex = 1)

gumbel_cop <- gumbelCopula(3)
sim_cop<- rCopula(n, gumbel_cop)
plot(sim_cop, main="Gumbel Copula", xlab="x", ylab="y", cex = 1)

clayton_cop <- claytonCopula(4)
sim_cop<- rCopula(n, clayton_cop)
plot(sim_cop, main="Clayton Copula", xlab="x", ylab="y", cex = 1)

frank_cop <- frankCopula(9.5)
sim_cop<- rCopula(n, frank_cop)
plot(sim_cop, main="Frank Copula", xlab="x", ylab="y", cex = 1)

ind_cop<-indepCopula(dim = 2)
sim_cop<- rCopula(n, ind_cop)
plot(sim_cop, main="Independent Copula", xlab="x", ylab="y", cex = 1)

# Copula Fitting
# Choose five variables and retain complete records
selected_vars <- c('year', 'extreme_ind', 'gdp_gr', 'inflation', 'ur', 'tby_10yr', 'sp500_rtn')
selected_data <- df_econ[,selected_vars]

cm_all <- cor(selected_data[,!(names(selected_data) %in% c('year','extreme_ind'))], use="pairwise.complete.obs")
round(cm_all, 2)

cm_extreme <- cor(selected_data[selected_data$extreme_ind == "Y",!(names(selected_data) %in% c('year','extreme_ind'))], use="pairwise.complete.obs")
round(cm_extreme, 2)

complete_data <- selected_data[complete.cases(selected_data), ]
cm_complete <- cor(complete_data[,!(names(complete_data) %in% c('year','extreme_ind'))], use="pairwise.complete.obs")
round(cm_complete, 2)

cm_complete_extreme <- cor(complete_data[complete_data$extreme_ind == "Y",!(names(complete_data) %in% c('year','extreme_ind'))], use="pairwise.complete.obs")
round(cm_complete_extreme, 2)

u <- pobs(complete_data[,!(names(complete_data) %in% c('year','extreme_ind'))])

## Maximum pseudo-likelihood method using Gaussian Copula
gaussian_cop_mpl <- fitCopula(normalCopula(dim=5, dispstr = "un"), u, method="mpl")
coef(gaussian_cop_mpl)
confint(gaussian_cop_mpl)
N=200
set.seed(123)
gofCopula(normalCopula(dim=5, dispstr = "un"), u, N=N, method="Sn", simulation="mult")

## Maximum pseudo-likelihood method using t Copula
t_cop_mpl <- fitCopula(tCopula(dim=5, dispstr = "un"), u, method="mpl")
coef(t_cop_mpl)
confint(t_cop_mpl)
N=200
set.seed(123)
gofCopula(tCopula(dim=5, dispstr = "un", df=9, df.fixed=TRUE), u, N=N, method="Sn", simulation="mult")

## Maximum pseudo-likelihood method using Gumbel Copula
gumbel_cop_mpl <- fitCopula(gumbelCopula(dim=5), u, method="mpl")
coef(gumbel_cop_mpl)
confint(gumbel_cop_mpl)
N=200
set.seed(123)
gofCopula(gumbelCopula(dim=5), u, N=N, method="Sn", simulation="mult")

## Maximum pseudo-likelihood method using Calyton Copula
clayton_cop_mpl <- fitCopula(claytonCopula(dim=5), u, method="mpl")
coef(clayton_cop_mpl)
confint(clayton_cop_mpl)
N=200
set.seed(123)
gofCopula(claytonCopula(dim=5), u, N=N, method="Sn", simulation="mult")

## Maximum pseudo-likelihood method using Frank Copula
frank_cop_mpl <- fitCopula(frankCopula(dim=5), u, method="mpl")
coef(frank_cop_mpl)
confint(frank_cop_mpl)
N=200
set.seed(123)
gofCopula(frankCopula(dim=5), u, N=N, method="Sn", simulation="mult")

## Compare copulas between inflation rates and S&P 500 index returns
n<-2000
gaussian_cop <- normalCopula(-0.24)
sim_cop<- rCopula(n, gaussian_cop)
plot(sim_cop, main="Gaussian Copula", xlab="x", ylab="y", cex = 1)

t_cop <- tCopula(-0.26, df=9)
sim_cop<- rCopula(n, t_cop)
plot(sim_cop, main="t Copula", xlab="x", ylab="y", cex = 1)

gumbel_cop <- gumbelCopula(1.081)
sim_cop<- rCopula(n, gumbel_cop)
plot(sim_cop, main="Gumbel Copula", xlab="x", ylab="y", cex = 1)

clayton_cop <- claytonCopula(0.143)
sim_cop<- rCopula(n, clayton_cop)
plot(sim_cop, main="Clayton Copula", xlab="x", ylab="y", cex = 1)

frank_cop <- frankCopula(0.571)
sim_cop<- rCopula(n, frank_cop)
plot(sim_cop, main="Frank Copula", xlab="x", ylab="y", cex = 1)

sim_cop<- u[,c('inflation','sp500_rtn')]
plot(sim_cop, main="Empirical Copula", xlab="x", ylab="y", cex = 1)

############################################################################
# This is the correlation examples in Section 4.3
###########################################################################
#Structured Model

# Load quarterly historical economic data
df_econ_q<-read.csv("econ_data_quarterly.csv")

#Structured Model VAR
#install.packages("vars")
library(vars)
Xnames <- c("gdp_gr","inflation","pce","fpi","ge","Fed_rate","ur")

Traindata <- df_econ_q[complete.cases(df_econ_q),names(df_econ_q) %in% c(Xnames)]
	
var1 <- VAR(Traindata, p = 1, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges

nr <- length(Xnames) + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(Xnames))
colnames(varoutput) <- Xnames

for (i in c(Xnames)) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)
tvaroutput <- tvaroutput[,c("gdp_gr.l1","inflation.l1","pce.l1","fpi.l1","ge.l1","Fed_rate.l1","ur.l1","const")]
A<-tvaroutput[,1:length(Xnames)]
B<-tvaroutput[,length(Xnames)+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans.csv")

econchol <- chol(summary(var1)$corres) #Cholesky decomposition of economic factor residuals
write.csv(econchol,"econchol.csv", row.names=FALSE)

#Regression Analysis

# install.packages(c("rpart","rpart.plot","FNN","gbm","randomForest","glmnet"))

library(rpart)
library(rpart.plot)
library(FNN)
library(gbm)
library(randomForest)
library(glmnet)

rawdata <- df_econ_q
Ynames <- names(df_econ_q)[!names(df_econ_q) %in% c(Xnames,"observation_date","extreme_ind","gdppc_gr")]

# define a dataframe that will be used to store the results
modeloutput <- data.frame(y=character(),
				 RMSE_train=double(), 		#RMSE based on training data
                 R2_train=double(),   		#R-squared based on training data
                 R2Adjust_train=double(), 	#Adjusted R-squared based on training data
				 RMSE=double(),				#RMSE based on valiation data
                 R2=double(),				#R-squared based on valiation data
                 R2Adjust=double(),			#Adjusted R-squared based on validation data
				 df=integer(),				#degree of freedom
				 fvar=double(),				#total variance
				 sfvar=double(),			#residual variance
				 rvar=double(),				#recession variance
				 srvar=double(),			#residual recession variance
				 corr=double(),				#correlation coefficient
				 scorr=double(),			#correlation coefficient during recessions
                 Models=character(),		#model type
                 stringsAsFactors=FALSE)


lag <-2 # how many lags to be used for regressions. A lag of 2 means x, x(-1), and x(-2) will be used in the regression.
residuals <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))		#store residuals of linear regression
residualsgbm <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))		#store residuals of GBM
residualsridge <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))	#store residuals of Ridge regression
residualslasso <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))	#store residuals of Lasso regression
residualsrf <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))		#store residuals of RandomForest
#set column names of these data frames that store residuals
colnames(residuals) <- Ynames											
colnames(residualsgbm) <- Ynames
colnames(residualsridge) <- Ynames
colnames(residualslasso) <- Ynames
colnames(residualsrf) <- Ynames

lambda = 0.5	#inital weight for regularization in Lasso and Ridge Regression. It is not used in the final optimized lambda value.
alpha = 0.5	#inital weight for regularization in Lasso and Ridge Regression. It is not used in the final optimized alpha value.
################################################################
# Model Calibration using training/validation split ############
################################################################

#You may see some warning messages populated for certain y variables and models. They may indicate some issues which can
#be caused by data scarcity. However, the model assessment is based on validation data and therefore the warnings may not
#be very meaningful for model selection.

for (y in Ynames) {

	Traindata <- rawdata[,names(rawdata) %in% c(y,Xnames,"extreme_ind")]
	Xnames1 <- names(Traindata)[!names(Traindata) %in% c("extreme_ind")] #get a list of variables to generate lagged variables
	
	#generate lagged variables
	if (lag > 0) {
		for (i in c(1:lag)){
			for (varname in Xnames1){
				Traindata[(i+1):nrow(Traindata),paste0(varname,i)] <- Traindata[1:(nrow(Traindata)-i),varname]
				Traindata[1:i,paste0(varname,i)] <- NA
			}
		}
	}
	
	#remove data records that have NA. If the lag equals 2, the first two records will be removed as x(-1) and x(-2) have no values
	Traindata <- na.omit(Traindata[(lag+1):nrow(Traindata),])
	
	Xnames2 <- names(Traindata)[!names(Traindata) %in% c(y,"extreme_ind")]
	
	#generate the formula used for calibration
	f <-as.formula(paste(y,"~",paste(Xnames2,collapse="+")))
	print(f)

	#split the data into training (80%) and validation (20%)
	set.seed(6)
	idx <- sample(seq(1, 2), size = nrow(Traindata), replace = TRUE, prob = c(.8, .2))
	training <- Traindata[idx==1,]
	validation <- Traindata[idx==2,]

	withCallingHandlers({
		#linear regression
		mdl <- "linear regression"
		lr<-lm(f, data=training)
		lrpredict <- predict(lr,training)
		rmse_train<-sqrt(mean((training[,y]-lrpredict)^2))
		r2_train<-1-sum((training[,y]-lrpredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		lrpredict <- predict(lr,validation)
		rmse<-sqrt(mean((validation[,y]-lrpredict)^2))
		r2<-1-sum((validation[,y]-lrpredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		lrpredict <- predict(lr, Traindata)
		fvar <- var(lrpredict)
		sfvar <- var(lrpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-lrpredict)
		srvar <- var((Traindata[,y]-lrpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-lrpredict,lrpredict)
		scorr<-cor((Traindata[,y]-lrpredict)[Traindata$extreme_ind=="Y"],lrpredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "LM")
		write.csv(summary(lr)$coefficients,paste0("lr_",y,".csv"))
		residuals[,y]<-c(rep(NA,nrow(residuals)-length(lr$residuals)),lr$residuals)
		
		#ridge regression
		mdl <- "ridge regression"
		ridge<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 0, lambda = lambda)
		cv.out <- cv.glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 0)
		lambda <- cv.out$lambda.min
		print(paste0(y," ridge regression: lambda = ", lambda))
		ridge<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 0, lambda = lambda)
		ridgepredict <- predict(ridge, s = lambda, newx = as.matrix(training[Xnames2]))
		rmse_train<-sqrt(mean((training[,y]-ridgepredict)^2))
		r2_train<-1-sum((training[,y]-ridgepredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		ridgepredict <- predict(ridge, s = lambda, newx = as.matrix(validation[Xnames2]))
		rmse<-sqrt(mean((validation[,y]-ridgepredict)^2))
		r2<-1-sum((validation[,y]-ridgepredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		ridgepredict <- predict(ridge, s = lambda, newx = as.matrix(Traindata[Xnames2]))
		fvar <- var(ridgepredict)
		sfvar <- var(ridgepredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-ridgepredict)
		srvar <- var((Traindata[,y]-ridgepredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-ridgepredict,ridgepredict)
		scorr<-cor((Traindata[,y]-ridgepredict)[Traindata$extreme_ind=="Y"],ridgepredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "Ridge")
		write.csv(rbind(as.matrix(ridge$a0), as.matrix(ridge$beta)),paste0("ridge_",y,".csv"))
		residual_ridge <- Traindata[,y]-ridgepredict
		residualsridge[,y]<-c(rep(NA,nrow(residualsridge)-length(residual_ridge)),residual_ridge)

		#lasso regression
		mdl <- "lasso"
		lasso<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 1)
		cv.out <- cv.glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 1)
		lambda <- cv.out$lambda.min
		print(paste0(y," lasso regression: lambda = ", lambda))
		lasso<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 1, lambda = lambda)
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(training[Xnames2]))
		rmse_train<-sqrt(mean((training[,y]-lassopredict)^2))
		r2_train<-1-sum((training[,y]-lassopredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(validation[Xnames2]))
		rmse<-sqrt(mean((validation[,y]-lassopredict)^2))
		r2<-1-sum((validation[,y]-lassopredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(Traindata[Xnames2]))
		fvar <- var(lassopredict)
		sfvar <- var(lassopredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-lassopredict)
		srvar <- var((Traindata[,y]-lassopredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-lassopredict,lassopredict)
		scorr<-cor((Traindata[,y]-lassopredict)[Traindata$extreme_ind=="Y"],lassopredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "lasso")
		write.csv(rbind(as.matrix(lasso$a0), as.matrix(lasso$beta)),paste0("lasso_",y,".csv"))
		residual_lasso <- Traindata[,y]-lassopredict
		residualslasso[,y]<-c(rep(NA,nrow(residualslasso)-length(residual_lasso)),residual_lasso)

		#cart
		mdl <- "CART"
		cart = rpart(f, data = training, cp = 10^(-3),minsplit = 10)
		cartpredict <- predict(cart, training)
		rmse_train<-sqrt(mean((training[,y]-cartpredict)^2))
		r2_train<-1-sum((training[,y]-cartpredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*(length(unique(cart$frame$var))-1)/(nrow(training)-(length(unique(cart$frame$var))-1)-1)
		cartpredict <- predict(cart, validation)
		rmse<-sqrt(mean((validation[,y]-cartpredict)^2))
		r2<-1-sum((validation[,y]-cartpredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*(length(unique(cart$frame$var))-1)/(nrow(training)-(length(unique(cart$frame$var))-1)-1)
		#rpart.plot(cart)
		df<-nrow(training)-(length(unique(cart$frame$var))-1)-1
		cartpredict <- predict(cart, Traindata)
		fvar <- var(cartpredict)
		sfvar <- var(cartpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-cartpredict)
		srvar <- var((Traindata[,y]-cartpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-cartpredict),cartpredict)
		scorr<-cor((Traindata[,y]-cartpredict)[Traindata$extreme_ind=="Y"],cartpredict[Traindata$extreme_ind=="Y"])

		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "CART")

		#knn
		mdl <- "KNN"
		knnreg <- knn.reg(train = training[,names(validation) %in% Xnames2] , test=training[,names(training) %in% Xnames2], y=training[,y], k=5, algorithm = "kd_tree")
		rmse_train<-sqrt(mean((training[,y]-knnreg$pred)^2))
		r2_train<-1-sum((training[,y]-knnreg$pred)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		knnreg <- knn.reg(train = training[,names(validation) %in% Xnames2] , test=validation[,names(validation) %in% Xnames2], y=training[,y], k=5, algorithm = "kd_tree")
		rmse<-sqrt(mean((validation[,y]-knnreg$pred)^2))
		r2<-1-sum((validation[,y]-knnreg$pred)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		knnreg <- knn.reg(train = training[,names(validation) %in% Xnames2] , test=Traindata[,names(Traindata) %in% Xnames2], y=training[,y], k=5, algorithm = "kd_tree")
		fvar <- var(knnreg$pred)
		sfvar <- var(knnreg$pred[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-knnreg$pred)
		srvar <- var((Traindata[,y]-knnreg$pred)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-knnreg$pred),knnreg$pred)
		scorr<-cor((Traindata[,y]-knnreg$pred)[Traindata$extreme_ind=="Y"],knnreg$pred[Traindata$extreme_ind=="Y"])

		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr,"KNN")

		#gbm
		mdl <- "GBM"
		set.seed(123) #reset random seed as gbm may use random subset
		gbmreg<-gbm(f, data=training, distribution = "gaussian", interaction.depth=6, n.minobsinnode = 2, bag.fraction=0.7, n.trees = 500)
		gbmpredict <- predict(gbmreg, training, n.trees = 100)
		rmse_train<-sqrt(mean((training[,y]-gbmpredict)^2))
		r2_train<-1-sum((training[,y]-gbmpredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		gbmpredict <- predict(gbmreg, validation, n.trees = 100)
		rmse<-sqrt(mean((validation[,y]-gbmpredict)^2))
		r2<-1-sum((validation[,y]-gbmpredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		gbmpredict <- predict(gbmreg, Traindata, n.trees = 100)	
		fvar <- var(gbmpredict)
		sfvar <- var(gbmpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-gbmpredict)
		srvar <- var((Traindata[,y]-gbmpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-gbmpredict),gbmpredict)
		scorr<-cor((Traindata[,y]-gbmpredict)[Traindata$extreme_ind=="Y"],gbmpredict[Traindata$extreme_ind=="Y"])
		residual_gbm <- Traindata[,y]-gbmpredict
		residualsgbm[,y]<-c(rep(NA,nrow(residualsgbm)-length(residual_gbm)),residual_gbm)

		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr,"gbm")

		#Random Forest
		mdl <- "RF"
		set.seed(123) #reset random seed as gbm may use random subset
		rfreg<-randomForest(f, data=training, nodesize = 2, ntree = 500)
		rfpredict <- predict(rfreg, training, n.trees = 100)
		rmse_train<-sqrt(mean((training[,y]-rfpredict)^2))
		r2_train<-1-sum((training[,y]-rfpredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		rfpredict <- predict(rfreg, validation, n.trees = 100)
		rmse<-sqrt(mean((validation[,y]-rfpredict)^2))
		r2<-1-sum((validation[,y]-rfpredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		rfpredict <- predict(rfreg, Traindata, n.trees = 100)	
		fvar <- var(rfpredict)
		sfvar <- var(rfpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-rfpredict)
		srvar <- var((Traindata[,y]-rfpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-rfpredict),rfpredict)
		scorr<-cor((Traindata[,y]-rfpredict)[Traindata$extreme_ind=="Y"],gbmpredict[Traindata$extreme_ind=="Y"])
		residual_rf <- Traindata[,y]-rfpredict
		residualsrf[,y]<-c(rep(NA,nrow(residualsrf)-length(residual_rf)),residual_rf)

		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr,"rf")

	}, warning = function(ex) { 
		message(paste0("There are some issues happened for variable: ", y, "; model: ", mdl))
		message("Here's the original warning message:")
		message(ex)
		cat("\n")
	})

}

#output results and residuals
write.csv(modeloutput,paste0("modeloutput.csv"))
write.csv(residuals,paste0("residuals1.csv"))
write.csv(residualsridge,paste0("residuals1ridge.csv"))
write.csv(residualslasso,paste0("residuals1lasso.csv"))
write.csv(residualsgbm,paste0("residuals1gbm.csv"))

# Calculate numbers in Table 7 in the report
modeloutput$RMSE <- as.numeric(modeloutput$RMSE)
modeloutput$RMSE_train <- as.numeric(modeloutput$RMSE_train)
modeloutput$R2 <- as.numeric(modeloutput$R2)
modeloutput$R2_train <- as.numeric(modeloutput$R2_train)
modeloutput_agg <- aggregate(modeloutput[,colnames(modeloutput) %in% c("RMSE_train", "R2_train", "RMSE", "R2")], by=list(modeloutput$Model), FUN = mean, na.rm=TRUE)
write.csv(modeloutput_agg,paste0("modeloutput_agg.csv"))


################################################################
# Lasso on All Data ############################################
################################################################
#After we chose Lasso as our best model, we apply it to all data to capture more information
#This will be used for the simulation

#define output structure as before
modeloutput <- data.frame(y=character(),
				 RMSE_train=double(),
                 R2_train=double(),
                 R2Adjust_train=double(),
				 RMSE=double(),
                 R2=double(),
                 R2Adjust=double(),
				 df=integer(),
				 fvar=double(),
				 sfvar=double(),
				 rvar=double(),
				 srvar=double(),
				 corr=double(),
				 scorr=double(),
                 Models=character(),
                 stringsAsFactors=FALSE)


#set lag, residuals data structure
lag <-2
residualslasso <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))
colnames(residualslasso) <- Ynames

#run Lasso model. It will generate a csv file (lasso_y.csv) that store the calibrated model for each y.
#The mapping.csv contains the values of all calibrated Lasso models.

for (y in Ynames) {
	Traindata <- rawdata[,names(rawdata) %in% c(y,Xnames,"extreme_ind")]
	Xnames1 <- names(Traindata)[!names(Traindata) %in% c("extreme_ind")] #get a list of variables to generate lagged variables
	
	#generate lagged variables
	if (lag > 0) {
		for (i in c(1:lag)){
			for (varname in Xnames1){
				Traindata[(i+1):nrow(Traindata),paste0(varname,i)] <- Traindata[1:(nrow(Traindata)-i),varname]
				Traindata[1:i,paste0(varname,i)] <- NA
			}
		}
	}
	
	#remove data records that have NA. If the lag equals 2, the first two records will be removed as x(-1) and x(-2) have no values
	Traindata <- na.omit(Traindata[(lag+1):nrow(Traindata),])
	
	Xnames2 <- names(Traindata)[!names(Traindata) %in% c(y,"extreme_ind")]
	
	#generate the formula used for calibration
	f <-as.formula(paste(y,"~",paste(Xnames2,collapse="+")))
	print(f)

	training <- Traindata
	validation <- Traindata

	withCallingHandlers({
		#lasso regression
		mdl <- "lasso"
		lasso<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 1)
		cv.out <- cv.glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 1)
		lambda <- cv.out$lambda.min
		print(paste0(y," lasso regression: lambda = ", lambda))
		lasso<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[,y]), alpha = 1, lambda = lambda)
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(training[Xnames2]))
		rmse_train<-sqrt(mean((training[,y]-lassopredict)^2))
		r2_train<-1-sum((training[,y]-lassopredict)^2)/sum((training[,y]-mean(training[,y]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(validation[Xnames2]))
		rmse<-sqrt(mean((validation[,y]-lassopredict)^2))
		r2<-1-sum((validation[,y]-lassopredict)^2)/sum((validation[,y]-mean(validation[,y]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(Traindata[Xnames2]))
		fvar <- var(lassopredict)
		sfvar <- var(lassopredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-lassopredict)
		srvar <- var((Traindata[,y]-lassopredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-lassopredict,lassopredict)
		scorr<-cor((Traindata[,y]-lassopredict)[Traindata$extreme_ind=="Y"],lassopredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "lasso")
		write.csv(rbind(as.matrix(lasso$a0), as.matrix(lasso$beta)),paste0("lasso_",y,".csv"))
		residual_lasso <- Traindata[,y]-lassopredict
		residualslasso[,y]<-c(rep(NA,nrow(residualslasso)-length(residual_lasso)),residual_lasso)
	}, warning = function(ex) { 
		message(paste0("There are some issues happened for variable: ", y, "; model: ", mdl))
		message("Here's the original warning message:")
		message(ex)
		cat("\n")
	})
}

write.csv(modeloutput,paste0("modeloutput_lasso.csv")) 
#this file contains the "fvar sfvar rvar srvar corr	scorr" used in mapping_lasso.csv
# fvar:		total variance
# sfvar:	residual variance
# rvar:		pandemic variance
# srvar:	residual pandemic variance
# corr:		correlation coefficient
# scorr:	correlation coefficient during pandemic
write.csv(residualslasso,paste0("residualslasso_all.csv"))

################################################################
# Correlation and Cholesky Decomposition #######################
################################################################

#loop to repair correlation matrix for non-positive definite
repairall <- function(C){

	tryCatch(
	{
		chol(C)
		sa<-1
		return (sa)
	},
		error = function(ex) {
		sa<-0
		return (sa)
		}
	)
}	

#repair correlation matrix for non-positive definite
repaircorr<-function(C){
	# compute eigenvectors/-values
	E   <- eigen(C, symmetric = TRUE)   
	V   <- E$vectors
	D   <- E$values

	# replace negative eigenvalues by 0.001
	D   <- pmax(D,0.001)

	# reconstruct correlation matrix
	BB  <- V %*% diag(D) %*% t(V)

	# rescale correlation matrix
	T   <- 1/sqrt(diag(BB))
	TT  <- outer(T,T)
	C   <- BB * TT
	return (C)
}


#correlation matrix for expansion periods
um<-"pairwise.complete.obs" #"pairwise.complete.obs" "complete.obs" "all.obs" "na.or.complete"
alldata <- read.csv("residualslasso_all.csv", header=TRUE, sep=",", dec=".")
alldata$extreme_ind <- rawdata$extreme_ind[3:length(rawdata$extreme_ind)] #lag = 2
cordata <- alldata[alldata$extreme_ind == "N",]
cordata <- alldata[,!names(alldata) %in% c("extreme_ind", "X")]
cordata <- cordata[complete.cases(cordata),]
normalcorr <- cor(cordata, use = um, method = "pearson")
normalcorr[is.na(normalcorr)]<-0
write.csv(normalcorr, "cmv_normal.csv")
icount <-0
while (repairall(normalcorr)==0) {
	normalcorr <- repaircorr(normalcorr)
	icount <- icount+1
	# print(icount)
}
normalchol <- chol(normalcorr) #Cholesky decomposition of capital market variable residuals during normal periods
write.csv(normalchol,"normalchol.csv", row.names=FALSE)

#correlation matrix for extreme periods
cordata <- alldata[alldata$extreme_ind == "Y",]
cordata <- cordata[,!names(cordata) %in% c("extreme_ind","X")]
extcorr <- cor(cordata, use = um, method = "pearson")
extcorr[is.na(extcorr)]<-0
write.csv(extcorr, "cmv_extreme.csv")
icount <-0
while (repairall(extcorr)==0) {
	extcorr <- repaircorr(extcorr)
	icount <- icount+1
	# print(icount)
}
extchol <- chol(extcorr) #Cholesky decomposition of capital market variable residuals during extreme periods
write.csv(extchol,"extchol.csv", row.names=FALSE)


############################################################################
# This is the ESG in Section 5.1 and 5.2
###########################################################################

# Check if required packages have been installed. If not, they will be
# installed automatically.
packages <- c("foreach","doParallel")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(foreach)
library(doParallel)

# Set if you want to use parallel computing
# Note that you may not be able to reproduce simulated results using
# parallel computing
parallel_compute <- TRUE

#########################################################################################
# Simulated economic factors and capital market variables based on VAR and Lasso models #
# Step 1 - Step 3 #######################################################################
#########################################################################################

start_time <- Sys.time()

## Inputs

# freq
freq_size <- 2.7
freq_p <- 0.5
freq_u <- 7
freq_sigma_u <- 4.03
freq_xi <- -0.16 
freq_mu <- freq_size*(1-freq_p)/freq_p
freq_u_perc <- pnbinom(freq_u,freq_size,freq_p)

# duration
dur_mu <- 0.68
dur_sigma <- 1.05
dur_u <- 7
dur_sigma_u <- 14.86
dur_xi <- -0.27
dur_u_perc <- plnorm(dur_u,dur_mu,dur_sigma)

# case fatality rate
cfr_alpha <- 0.49
cfr_beta <- 1.3

# infection rate
ir_alpha <- 0.55
ir_beta <- 1.32

# duraton and severity correlation matrix
ds_corr <- as.data.frame(matrix(c(1.00,0.06,-0.23,0.06,1.00,0.17,-0.23,0.17,1.00),nrow=3, ncol=3))
colnames(ds_corr) = c("duration","cfr","ir")
ds_chol <- chol(ds_corr)

# age group
ag_probs <- c(0.32, 0.3, 0.14, 0.11, 0.13) # infant, children, young adult, mid aged adult, old aged

# pandemic criteria
pandemic_prob <- 0.037
death_threshold <- 800000
infection_threshold <- 12000000
wp <- 7.8e9 #world population

#esg function: create one single scenario for insurance risk (frequency, duration, severity)
esg_ins <- function(freq_size, freq_p, freq_u, freq_sigma_u, freq_xi, 
					dur_mu, dur_sigma, dur_u, dur_sigma_u, dur_xi, 
					cfr_alpha, cfr_beta,
					ir_alpha, ir_beta,
					ds_corr, ag_probs,
					period, sim){
	set.seed(123+sim)
	
	#define output structure as before
	ins_output <- data.frame(sim=double(),
					 period=double(),
					 duration=double(),
					 cfr=double(),
					 ir=double(),
					 infant_death_portion=double(),
					 children_death_portion=double(),
					 young_adult_death_portion=double(),
					 mid_aged_death_portion=double(),
					 old_aged_death_portion=double(),
					 pandemic=integer(),
					 extreme_ind=character(),
					 stringsAsFactors=FALSE)

	row_idx <- 1
	for (i in c(1:period)){
		#simulate frequency
		y_events <- rnbinom(1,freq_size, freq_p)
		if (y_events >= freq_u){
			y_events <- rgpd(1, xi=freq_xi,beta=freq_sigma_u)+freq_u
		}
		
		n_events <- rmultinom(1,y_events,c(0.25,0.25,0.25,0.25))
		
		if (n_events > 0) {
			for (e in c(1:n_events)){
				rnds <- t(data.matrix(ds_chol)) %*% (runif(3))
				
				rnds <- pmin(1,pmax(rnds,0))
				if (rnds[1] <= dur_u_perc){
					duration <- qlnorm(rnds[1],meanlog=dur_mu,sdlog=dur_sigma)
				} else {
					duration <- qgpd((rnds[1]-dur_u_perc)/(1-dur_u_perc),xi=dur_xi,beta=dur_sigma_u)+dur_u
				}
				
				cfr <-qbeta(rnds[2],cfr_alpha,cfr_beta)
				ir <- qbeta(rnds[3],ir_alpha,ir_beta)
				
				ag_death <- rmultinom(1, 10000, ag_probs)/10000
				
				pandemic <- 0
				extreme_ind <- "N"
				ins_output[row_idx,]<-c(sim,i,duration,cfr,ir,as.vector(ag_death),pandemic,extreme_ind)
				row_idx <- row_idx+1
			}
		}
	}
	return(ins_output)
}

# set.seed(123)
# scen <- esg_ins(freq_size, freq_p, freq_u, freq_sigma_u, freq_xi, 
					# dur_mu, dur_sigma, dur_u, dur_sigma_u, dur_xi, 
					# cfr_alpha, cfr_beta,
					# ir_alpha, ir_beta,
					# ds_corr, ag_probs,
					# 4, 1)

#generate insurance risk scenarios
set.seed(123)
nscns <- 1000
nperiod <- 100 #in quarters

if (parallel_compute){
	# Parallel simulation using multiple cores. However, results are not reproducible
	cl <- parallel::makeCluster(4) #set number of cores to use
	doParallel::registerDoParallel(cl)
	simulations_ins <- foreach(i = 1:nscns, .combine = 'rbind') %dopar% {
		library(fExtremes)
		esg_ins(freq_size, freq_p, freq_u, freq_sigma_u, freq_xi, 
					dur_mu, dur_sigma, dur_u, dur_sigma_u, dur_xi, 
					cfr_alpha, cfr_beta,
					ir_alpha, ir_beta,
					ds_corr, ag_probs,
					nperiod, i)}
	parallel::stopCluster(cl)
} else {
	# This is not using parallel but results are reproducible. It is also used when generating the results.
	set.seed(123)
	for (i in c(1:nscns)){
		isim <- i %% nscns
		if (isim==0) {isim=nscns}
		if (i == 1){
			simulations_ins <- esg_ins(freq_size, freq_p, freq_u, freq_sigma_u, freq_xi, 
					dur_mu, dur_sigma, dur_u, dur_sigma_u, dur_xi, 
					cfr_alpha, cfr_beta,
					ir_alpha, ir_beta,
					ds_corr, ag_probs,
					nperiod, 1)
		} else {
			one_scn <- esg_ins(freq_size, freq_p, freq_u, freq_sigma_u, freq_xi, 
						dur_mu, dur_sigma, dur_u, dur_sigma_u, dur_xi, 
						cfr_alpha, cfr_beta,
						ir_alpha, ir_beta,
						ds_corr, ag_probs,
						nperiod, isim)
			simulations_ins <- rbind(simulations_ins, one_scn)
		}
		if (i %% 10 == 0){print(paste0(i," simulations are done."))}
	}
}

for (name in colnames(simulations_ins)){
# print(name)
	if (name != "extreme_ind"){
		simulations_ins[,name] <- as.numeric(simulations_ins[,name])
	}
}

#Randomly choosing pandemic events based on probability 3.7% and possible global mortality rates
set.seed(123)
n_events <- nrow(simulations_ins)
n_pandemics <- rbinom(1, n_events,.037) #3.7% probabilty

# The split among the three categories are based on historical experience of global pandemic events
n_pandemics_cat_1 <- rbinom(1, n_pandemics,.8) #global mortality rate <0.0005
n_pandemics_cat_2 <- rbinom(1, n_pandemics,.17) #global mortality rate belongs to [0.0005, 0.01)
n_pandemics_cat_3 <- n_pandemics - n_pandemics_cat_1 - n_pandemics_cat_2 #global mortality rate belongs to [0.0005, 0.12].

glb_mort_rate = simulations_ins$cfr * simulations_ins$ir
n_cat_1 = sum()
cat_1_ind <- glb_mort_rate < 0.0005
cat_2_ind <- (glb_mort_rate >= 0.0005) & (glb_mort_rate < 0.01)
cat_3_ind <- (glb_mort_rate >= 0.01) & (glb_mort_rate <= 0.12)

cat_1_rows <- as.numeric(rownames(simulations_ins)[cat_1_ind])
simulations_ins$pandemic[sample(cat_1_rows,n_pandemics_cat_1)] <- 1 
cat_2_rows <- as.numeric(rownames(simulations_ins)[cat_2_ind])
simulations_ins$pandemic[sample(cat_2_rows,n_pandemics_cat_2)] <- 1 
cat_3_rows <- as.numeric(rownames(simulations_ins)[cat_3_ind])
simulations_ins$pandemic[sample(cat_3_rows,n_pandemics_cat_3)] <- 1 

infection <- wp*simulations_ins$ir
death <- infection*simulations_ins$cfr
simulations_ins[(death >= death_threshold | infection >= infection_threshold) & simulations_ins$pandemic == 1,]$extreme_ind <- "Y"

for (name in colnames(simulations_ins)){
# print(name)
	if (name != "extreme_ind"){
		simulations_ins[,name] <- as.numeric(simulations_ins[,name])
	}
}

simulations_ins <- simulations_ins[order(simulations_ins$sim, simulations_ins$period),]

write.csv(simulations_ins,"esg_ins.csv",row.names=FALSE)

apply(simulations_ins[,!names(simulations_ins) %in% c("extreme_ind")],2,mean)
apply(simulations_ins[,!names(simulations_ins) %in% c("extreme_ind")],2,sd)

#Below are just a helper function to output percentiles by period
percs <- c(0.005,0.01,0.05, 0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.995)

perc <- function(scns, ynames, percs, start_period, end_period, ext_ind = "All"){
	modeloutput <- data.frame(y=character(),
				 period = double(),
				 min_=double(),
				 perc_005 = double(), 
				 perc_01 = double(), 
				 perc_05 = double(), 
				 perc_1 = double(), 
				 perc_25 = double(), 
				 perc_50 = double(), 
				 perc_75 = double(), 
				 perc_90 = double(), 
				 perc_95 = double(), 
				 perc_99 = double(), 
				 perc_995 = double(), 
				 max_=double(),
                 stringsAsFactors=FALSE)
		
	counter = 1
	
	if (ext_ind != "All") {
		scns <- scns[which(scns$extreme_ind == ext_ind),]
	}
	
	for (yname in ynames){
		for (p in c(start_period:end_period)){
			idata <- scns[which(scns$period == p),]
			idata <- idata[,yname]
			min_ <- min(idata)
			percs_val <- quantile(idata, percs)
			max_ <- max(idata)
			modeloutput[counter,] = c(yname, p, min_, percs_val, max_)
			counter = counter + 1
		}
	
	}
	
	return(modeloutput)

}

#perc(simulations_ins,c("cfr"),percs,1,100, ext_ind="All")

# summarize extreme_ind per sim per period
summarize_extreme_ind <- function(simulations_ins,nperiod,sim){
	extreme_ind_output <- data.frame(sim=double(),
				 period=double(),
				 extreme_ind=character(),
				 no_of_events=integer(),
				 new_extreme_events=integer(),
				 stringsAsFactors=FALSE)

	p <- 1
	while (p<=nperiod){
		data_sim_period <- with(simulations_ins, simulations_ins[ (sim==i) & (period==p), ])
		
		if (nrow(data_sim_period) <= 0) {
			extreme_ind_output[p,] = c(i, p, "N", 0, 0)
			p <- p+1
		} else if ("Y" %in% unique(data_sim_period$extreme_ind)){
			max_dur <- as.integer(max(with(data_sim_period,data_sim_period[extreme_ind=="Y",])$duration)*4)
			for (d in c(1:max_dur)){
				if (p<=nperiod){
					new_data_sim_period <- with(simulations_ins, simulations_ins[ (sim==i) & (period==p), ])
					extreme_ind_output[p,] = c(i, p, "Y", nrow(new_data_sim_period), nrow(with(new_data_sim_period,new_data_sim_period[extreme_ind=="Y",])))
					p <- p+1
				} else {
					break
				}
			}
		} else {
			extreme_ind_output[p,] = c(i, p, "N", nrow(data_sim_period), nrow(with(data_sim_period,data_sim_period[extreme_ind=="Y",])))
			p <- p+1
		}
	}
	
	return(extreme_ind_output)
}

if (parallel_compute){
	# Parallel simulation using multiple cores. However, results are not reproducible
	cl <- parallel::makeCluster(4) #set number of cores to use
	doParallel::registerDoParallel(cl)
	simulations_extreme_inds <- foreach(i = 1:nscns, .combine = 'rbind') %dopar% {
		summarize_extreme_ind(simulations_ins,nperiod,i)}
	parallel::stopCluster(cl)
} else {
	# This is not using parallel but results are reproducible. It is also used when generating the results.
	set.seed(123)
	for (i in c(1:nscns)){
		isim <- i %% nscns
		if (isim==0) {isim=nscns}
		if (i == 1){
			simulations_extreme_inds <- summarize_extreme_ind(simulations_ins,nperiod,1)
		} else {
			one_scn <- summarize_extreme_ind(simulations_ins,nperiod,isim)
			simulations_extreme_inds <- rbind(simulations_extreme_inds, one_scn)
		}
		if (i %% 10 == 0){print(paste0(i," simulations have been summarized."))}
	}
}

for (name in colnames(simulations_extreme_inds)){
# print(name)
	if (name != "extreme_ind"){
		simulations_extreme_inds[,name] <- as.numeric(simulations_extreme_inds[,name])
	}
}

write.csv(simulations_extreme_inds,"simulations_extreme_inds.csv",row.names=FALSE)

#########################################################################################
# Simulated economic factors and capital market variables based on VAR and Lasso models #
# Step 4 & Step 5 #######################################################################
#########################################################################################

# read ESG inputs
var1 <- read.csv("esg_input/var1.csv")
econchol <- read.csv("esg_input/econchol.csv")
mapping <- read.csv("esg_input/mapping_lasso.csv")
mappingNames <- colnames(mapping)[2:(ncol(mapping)-7)]
normalchol <- read.csv("esg_input/normalchol.csv")
extchol <- read.csv("esg_input/extchol.csv")
inputmap <- read.csv("econ_data_quarterly.csv", header=TRUE, sep=",", dec=".")
inputmap <- inputmap[, !colnames(inputmap) %in% c("gdppc_gr")]
Xnames <- c("gdp_gr","inflation","ur","Fed_rate","pce","fpi","ge")
Ynames <- names(inputmap)[!names(inputmap) %in% c(Xnames,"observation_date","extreme_ind")]
# Ynames <- c("tby_1yr", "tby_10yr", "Aaa_cs", "Baa_cs", "BBB_default", "sp500_rtn", "sp500_divd", "mhp_gr", "rent_gr")

#get two period historical economic factors (we are using lag = 2)
histEF <- inputmap[,names(inputmap) %in% Xnames]
histEF <- histEF[(nrow(histEF)-1):nrow(histEF),]
histEF <- histEF[, Xnames]

#get two period historical capital market variables (we are using lag = 2)
histCMV <- inputmap[,names(inputmap) %in% Ynames]
histCMV <- histCMV[(nrow(histCMV)-1):nrow(histCMV),]
histCMV <- histCMV[, Ynames]

#get two period historical pandemic indicator information
histPandemic <- inputmap[(nrow(inputmap)-1):nrow(inputmap),names(inputmap) %in% c("extreme_ind")]

#esg function: create one single scenario for financial risk (economic factors and capital market variables)
esg_fr <- function(var1,mapping,histEF,histCMV,econchol,normalchol,extchol,simulations_extreme_inds,period,sim){
	set.seed(123+sim)
	sim_ef <- histEF
	
	x_ef <- histEF[2,]
	y_ef <- histEF[2,]
	cols <- colnames(y_ef)
	sdef <- var1[,names(var1) %in% c("sd")]

	for (i in c(1:period)){
		rnds <- t(data.matrix(econchol)) %*% (sdef * rnorm(length(sdef)))
		y_ef <- t(data.matrix(var1[,!names(var1) %in% c("X","const","sd")]) %*% t(x_ef) + var1$const + rnds)
		colnames(y_ef) <- cols
		x_ef = y_ef
		sim_ef <- rbind(sim_ef, y_ef) 
	}
	rownames(sim_ef) <- NULL

	mappingX <- sim_ef
	
	s<-sim
	extreme_ind_sim <- with(simulations_extreme_inds, simulations_extreme_inds[ (sim==s), ])
	sim_ef$extreme_ind <- "N"
	sim_ef$extreme_ind[1:2] <- histPandemic
	sim_ef$extreme_ind[3:(period+2)] <- extreme_ind_sim$extreme_ind[1:period]
	
	sim_ef$period <- 0
	sim_ef$period[1] <- -1
	sim_ef$period[3:nrow(sim_ef)]=seq(1,period)
	
	# sim_ef$ur <- ifelse(sim_ef$ur>0,sim_ef$ur,0) #set floor 0 for unemployment rate
	
	sim_mf <- cbind(sim_ef,histCMV,row.names = NULL)
	
	sdnormal <- sqrt(mapping[,names(mapping) %in% c("fvar")])
	sdpandemic <- sqrt(mapping[,names(mapping) %in% c("sfvar")])
	sdinormal <- sqrt(mapping[,names(mapping) %in% c("rvar")])
	sdipandemic <- sqrt(mapping[,names(mapping) %in% c("srvar")])
	corrnormal <- mapping[,names(mapping) %in% c("corr")]
	corrpandemic <- mapping[,names(mapping) %in% c("scorr")]
	stable_value <- mapping[,names(mapping) %in% c("stable_val")]

	lag <-2
	if (lag > 0) {
		for (i in c(1:lag)){
			for (varname in Xnames){
				mappingX[(i+1):nrow(mappingX),paste0(varname,i)] <- mappingX[1:(nrow(mappingX)-i),varname]
				mappingX[1:i,paste0(varname,i)] <- NA
			}
		}
	}
	mappingX <- mappingX[, colnames(mapping)[5:(ncol(mapping)-7)]]
	
	for (i in c(1:period)){
		mappingX_i <- mappingX[i+2,]
		mappingX_i <- data.frame(c(1,0,0,mappingX_i))
		colnames(mappingX_i) <- mappingNames

		mappingX_i_m <- as.data.frame(lapply(mappingX_i, rep, length(sdinormal)))
		mappingX_i_m$x2 <- as.numeric(sim_mf[i,c(10:ncol(sim_mf))])
		mappingX_i_m$x1 <- as.numeric(sim_mf[i+1,c(10:ncol(sim_mf))])
		
		mappingfunc <- mapping[,2:(ncol(mapping)-7)]
		det_returns = rowSums(mappingfunc * mappingX_i_m)

		if (sim_mf$extreme_ind[i+2]=="N"){
			rnds <- t(data.matrix(normalchol)) %*% (sdinormal * rnorm(length(sdinormal)))
			rnds <- as.numeric(rnds)
			#print(det_returns, rnds)
		}else{
			rnds <- t(data.matrix(extchol)) %*% (sdipandemic * rnorm(length(sdipandemic)))
			rnds <- (corrpandemic*(det_returns-stable_value) + sqrt(1-corrpandemic*corrpandemic)*rnds) * sdipandemic #stable_value; mappingX_i_m$x1
			rnds <- as.numeric(rnds)/sqrt(corrpandemic*corrpandemic*sdpandemic*sdpandemic + (1-corrpandemic*corrpandemic)*sdipandemic*sdipandemic)
		}
		sim_mf[i+2,c(10:ncol(sim_mf))] <- det_returns + as.numeric(rnds)*1
		# sim_mf$BBB_default[i+2] <- ifelse(sim_mf$BBB_default[i+2]>0,sim_mf$BBB_default[i+2],0) #set floor 0 for default rate
		# sim_mf$sp500_divd[i+2] <- ifelse(sim_mf$sp500_divd[i+2]>0,sim_mf$sp500_divd[i+2],0) #set floor 0 for dividend yield

	}
	
	sim_mf$sim <- sim
	sim_mf <- sim_mf[,c("sim","period","extreme_ind",Xnames,Ynames)]
	sim_mf$ur <- ifelse(sim_mf$ur>0,sim_mf$ur,0) #set floor 0 for unemployment rate
	sim_mf$BBB_default <- ifelse(sim_mf$BBB_default>0,sim_mf$BBB_default,0) #set floor 0 for default rate
	sim_mf$sp500_divd <- ifelse(sim_mf$sp500_divd>0,sim_mf$sp500_divd,0) #set floor 0 for dividend yield
	
	return(sim_mf)
}

# set.seed(123)
# scen <- esg_fr(var1,mapping,histEF,histCMV,econchol,normalchol,extchol,simulations_extreme_inds,100,3)

#generate economic factor and capital market variable scenarios
set.seed(123)

if (parallel_compute){
	# Parallel simulation using multiple cores. However, results are not reproducible
	cl <- parallel::makeCluster(4) #set number of cores to use
	doParallel::registerDoParallel(cl)
	simulations <- foreach(i = 1:nscns, .combine = 'rbind') %dopar% {
		esg_fr(var1,mapping,histEF,histCMV,econchol,normalchol,extchol,simulations_extreme_inds,nperiod,i)}
	parallel::stopCluster(cl)
} else {
	# This is not using parallel but results are reproducible. It is also used when generating the results.
	set.seed(6)
	for (i in c(1:nscns)){
		isim <- i %% nscns
		if (isim==0) {isim=nscns}
		if (i == 1){
			simulations <- esg_fr(var1,mapping,histEF,histCMV,econchol,normalchol,extchol,simulations_extreme_inds,nperiod,1)
		} else {
			one_scn <- esg_fr(var1,mapping,histEF,histCMV,econchol,normalchol,extchol,simulations_extreme_inds,nperiod,isim)
			simulations <- rbind(simulations, one_scn)
		}
		if (i %% 10 == 0){print(paste0(i," simulations are done."))}
	}
}

write.csv(simulations,"esg_fr.csv",row.names=FALSE)

end_time <- Sys.time()

print(paste0("It took ", end_time - start_time, " mins to generate ", nscns, " asset return scenarios of ", nperiod, " quarters."))
#4.39403501749039 mins to generate 1000 scenarios using 4 cores.


#if you  do not use parallel computing, you can replicate numbers in the report
for (name in colnames(simulations)){
# print(name)
	if (name != "extreme_ind"){
		simulations[,name] <- as.numeric(simulations[,name])
	}
}
apply(simulations[,!names(simulations) %in% c("extreme_ind")],2,mean)
apply(simulations[,!names(simulations) %in% c("extreme_ind")],2,sd)

#perc(simulations,c("tby_10yr"),percs,1,20)

# Percentile plots in Section 5.2
percentile_plot <- function(simulations, variable, start_period, end_period, ylimit, ext_ind = "All"){
	percs <- perc(simulations,c(variable),c(0.005,0.01,0.05, 0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.995),start_period,end_period, ext_ind = ext_ind)
	xdata <- percs$period
	median <- percs$perc_50
	percentile_05th <- percs$perc_005
	percentile_995th <- percs$perc_995
	percentile_95th <- percs$perc_95
	percentile_5th <- percs$perc_05
	percentile_95th <- percs$perc_95
	percentile_25th <- percs$perc_25
	percentile_75th <- percs$perc_75
	plot(xdata, median, type="o", col="black", pch=15, lty=1, ylim=ylimit, main=paste0("Percentiles of Simulated ", variable), xlab="Quarter", ylab=variable)
	legend(0.2,ylimit[2],legend=c("median", "25/75th percentile", "5/95th percentile", "0.5/99.5th percentile"), col=c("black", "blue", "red", "yellow"), lty=1:4, pch=15:18, cex=0.8)
	points(xdata, percentile_25th, col="blue",pch=16)
	lines(xdata, percentile_25th, col="blue", lty=2)
	points(xdata, percentile_75th, col="blue",pch=16)
	lines(xdata, percentile_75th, col="blue", lty=2)
	points(xdata, percentile_5th, col="red", pch=17)
	lines(xdata, percentile_5th, col="red",lty=3)
	points(xdata, percentile_95th, col="red", pch=17)
	lines(xdata, percentile_95th, col="red",lty=3)
	points(xdata, percentile_05th, col="yellow", pch=18)
	lines(xdata, percentile_05th, col="yellow",lty=4)
	points(xdata, percentile_995th, col="yellow", pch=18)
	lines(xdata, percentile_995th, col="yellow",lty=4)
}

#percentile plots for insurance risks
percentile_plot(simulations_ins, "duration", 1, 100, c(min(simulations_ins$duration),max(simulations_ins$duration)))
percentile_plot(simulations_ins, "cfr", 1, 100, c(min(simulations_ins$cfr),max(simulations_ins$cfr)+0.25))
percentile_plot(simulations_ins, "ir", 1, 100, c(min(simulations_ins$ir),max(simulations_ins$ir)+0.25))
percentile_plot(simulations_extreme_inds, "no_of_events", 1, 100, c(min(simulations_extreme_inds$no_of_events),max(simulations_extreme_inds$no_of_events)))

#percentile plots for economic factors
percentile_plot(simulations, "gdp_gr", 0, 100, c(min(simulations$gdp_gr),max(simulations$gdp_gr)))
percentile_plot(simulations, "inflation", 0, 100, c(min(simulations$inflation),max(simulations$inflation)))
percentile_plot(simulations, "pce", 0, 100, c(min(simulations$pce),max(simulations$pce)))
percentile_plot(simulations, "fpi", 0, 100, c(min(simulations$fpi),max(simulations$fpi)))
percentile_plot(simulations, "ge", 0, 100, c(min(simulations$ge),max(simulations$ge)))
percentile_plot(simulations, "Fed_rate", 0, 100, c(min(simulations$Fed_rate),max(simulations$Fed_rate)))
percentile_plot(simulations, "ur", 0, 100, c(min(simulations$ur),max(simulations$ur)))

#percentile plots for capital market variables
percentile_plot(simulations, "tby_1yr", 0, 100, c(min(simulations$tby_1yr),max(simulations$tby_1yr)))
percentile_plot(simulations, "tby_10yr", 1, 100, c(min(simulations$tby_10yr),max(simulations$tby_10yr)))
percentile_plot(simulations, "Aaa_cs", 0, 100, c(min(simulations$Aaa_cs),max(simulations$Aaa_cs)))
percentile_plot(simulations, "Baa_cs", 0, 100, c(min(simulations$Baa_cs),max(simulations$Baa_cs)))
percentile_plot(simulations, "BBB_default", 0, 100, c(min(simulations$BBB_default),max(simulations$BBB_default)))
percentile_plot(simulations, "sp500_rtn", 0, 100, c(min(simulations$sp500_rtn),max(simulations$sp500_rtn)+0.1))
percentile_plot(simulations, "sp500_divd", 0, 100, c(min(simulations$sp500_divd),max(simulations$sp500_divd)))
percentile_plot(simulations, "mhp_gr", 0, 100, c(min(simulations$mhp_gr),max(simulations$mhp_gr)))
percentile_plot(simulations, "rent_gr", 0, 100, c(min(simulations$rent_gr),max(simulations$rent_gr)))
