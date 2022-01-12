install.packages("R2jags")
install.packages("polspline")
install.packages("mcmcplots")
install.packages("ggplot2")

set.seed(1982) 
library(R2jags)
library(polspline)
library(mcmcplots)
library(ggplot2)

################################################################################
########################### Pre-processing #####################################
################################################################################

# ------------- Task characteristics -----------------------------------
groupSize <- 4
ntrials <- 10
pi <- 1.6 # multiplication factor in game
ntokens <- 20
vals <- seq(0,ntokens,1) #possible values to contribute - from 0 to 20 tokens

# ------------- load, define, and prepare data - including covariates ---
setwd("/work/updated_scripts")
rawDat <- read.csv("HerrmannThoeniGaechterDATA.csv", skip = 3) # Public goods game

#- create covariates in raw data matrix
# nation index
rawDat$nation <- c()
rawDat$nation[rawDat$city=="Melbourne"]=1
rawDat$nation[rawDat$city=="Minsk"]=2
rawDat$nation[rawDat$city=="Chengdu"]=3
rawDat$nation[rawDat$city=="Copenhagen"]=4
rawDat$nation[rawDat$city=="Bonn"]=5
rawDat$nation[rawDat$city=="Athens"]=6
rawDat$nation[rawDat$city=="Seoul"]=7
rawDat$nation[rawDat$city=="Samara"]=8
rawDat$nation[rawDat$city=="Zurich"]=9
rawDat$nation[rawDat$city=="St. Gallen"]=9
rawDat$nation[rawDat$city=="Istanbul"]=10
rawDat$nation[rawDat$city=="Nottingham"]=11
rawDat$nation[rawDat$city=="Dnipropetrovs'k"]=12
rawDat$nation[rawDat$city=="Boston"]=13
rawDat$nation[rawDat$city=="Riyadh"]=14
rawDat$nation[rawDat$city=="Muscat"]=15

# Variable for Gini Data from 
# http://hdr.undp.org/sites/default/files/reports/269/hdr_2009_en_complete.pdf,
rawDat$gini <- c()
rawDat$gini[rawDat$city=="Melbourne"]=34.3
rawDat$gini[rawDat$city=="Minsk"]=25.3
rawDat$gini[rawDat$city=="Chengdu"]=38.5
rawDat$gini[rawDat$city=="Copenhagen"]=28.7
rawDat$gini[rawDat$city=="Bonn"]=31.9
rawDat$gini[rawDat$city=="Athens"]=34.4
rawDat$gini[rawDat$city=="Seoul"]=31.6
rawDat$gini[rawDat$city=="Samara"]=37.5
rawDat$gini[rawDat$city=="Zurich"]=32.7
rawDat$gini[rawDat$city=="St. Gallen"]=32.7
rawDat$gini[rawDat$city=="Istanbul"]=41.9
rawDat$gini[rawDat$city=="Nottingham"]=34.8
rawDat$gini[rawDat$city=="Dnipropetrovs'k"]=26.1
rawDat$gini[rawDat$city=="Boston"]=41.1

## GGGI
gggi <- read.csv("GGGI_2008.csv",sep=";")
rawDat$gggi <- c()
rawDat$gggi[rawDat$city=="Melbourne"]=gggi$GGGI_2008[gggi$Country=="Australia"]
rawDat$gggi[rawDat$city=="Minsk"]=gggi$GGGI_2008[gggi$Country=="Belarus"]
rawDat$gggi[rawDat$city=="Chengdu"]=gggi$GGGI_2008[gggi$Country=="China"]
rawDat$gggi[rawDat$city=="Copenhagen"]=gggi$GGGI_2008[gggi$Country=="Denmark"]
rawDat$gggi[rawDat$city=="Bonn"]=gggi$GGGI_2008[gggi$Country=="Germany"]
rawDat$gggi[rawDat$city=="Athens"]=gggi$GGGI_2008[gggi$Country=="Greece"]
rawDat$gggi[rawDat$city=="Seoul"]=gggi$GGGI_2008[gggi$Country=="Korea (Republic of)"]
rawDat$gggi[rawDat$city=="Samara"]=gggi$GGGI_2008[gggi$Country=="Russian Federation"]
rawDat$gggi[rawDat$city=="Zurich"]=gggi$GGGI_2008[gggi$Country=="Switzerland"]
rawDat$gggi[rawDat$city=="St. Gallen"]=gggi$GGGI_2008[gggi$Country=="Switzerland"]
rawDat$gggi[rawDat$city=="Istanbul"]=gggi$GGGI_2008[gggi$Country=="Turkey"]
rawDat$gggi[rawDat$city=="Nottingham"]=gggi$GGGI_2008[gggi$Country=="United Kingdom"]
rawDat$gggi[rawDat$city=="Dnipropetrovs'k"]=gggi$GGGI_2008[gggi$Country=="Ukraine"]
rawDat$gggi[rawDat$city=="Boston"]=gggi$GGGI_2008[gggi$Country=="United States"]
rawDat$gggi[rawDat$city=="Riyadh"]=gggi$GGGI_2008[gggi$Country=="Saudi Arabia"]
rawDat$gggi[rawDat$city=="Muscat"]=gggi$GGGI_2008[gggi$Country=="Oman"]

# Variable for individualism. Data from
# https://www.hofstede-insights.com/product/compare-countries/
# Reference: Hofstede, Hofstede, & Minkov, 2010
# Used instead of data from paper - includes all nations, and closer in time
rawDat$ind <- c()
rawDat$ind[rawDat$city=="Melbourne"]=90
rawDat$ind[rawDat$city=="Minsk"]=25 # not in paper - added to 2010 edition?
rawDat$ind[rawDat$city=="Chengdu"]=20
rawDat$ind[rawDat$city=="Copenhagen"]=74
rawDat$ind[rawDat$city=="Bonn"]=67
rawDat$ind[rawDat$city=="Athens"]=35
rawDat$ind[rawDat$city=="Seoul"]=18
rawDat$ind[rawDat$city=="Samara"]=25 #updated in 2010 edition - paper says 39
rawDat$ind[rawDat$city=="Zurich"]=68
rawDat$ind[rawDat$city=="St. Gallen"]=68
rawDat$ind[rawDat$city=="Istanbul"]=37
rawDat$ind[rawDat$city=="Nottingham"]=89
rawDat$ind[rawDat$city=="Dnipropetrovs'k"]=25 # not in paper - added to 2010 edition?
rawDat$ind[rawDat$city=="Boston"]=91

# Variable for per capita GDP. Included data from paper. In thousands
# 2000 to 2009 data from IMF estimates included in comments)
# https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)#IMF_estimates_from_2000_to_2009
rawDat$gdp <- c()
rawDat$gdp[rawDat$city=="Melbourne"]=32.9 #40822
rawDat$gdp[rawDat$city=="Minsk"]=8.9 #14051
rawDat$gdp[rawDat$city=="Chengdu"]=7.6 #8215
rawDat$gdp[rawDat$city=="Copenhagen"]=36.5 #42096
rawDat$gdp[rawDat$city=="Bonn"]=31.1 #37949
rawDat$gdp[rawDat$city=="Athens"]=26.0 #29312
rawDat$gdp[rawDat$city=="Seoul"]=23.9 #28820
rawDat$gdp[rawDat$city=="Samara"]=12.1 #20123
rawDat$gdp[rawDat$city=="Zurich"]=37.4 #54812
rawDat$gdp[rawDat$city=="St. Gallen"]=37.4 #54812
rawDat$gdp[rawDat$city=="Istanbul"]=9.1 #16093
rawDat$gdp[rawDat$city=="Nottingham"]=35.1 #35372
rawDat$gdp[rawDat$city=="Dnipropetrovs'k"]=7.6 #7845
rawDat$gdp[rawDat$city=="Boston"]=43.4 #47008 

# Variable for trust/social cohesion. Data from paper
# Also available from world value and european values surveys (commented out)
# https://ourworldindata.org/trust?utm_source=Our+World+in+Data+%E2%80%93+Newsletter&utm_campaign=ab0d6124e2-Newsletter_OurWorldInData_No4_Trust&utm_medium=email&utm_term=0_2e166c1fc1-ab0d6124e2-284219045
# values from 2009 - also csv saved in data folder. Source is wave five survey 
# Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014. 
# World Values Survey: Round Five - Country-Pooled Datafile Version:
# Values for Denmark and Greece from Halman, L. 2001. The European Values Study: A Third Wave. Tilburg: EVS, WORC, Tilburg
# University. Quoted here: https://www.oecd.org/innovation/research/2381883.pdf
# Data from the EVS and WVS are stable and comparable for countries that are included on both
# https://www.researchgate.net/publication/287200653_Trust_in_institutions_and_human_values_in_the_European_context_A_comparison_between_the_World_Value_Survey_and_the_European_Social_Survey
rawDat$trust <- c()
rawDat$trust[rawDat$city=="Melbourne"]=.40 #.48 #2009
rawDat$trust[rawDat$city=="Minsk"]= .42 #.41 #2008-2010 EVS - european values survey (only 2014 value of .33 available for WVS)
rawDat$trust[rawDat$city=="Chengdu"]= .55 #.49 #2009
rawDat$trust[rawDat$city=="Copenhagen"]= .67 #.75 #2008-2010 EVS - european values survey
rawDat$trust[rawDat$city=="Bonn"]=.38 #.32 #2009
rawDat$trust[rawDat$city=="Athens"]=.24 #.19 #2008-2010 #EVS - european values survey
rawDat$trust[rawDat$city=="Seoul"]=.27 #.30 #2009
rawDat$trust[rawDat$city=="Samara"]=.24 #.25 #2009
rawDat$trust[rawDat$city=="Zurich"]= .37 #.49 #2009
rawDat$trust[rawDat$city=="St. Gallen"]= .37 #.49 #2009
rawDat$trust[rawDat$city=="Istanbul"]=.16 #.05 #2009
rawDat$trust[rawDat$city=="Nottingham"]= .29 #.30 #2009
rawDat$trust[rawDat$city=="Dnipropetrovs'k"]=.27 #.27 #2009
rawDat$trust[rawDat$city=="Boston"]=.36 #.40 #2009

#---------------------- prepare response dataset -------------------------------

# extract every third line - data file has lines representing others' responses and we don't need that
redDat <- rawDat[seq(1,length(rawDat$sessionid),3),]

group_names <- unique(redDat$groupid)
ngroups <- length(group_names)

# THIS WILL REMOVE SUBJECTS WITH MISSING DATA IN NO PUNISHMENT CONDITION
ngroups <- 269

subject_names <- unique(redDat$subjectid)
nsubjects <- length(subject_names)

#------- data for no punishment condition -----------#
c_no_punish <- array(0,c(groupSize,ntrials,ngroups))
Ga_no_punish <- array(0,c(ntrials,ngroups))
Gc_no_punish <- array(0,c(groupSize,ntrials,ngroups))
missing <- array(0,ngroups)

for (g in 1:ngroups) {
  c_no_punish[,,g] <- rbind(redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][1:10],
                            redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][11:20],
                            redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][21:30],
                            redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="N-experiment"][31:40])
  
  Ga_no_punish[,g] <- colMeans(c_no_punish[,,g])
  
  missing[g] <- is.na(c_no_punish[1,1,g])
  
  for (s in 1:groupSize) {
    Gc_no_punish[,,g] <- colSums(c_no_punish[-s,,g])
    
  }
}

#------- data for punishment condition ---------------#
c_punish <- array(0,c(groupSize,ntrials,ngroups))
Ga_punish <- array(0,c(ntrials,ngroups))
Gc_punish <- array(0,c(groupSize,ntrials,ngroups))

for (g in 1:ngroups) {
  c_punish[,,g] <- rbind(redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][1:10],
                         redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][11:20],
                         redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][21:30],
                         redDat$senderscontribution[redDat$groupid==group_names[g]&redDat$p=="P-experiment"][31:40])
  
  Ga_punish[,g] <- colMeans(c_punish[,,g])
  
  for (s in 1:groupSize) {
    Gc_punish[,,g] <- colSums(c_punish[-s,,g])
  }
}

# compile data from each condition into 4D matrix
c <- array(0,c(groupSize,ntrials,ngroups,2))
c[,,,1] <- c_no_punish
c[,,,2] <- c_punish

Ga <- array(0,c(ntrials,ngroups,2))
Ga[,,1] <- Ga_no_punish
Ga[,,2] <- Ga_punish

Gc <- array(0,c(groupSize,ntrials,ngroups,2))
Gc[,,,1] <- Gc_no_punish
Gc[,,,2] <- Gc_punish

#c_choice_index <- c (not used anywhere)

#---------------------- prepare covariates as group level vectors ---------------
Nation <- array(0,c(ngroups))
civic <- array(0,c(ngroups))
Gggi <- array(0,c(ngroups))
Indiv <- array(0,c(ngroups))
GDP <- array(0,c(ngroups))
Trust <- array(0,c(ngroups))

for (g in 1:ngroups) {
  Nation[g] <- mean(redDat$nation[redDat$groupid==group_names[g]&redDat$p=="P-experiment"])
  civic[g] <- mean(redDat$civic[redDat$groupid==group_names[g]&redDat$p=="P-experiment"])
  Gggi[g] <- mean(redDat$gggi[redDat$groupid==group_names[g]&redDat$p=="P-experiment"])  
  Indiv[g] <- mean(redDat$ind[redDat$groupid==group_names[g]&redDat$p=="P-experiment"])
  GDP[g] <- mean(redDat$gdp[redDat$groupid==group_names[g]&redDat$p=="P-experiment"])  
  Trust[g] <- mean(redDat$trust[redDat$groupid==group_names[g]&redDat$p=="P-experiment"])
  }

#--- remove groups (from nations) without gggi scores 
Ga_punish <- Ga_punish[,!is.na(Gggi)]
Ga_no_punish <- Ga_no_punish[,!is.na(Gggi)]

c <- c[,,!is.na(Gggi),]
Ga <- Ga[,!is.na(Gggi),]
Gc <- Gc[,,!is.na(Gggi),]
Nation <- Nation[!is.na(Nation)]
Indiv <- Indiv[!is.na(Gggi)]
Civic <- civic[!is.na(Gggi)]
GDP <- GDP[!is.na(Gggi)]
Trust <- Trust[!is.na(Gggi)]
Gggi <- Gggi[!is.na(Gggi)]

#redefine number of groups after removal
ngroups <- length(Gggi)

# -- calculate group level winnings --
winnings <- array(0,c(ngroups))
for (g in 1:ngroups) {
  winnings[g] <- sum(colSums(c[,,g,1])*pi)
}

# --- Convert covariates to national level -------------------------------------
# all analyses are hierarchical, individuals assumed to follow national dists
Gggi <- aggregate(Gggi~Nation,FUN=mean)[,2]
GDP <- aggregate(GDP~Nation,FUN=mean)[,2]
Indiv <- aggregate(Indiv~Nation,FUN=mean)[,2]
Trust <- aggregate(Trust~Nation,FUN=mean)[,2]
Civic <- aggregate(Civic~Nation,FUN=mean)[,2]

nnations <- length(Gggi)



################################################################################
########################### Conditional cooperation model ######################
################################################################################

set.seed(1995)

# Bayes factors computed for JZS priors. Method described here
# https://link.springer.com/article/10.3758/s13423-012-0295-x
# Code available here
# https://github.com/MicheleNuijten/BayesMed/blob/master/R/jzs_corSD.R
# Paper where code is used here (mediation paper)
# https://link.springer.com/article/10.3758/s13428-014-0470-2

#-------------------  Regress Gggi on slope of prefs in CC model ---------------
# standardise covariate
X <- Gggi
X <- (X-mean(X))/sd(X) # z-score
invSigma <- solve(t(X)%*%X) # required for JZS priors

data <- list("groupSize", "ngroups", "ntrials", "nnations","c","Ga","X","Nation","invSigma") 
params <- c("beta0_rho","betaX_rho","beta0_omega","betaX_omega") # , "nat_rho_probit", "nat_omega_probit") 

# - run jags code # start of 4 chains = 
CC.samples <- jags(data, inits=NULL, params,
                   model.file ="CC_cor.txt",
                   n.chains=4, n.iter=30000, n.burnin=5000, n.thin=1)

# Plotting diagnostics
mcmcplot(CC.samples, col= c("#F8766D", "#00BFC4","#39B600","#7CAE00"))

# BetaX, slope, rho (preference; readiness to cooperate)
library(ggplot2)
postBetaX_rho <- CC.samples$BUGSoutput$sims.list$betaX_rho
plot(density(postBetaX_rho))

prior <- dnorm(0,1)
fit.posterior <- logspline(CC.samples$BUGSoutput$sims.list$betaX_rho)
posterior <- dlogspline(0, fit.posterior) #
CC.betaX_rho.BF <- prior/posterior

CC.betaX_rho.CIL <- qlogspline(0.025,fit.posterior) #2.5% CI
CC.betaX_rho.HIL <- qlogspline(0.975,fit.posterior) #97.5% CI

postBetaX_rho.df <- data.frame(postBetaX_rho)
ggplot(postBetaX_rho.df,aes(postBetaX_rho)) + 
  geom_density(colour=3,fill=3,alpha=0.05) + 
  geom_segment(aes(x = CC.betaX_rho.CIL, y = 0, xend = CC.betaX_rho.CIL, yend = 1.1),
               colour=3,size=0.1,linetype="dashed") + 
  geom_segment(aes(x = CC.betaX_rho.HIL, y = 0, xend = CC.betaX_rho.HIL, yend = 1.3),
               colour=3,size=0.1,linetype="dashed") + 
  geom_segment(aes(x = mean(postBetaX_rho), 
                   y = 0, xend = mean(postBetaX_rho), yend = 8.2), 
               colour=3,size=0.2)+
  xlim(-0.5,0.5) +
  labs(title = "a) Readiness to Cooperate (slope of ρ)") + 
  xlab("Parameter Estimate") +  ylab("Density") + theme_classic() + 
  theme(text=element_text(size=12,  family="Times"))

# BetaX, slope, omega (attention to others)
prior <- dnorm(0,1)
fit.posterior <- logspline(CC.samples$BUGSoutput$sims.list$betaX_omega)
posterior <- dlogspline(0, fit.posterior) # this 
CC.betaX_omega.BF <- prior/posterior

CC.betaX_omega.CIL <- qlogspline(0.025,fit.posterior) #2.5% CI
CC.betaX_omega.HIL <- qlogspline(0.975,fit.posterior) #97.5% CI

postBetaX_omega <- CC.samples$BUGSoutput$sims.list$betaX_omega
plot(density(postBetaX_omega))

postBetaX_omega.df <- data.frame(postBetaX_omega)
ggplot(postBetaX_omega.df,aes(postBetaX_omega)) + 
  geom_density(colour=3,fill=3,alpha=0.05) + 
  geom_segment(aes(x = CC.betaX_omega.CIL, y = 0, xend = CC.betaX_omega.CIL, yend = 0.49),
               colour=3,size=0.2,linetype="dashed") + 
  geom_segment(aes(x = CC.betaX_omega.HIL, y = 0, xend = CC.betaX_omega.HIL, yend = 0.5),
               colour=3,size=0.2,linetype="dashed") + 
  geom_segment(aes(x = mean(postBetaX_omega), 
                   y = 0, xend = mean(postBetaX_omega), yend = 3.45), 
               colour=3,size=0.2)+
  xlim(-0.5,0.5)+
  labs(title = "b) Attention to Others (slope of ω)") + 
  xlab("Parameter Estimate") +  ylab("Density") + theme_classic() + 
  theme(text=element_text(size=12,  family="Times"))


# #--------------- winnings -------------------------------------
# 
################################################################################
########################### Correlations with winnings #########################
################################################################################

# Bayes factors computed for JZS priors. Method described here
# https://link.springer.com/article/10.3758/s13423-012-0295-x
# Code available here
# https://github.com/MicheleNuijten/BayesMed/blob/master/R/jzs_corSD.R
# Paper where code is used here (mediation paper)
# https://link.springer.com/article/10.3758/s13428-014-0470-2

#-------------------  Regress Gggi on winnings ---------------
# not a function - only called once

# standardise covariate
set.seed(1982)

X <- Gggi
X <- (X-mean(X))/sd(X)

invSigma <- solve(t(X)%*%X) # required for JZS priors

Y <- (winnings-mean(winnings))/sd(winnings)

data <- list("ngroups", "Y", "nnations","X","Nation","invSigma") 
params <- c("beta0","betaX") 

# - run jags code
win.samples <- jags(data, inits=NULL, params,
                    model.file ="win_cor.txt",
                    n.chains=4, n.iter=15000, n.burnin=5000, n.thin=1)
  
# plot winnings: posterior distribution 
prior <- dnorm(0,1)
fit.posterior <- logspline(win.samples$BUGSoutput$sims.list$betaX)
posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
win.BF <- prior/posterior

win.CIL <- qlogspline(0.025,fit.posterior) #2.5% CI
win.HIL <- qlogspline(0.975,fit.posterior) #97.5% CI

win <- win.samples$BUGSoutput$sims.list$betaX
plot(density(win))

win.df <- data.frame(win)
ggplot(win.df,aes(win)) + 
  geom_density(colour=3,fill=3,alpha=0.05) + 
  #lower CI
  geom_segment(aes(x = win.CIL, y = 0, xend = win.CIL, yend = 0.8),
               colour=3,size=0.1,linetype="dashed") + 
  # higher CI
  geom_segment(aes(x = win.HIL, y = 0, xend = win.HIL, yend = 0.8),
               colour=3,size=0.1,linetype="dashed") + 
  # mean
  geom_segment(aes(x = mean(win), 
                   y = 0, xend = mean(win), yend = 6), 
               colour=3,size=0.2)+
  xlim(-0.5,0.5)+
  xlab("Parameter Estimate") +  ylab("Density") + theme_classic() + 
  theme(text=element_text(size=12,  family="Times")) 

# Plotting diagnostics
mcmcplot(win.samples, col= c("#F8766D", "#00BFC4","#39B600","#7CAE00"))

