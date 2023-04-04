####Possible Outcomes for Different Strategies After First Goal####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


#rm(list = ls())
#https://statsbomb.com/2019/02/attacking-contributions-markov-models-for-football/
#par(mfrow=c(1,1))
####Packages####
#if(!require(readr)) install.packages("readr")
#library(readr)
#if(!require(tidyverse)) install.packages("tidyverse")
#library(tidyverse)
#if(!require(devtools)) install.packages("devtools")
#library(devtools)
#if(!require(coefplot)) install.packages("coefplot")
#library(coefplot)
#if(!require(ggplot2)) install.packages("ggplot2")
#library(ggplot2)
#if(!require(installr)) install.packages("installr")
#library(installr)
#if(!require(Rfast)) install.packages("Rfast")
library(Rfast)
#if(!require(stargazer)) install.packages("stargazer")
library(stargazer)
#if(!require(tseries)) install.packages("tseries")
#library(tseries)

#install.packages("ggforce")
#install.packages("ggrepel")
#install.packages("xts")
#library(githubinstall)
#devtools::install_github("statsbomb/StatsBombR")
#devtools::install_github("jogall/soccermatics")
#githubinstall("soccermatics")

#library(useful)
#library(StatsBombR)
#library(soccermatics)
library(dplyr)
#library(knitr)
#library(gridExtra)
#library(grid)
#library(ggplot2)
#library(lattice)
#if(!require(msm)) install.packages("msm")
library(msm)
#library(tidyverse)
#library(lattice)
library(fitdistrplus)
#library(flexsurv)
#library(survival)
#library(mstate)
#library(spgs)
#library(SemiMarkov)
#library(EnvStats)
#library(corrplot)
#library(gmodels)
##Working Directory##
#setwd("C:/Users/Sean Hellingman/Desktop/NHL Draft/Substitutions/AllSubs")

####ACTUAL DATA####
# # # # # # # # # # 

#Actual Matches#
load("StatesRed.RData")


####Reduce the goal difs####
StatesRed$RedGD <- StatesRed$BarcelonaDIFF+StatesRed$GDCorrection


StatesRed$State3GD <- ifelse(!is.na(StatesRed$State), paste0("BDIFF"," ",StatesRed$RedGD," ", StatesRed$State),NA)



StatesRed$State3GDNUM <- as.numeric(as.factor(StatesRed$State3GD))



####Read in Estimated Models####
# # # # # # # # # # # # # # # # 

###Barca subs are second###

load("MCGDSub10.RData")
QStates10 <- QStates00
rm(QStates00)
Q10 <- qmatrix.msm(QStates10) #Extract Intensities [Start]
Q10 <- Q10$estimates/12 #Intensities for 5 second intervals

load("MCGDSub11.RData")
QStates11 <- QStates00
rm(QStates00)
Q11 <- qmatrix.msm(QStates11) #Extract Intensities [Start]
Q11 <- Q11$estimates/12 #Five second

load("MCGDSub12.RData")
QStates12 <- QStates00
rm(QStates00)
Q12 <- qmatrix.msm(QStates12) #Extract Intensities [Start]
Q12 <- Q12$estimates/12 #Five second

load("MCGDSub13.RData")
QStates13 <- QStates00
rm(QStates00)
Q13 <- qmatrix.msm(QStates13) #Extract Intensities [Start]
Q13 <- Q13$estimates/12 #Five second

load("MCGDSub01.RData")
QStates01 <- QStates00
rm(QStates00)
Q01 <- qmatrix.msm(QStates01) #Extract Intensities [Start]
Q01 <- Q01$estimates/12 #Five second

load("MCGDSub02.RData")
QStates02 <- QStates00
rm(QStates00)
Q02 <- qmatrix.msm(QStates02) #Extract Intensities [Start]
Q02 <- Q02$estimates/12 #Five second

load("MCGDSub03.RData")
QStates03 <- QStates00
rm(QStates00)
Q03 <- qmatrix.msm(QStates03) #Extract Intensities [Start]
Q03 <- Q03$estimates/12 #Five second

load("MCGDSub20.RData")
QStates20 <- QStates00
rm(QStates00)
Q20 <- qmatrix.msm(QStates20) #Extract Intensities [Start]
Q20 <- Q20$estimates/12 #Five second

load("MCGDSub21.RData")
QStates21 <- QStates00
rm(QStates00)
Q21 <- qmatrix.msm(QStates21) #Extract Intensities [Start]
Q21 <- Q21$estimates/12 #Five second

load("MCGDSub22.RData")
QStates22 <- QStates00
rm(QStates00)
Q22 <- qmatrix.msm(QStates22) #Extract Intensities [Start]
Q22 <- Q22$estimates/12 #Five second


load("MCGDSub23.RData")
QStates23 <- QStates00
rm(QStates00)
Q23 <- qmatrix.msm(QStates23) #Extract Intensities [Start]
Q23 <- Q23$estimates/12 #Five second

load("MCGDSub30.RData")
QStates30 <- QStates00
rm(QStates00)
Q30 <- qmatrix.msm(QStates30) #Extract Intensities [Start]
Q30 <- Q30$estimates/12 #Five second

load("MCGDSub31.RData")
QStates31 <- QStates00
rm(QStates00)
Q31 <- qmatrix.msm(QStates31) #Extract Intensities [Start]
Q31 <- Q31$estimates/12 #Five second

load("MCGDSub32.RData")
QStates32 <- QStates00
rm(QStates00)
Q32 <- qmatrix.msm(QStates32) #Extract Intensities [Start]
Q32 <- Q32$estimates/12 #Five second

load("MCGDSub33.RData")
QStates33 <- QStates00
rm(QStates00)
Q33 <- qmatrix.msm(QStates33) #Extract Intensities [Start]
Q33 <- Q33$estimates/12 #Five second

load("Q00.RData") #Already in 5 second intervals

#Q00 <- qmatrix.msm(QStates00) #Extract Intensities [Start]
#Q00 <- Q00$estimates/12 #Five second


#####EXAMPLE####
################
#set.seed(1414)

# 3x3 intensity
#A1 <- rbind(c(-7,1,2,3,1),c(3,-8,2,2,1),c(1,1,-7,3,2),c(3,2,2,-8,1),c(1,1,2,3,-7))
#A2 <- rbind(c(-4,1,1,1,1),c(3,-8,2,2,1),c(1,2,-9,4,2),c(3,2,2,-8,1),c(1,2,2,3,-8))
#A3 <- rbind(c(-6,3,3),c(3,-10,7),c(6,3,-9))


# Length of simulations
#set.seed(1415)

#seconds <- 2 #One Step
#games <- 1 #Number of games
#simgdB.df <- data.frame(subject = rep(1:games, rep(seconds,games)), time = rep(seq(1, seconds, 1),games)) 

#A1 Simulations
#SimA1 <- simmulti.msm(simgdB.df, A1, start = 1) #State 1
#SimA1$Stage <- 1
#EndA1 <- SimA1 %>%
#  group_by(subject) %>% 
##  slice_tail()
#simA1B <- simmulti.msm(simgdB.df, A1, start = EndA1$state)



#seconds <- 2 #One Step
#games <- 1 #Number of games (doesn't matter in this example)
#Initiate location for simulation:
#simgdB.df <- data.frame(subject = rep(1:games, rep(seconds,games)), time = rep(seq(1, seconds, 1),games)) 
#S <- 50 #Number of seconds
#strt <- 1 #Starting state of simulation
#Game <- data.frame(matrix(ncol = 4, nrow = 0 )) #Dataframe for results
#x <- c("subject", "time","state","switch") #col names
#colnames(Game) <- x
#rm(x)
#GameEND <- Game

#swtch <- 0 #if the process switched to a different intensity matrix
#Q <- A1 #Starting intensity matrix


#for(j in 1:S){
  
  
  #  SimA1 <- simmulti.msm(simgdB.df, Q, start = strt) #Simulate 1 step
  #  EndA1 <- SimA1 %>%
    #    group_by(subject) %>% 
    #    slice_tail()
  #  strt <- EndA1$state #starting point for next itteration
  
  #  Q <- ifelse(EndA1$state == 5,list(A2),list(A1)) #condition on switching Q
  #  swtch <- ifelse(EndA1$state == 5,swtch + 1,swtch) #number of switches
  #  Q <- as.matrix(Q[[1]])
  
  #  Game[1,] <- c(EndA1$subject,j,EndA1$state,swtch)
  #  
  #  GameEND <- rbind(GameEND,Game) #dataframe with all the results
  #}





####From the Real Games####
###########################

#Simulate until goal is scored# 

set.seed(1235)
#levels(as.factor(StatesRed$State3GD))
k <- 1
seconds <- 2 #One Step
games <- 1 #Number of games 
#Initiate location for simulation:
simgdB.df <- data.frame(subject = rep(1:games, rep(seconds,games)), time = rep(seq(1, seconds, 1),games)) 
S <- 15*12 #Number of minutes
strt <- 15 #Starting state of simulation [15 GD0 Barca Pos, 18 GD0 Opp Pos]
A <- strt #for outcome at substitution
Game <- data.frame(matrix(ncol = 7, nrow = 0 )) #Dataframe for results
x <- c("subject", "time","state","switch","intermediate","Strategy","TimeG1") #col names
colnames(Game) <- x
rm(x)
GameEND <- Game
GamesEND <- Game
G <- 100 #Number of Games
G2 <- 400 #Number of simulated games after each goal

swtch <- 0 #if a goal is scored [Not really used here]

Q <- Q00 #Initial intensity matrix

##TIME UNTIL FIRST GOAL##
#  Qa <- as.matrix(Qa[[1]])
  
  for(i in 1:G){ #The number of games to be simulated
    
    GameEND <- Game 
    strt <- 15 #Reset the starting state
    swtch <- 0 #Reset the switch 
    A <- strt  #Reset starting intensity matrix
    G1 <- NA
    for(j in 1:S){
      
      SimA1 <- simmulti.msm(simgdB.df, Q, start = strt) #Simulate 1 step
      EndA1 <- SimA1 %>%
        group_by(subject) %>% 
        slice_tail()
      strt <- EndA1$state #starting point for next iteration
      
      B <- (swtch == 0 & (EndA1$state < 13 | EndA1$state > 18)) #If true, mark results
      

      A <- ifelse(B,strt,A)
      G1 <- ifelse(B,j,G1) #Goal Time
      #j <- ifelse(B,S,j)
      swtch <- ifelse(B,swtch + 1,swtch) #if goal
    
      
      Game[1,] <- c(i,j,EndA1$state,swtch,A,k,G1) #save results from that step
      
      GameEND <- rbind(GameEND,Game) #dataframe with all the results from the game
      
      if(B){break}
    }
    GamesEND <- rbind(GamesEND,GameEND)
  }
  
  GamesResults  <- GamesEND %>%
    group_by(subject) %>% 
    slice_tail() #Keep only the end of each game
  
  assign(paste("FirstLegResults", "B0O0", sep=""), GamesResults) #Mark the results by the strategy


Result <- function(x) {
  x <- as.numeric(x)
  x <- ifelse(x < 13, "L",ifelse(x > 12 & x < 19, "D","W")) #Function for win loss draw
  
}

FirstLegResultsB0O0$WDL <- Result(FirstLegResultsB0O0$intermediate)

NoGoals <- subset(FirstLegResultsB0O0,FirstLegResultsB0O0$switch == 0)
#save(NoGoals,file = "NoGoals.RData")
Goal <- subset(FirstLegResultsB0O0,FirstLegResultsB0O0$WDL != "D")
#Split by which team scored first
BGoal <- subset(FirstLegResultsB0O0,FirstLegResultsB0O0$WDL == "W")
OGoal <- subset(FirstLegResultsB0O0,FirstLegResultsB0O0$WDL == "L")

#save(FirstLegResultsB0O0,file = "FirstLegResultsB0O0.RData")

##TIME AFTER FIRST GOAL FOR EACH GAME##

#Barca Scores First#
WSB0O0.df <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("subject", "time")
colnames(WSB0O0.df) <- x
rm(x)
id <- length(BGoal$subject)
k=1
for(k in 1:id){
  
  sec <- S - BGoal$TimeG1[k] + 1 #plus 5 seconds
  subject <- as.numeric(BGoal$subject[k])
  
  WsimB0O0 <- data.frame(subject = rep.int(subject,sec), time = rep(seq(1, sec, 1), 1))
  WsimB0O0 <- as.data.frame(WsimB0O0)
  WSB0O0.df <- as.data.frame(WSB0O0.df)
  WSB0O0.df <- rbind(WSB0O0.df,WsimB0O0)
  
}

#Opp Scores First#
LSB0O0.df <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("subject", "time")
colnames(LSB0O0.df) <- x
rm(x)
id <- length(OGoal$subject)
k=1
for(k in 1:id){
  
  sec <- S - OGoal$TimeG1[k] + 1
  subject <- as.numeric(OGoal$subject[k])
  
  LsimB0O0 <- data.frame(subject = rep.int(subject,sec), time = rep(seq(1, sec, 1),1))
  LsimB0O0 <- as.data.frame(LsimB0O0)
  LSB0O0.df <- as.data.frame(LSB0O0.df)
  LSB0O0.df <- rbind(LSB0O0.df,LsimB0O0)
  
}


####Simulations for remaining game time####
# # # # # # # # # # # # # # # # # # # # # #

#For each strategy#

###B0O0###


BWinB0O0 <- data.frame(matrix(ncol = 4, nrow = 0)) #Results df

for(l in 1:G2){
  
  SW00 <- simmulti.msm(WSB0O0.df, Q00, start = 21)
  EndSW00 <- SW00 %>%
    group_by(subject) %>% 
    slice_tail()
  BWinB0O0  <- rbind(BWinB0O0 ,EndSW00)
}


BLossB0O0 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SL00 <- simmulti.msm(LSB0O0.df, Q00, start = 3)
  EndSL00 <- SL00 %>%
    group_by(subject) %>% 
    slice_tail()
  BLossB0O0  <- rbind(BLossB0O0 ,EndSL00)
}


#save(BWinB0O0, file = "BWinB0O0.RData") #Save the results
#save(BLossB0O0,file = "BLossB0O0.RData")




###B0O1###


BWinB0O1 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SW00 <- simmulti.msm(WSB0O0.df, Q10, start = 21)
  EndSW00 <- SW00 %>%
    group_by(subject) %>% 
    slice_tail()
  BWinB0O1  <- rbind(BWinB0O1 ,EndSW00)
}


BLossB0O1 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SL00 <- simmulti.msm(LSB0O0.df, Q10, start = 3)
  EndSL00 <- SL00 %>%
    group_by(subject) %>% 
    slice_tail()
  BLossB0O1  <- rbind(BLossB0O1 ,EndSL00)
}


#save(BWinB0O1, file = "BWinB0O1.RData")
#save(BLossB0O1,file = "BLossB0O1.RData")


###B1O0###


BWinB1O0 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SW00 <- simmulti.msm(WSB0O0.df, Q01, start = 21)
  EndSW00 <- SW00 %>%
    group_by(subject) %>% 
    slice_tail()
  BWinB1O0  <- rbind(BWinB1O0 ,EndSW00)
}


BLossB1O0 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SL00 <- simmulti.msm(LSB0O0.df, Q01, start = 3)
  EndSL00 <- SL00 %>%
    group_by(subject) %>% 
    slice_tail()
  BLossB1O0  <- rbind(BLossB1O0 ,EndSL00)
}


#save(BWinB1O0, file = "BWinB1O0.RData")
#save(BLossB1O0,file = "BLossB1O0.RData")


###B1O1###


BWinB1O1 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SW00 <- simmulti.msm(WSB0O0.df, Q11, start = 21)
  EndSW00 <- SW00 %>%
    group_by(subject) %>% 
    slice_tail()
  BWinB1O1  <- rbind(BWinB1O1 ,EndSW00)
}


BLossB1O1 <- data.frame(matrix(ncol = 4, nrow = 0))

for(l in 1:G2){
  
  SL00 <- simmulti.msm(LSB0O0.df, Q11, start = 3)
  EndSL00 <- SL00 %>%
    group_by(subject) %>% 
    slice_tail()
  BLossB1O1  <- rbind(BLossB1O1 ,EndSL00)
}


#save(BWinB1O1, file = "BWinB1O1.RData")
#save(BLossB1O1,file = "BLossB1O1.RData")


####Cleaning Results####
# # # # # # # # # # # # 


##Win##
# # # #

BWinB0O0$Strategy <- "B0O0" #Mark the strategy
BWinB0O1$Strategy <- "B0O1" 
BWinB1O0$Strategy <- "B1O0" 
BWinB1O1$Strategy <- "B1O1" 

BW <- rbind(BWinB0O0,BWinB0O1,BWinB1O0,BWinB1O1)
BW$End <- Result(BW$state)

rm(BWinB0O0,BWinB0O1,BWinB1O0,BWinB1O1) #Done

BW$W <- ifelse(BW$End == "W",1,0)
BW$D <- ifelse(BW$End == "D",1,0)
BW$L <- ifelse(BW$End == "L",1,0)

BW <- BW %>%
  group_by(subject,Strategy) %>% 
  mutate(NW = cumsum(W),ND = cumsum(D),NL = cumsum(L)) #How many outcomes for each strategy
                                                       #After each goal
BWEnd <- BW %>%
  group_by(subject,Strategy) %>% 
  slice_tail()

BWEnd$W <- BWEnd$NW/G2
BWEnd$D <- BWEnd$ND/G2
BWEnd$L <- BWEnd$NL/G2

BarcaFirstGoalResults <- BWEnd %>%
  dplyr::select(subject,time,Strategy,W,D,L)

BarcaFirstGoalResults$time <- round(BarcaFirstGoalResults$time/12,digits = 1)

#Expected Payoffs
BarcaFirstGoalResults$BPayoff <- 3*BarcaFirstGoalResults$W + 1*BarcaFirstGoalResults$D  #Expected payoff for each strategy
BarcaFirstGoalResults$OPayoff <- 3*BarcaFirstGoalResults$L + 1*BarcaFirstGoalResults$D 

BarcaFirstGoalResults <- BarcaFirstGoalResults %>%  #Rank the payoffs
  group_by(subject) %>% 
  mutate(BRank = rank(-BPayoff),ORank = rank(-OPayoff))
         
save(BarcaFirstGoalResults,file = "BarcaFirstGoalResults.RData")
write.csv(BarcaFirstGoalResults,'BarcaFirstGoalResults.csv')


#stargazer(BarcaFirstGoalResults[,1:11],summary = FALSE,rownames=FALSE) #LaTeX Table of Results

##Loss##
# # # # 


BLossB0O0$Strategy <- "B0O0" 
BLossB0O1$Strategy <- "B0O1" 
BLossB1O0$Strategy <- "B1O0" 
BLossB1O1$Strategy <- "B1O1" 


BL <- rbind(BLossB0O0,BLossB0O1,BLossB1O0,BLossB1O1)
BL$End <- Result(BL$state)

rm(BLossB0O0,BLossB0O1,BLossB1O0,BLossB1O1) 

BL$W <- ifelse(BL$End == "W",1,0)
BL$D <- ifelse(BL$End == "D",1,0)
BL$L <- ifelse(BL$End == "L",1,0)

BL <- BL %>%
  group_by(subject,Strategy) %>% 
  mutate(NW = cumsum(W),ND = cumsum(D),NL = cumsum(L))

BLEnd <- BL %>%
  group_by(subject,Strategy) %>% 
  slice_tail()

BLEnd$W <- BLEnd$NW/G2
BLEnd$D <- BLEnd$ND/G2
BLEnd$L <- BLEnd$NL/G2

OppFirstGoalResults <- BLEnd %>%
  dplyr::select(subject,time,Strategy,W,D,L)

OppFirstGoalResults$time <- round(OppFirstGoalResults$time/12,digits = 1)

OppFirstGoalResults$BPayoff <- 3*OppFirstGoalResults$W + 1*OppFirstGoalResults$D  #Expected payoff for each strategy
OppFirstGoalResults$OPayoff <- 3*OppFirstGoalResults$L + 1*OppFirstGoalResults$D 

OppFirstGoalResults <- OppFirstGoalResults %>%  #Rank the strategies for each team
  group_by(subject) %>% 
  mutate(BRank = rank(-BPayoff),ORank = rank(-OPayoff))


#save(OppFirstGoalResults,file = "OppFirstGoalResults.RData")
#write.csv(OppFirstGoalResults,'OppFirstGoalResults.csv')

#stargazer(OppFirstGoalResults[,1:10],summary = FALSE,rownames=FALSE)




###Distribution of Goal Times?###

descdist(Goal$TimeG1, discrete=FALSE, boot=500)



####FIXED DPs OVERALL RANKS####
# # # # # # # # # # # # # # # # 
library(readr)
CH3Ranks <- read_csv("CH3Ranks.csv")

CH3Ranks$Brank <- rank(-CH3Ranks$Barcelona)
CH3Ranks$Orank <- rank(-CH3Ranks$Opposition)




stargazer(CH3Ranks,summary = FALSE,rownames=FALSE)
