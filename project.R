
install.packages(c("dplyr","ggplot2",'knitr','manipulate' ,"rmarkdown"))
library(dplyr)
library(ggplot2)
library(knitr)
library(manipulate)
library(datasets)

"
CLT vs Exponent distribution


graph for exponent distribution.


"

#part1

# 1. exponent distribution -theoretical
lambda<-.2
meanEXP<-1/lambda
sdEXP<-1/lambda
#Given the standard deviation 1/lambada, then
varEXP<-sdEXP^2

#2. Exponent Simulation.
x<-1:40
mod1<-lm(c(rexp(40,rate=lambda))~x)
Simulation<- simulate(mod1, nsim = 1000)
MeansPerSimulation<-matrix(lapply(Simulation, mean),nrow = 1000,ncol = 1)
Smean<-mean(as.numeric(c(MeansPerSimulation)))
VarPerSimultation<-matrix(lapply(Simulation, var),nrow = 1000,ncol = 1)#sample variance 
#The CLT states that the distribution of averages of iid variables becomes that of a standard then, 
Svar<-mean(as.numeric(c(VarPerSimultation)))#popVariance



#_____
#Graphs exp simulation
hist(as.numeric(MeansPerSimulation))
# plot<-ggplot(as.data.frame(simMeans),aes(as.numeric(simMeans)))+geom_histogram()x


#part 1A Show the sample mean and compare it to the theoretical mean of the distribution.
"in this graph: one simulaiton with n=40000, 
shows its distrubution,calculated mean and theoretical mean of one simulaton, "
SimPlot<- ggplot()+
  geom_histogram(
    mapping = aes(x=(rexp(40*1000,rate = lambda))),
    color="gray50",bins = 50
  )+
  geom_vline(xintercept=meanEXP,colour="red")+
  geom_vline(xintercept=mean((rexp(1000*40,rate = lambda)),colour="black"))+
  annotate("text",x=mean((rexp(1000*40,rate = lambda))),
           y=3000, 
           label=paste("Sample- Mean: ",mean((rexp(1000*40,rate = lambda)))),
           colour="black")+
  annotate("text",x=meanEXP,y=5000, label=paste("Theoretical- Mean: ",meanEXP),colour="red")+
  labs(title = "Exponent Distrubution Simulation",tag = "Part A1",x="X",y="Y")

SimPlot

"in this graph shows the distribution of the means for n=40 and 1000 simulations "
plotgg2<-ggplot(as.data.frame(MeansPerSimulation),aes(as.numeric(MeansPerSimulation))) + 
  geom_bar(fill = "white",colour = "white") + 
  scale_x_binned()+
  geom_vline(xintercept=meanEXP,colour="red")+
  geom_vline(xintercept=Smean,colour="black")+
  annotate("text",x=Smean,y=300, label=paste("Population-Mean: ",Smean),colour="black")+
  annotate("text",x=meanEXP,y=250, label=paste("Theoretical- Mean: ",meanEXP),colour="red")+
  labs(title = "Exponential Distribution- Theoretical vs Sample",tag = "Part A1",x="X",y="Y")
plotgg2  

#part1B
#Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

hist(as.numeric(  VarPerSimultation))

varPlot<-ggplot(as.data.frame(VarPerSimultation),aes(as.numeric(VarPerSimultation))) +
  geom_histogram(alpha=.5, position="identity", fill="white", col="black")+
  geom_vline(xintercept=varEXP,colour="red")+
  geom_vline(xintercept=Svar,colour="black")+
  annotate("text",x=Svar,y=75, label=paste("Simulation-Variability: ",Svar),colour="black")+
  annotate("text",x=varEXP,y=50, label=paste("Theoretical- Variability: ",varEXP),colour="red")+
  labs(title = "Simulation Variances Distributions ",tag = "Part 1B",x="X",y="Y")
varPlot

"The distribution of the sample variance of a random sample from a population
is centered at the population variance. This means that on average, the sample
variances will be equal to the population variance. The sample variance is a 
measure of the spread of data within a sample, and it is calculated by taking 
the average of the squared differences between each data point and the sample mean. 
When multiple random samples are taken, the sample variances will vary around 
the population variance. However, the distribution of these sample variances is
centered at the population variance, indicating that the average sample variance
will be equal to the population variance."


#part 1C Show that the distribution is approximately normal.

plotgg3<-ggplot(as.data.frame(MeansPerSimulation),aes(as.numeric(MeansPerSimulation))) + 
  geom_histogram(aes(y=after_stat(density)), alpha=.5, position="identity", fill="white", col="black")+
  geom_density(colour="black", size=1)+
  stat_function(fun = dnorm, colour = "red", size=1,args = list(mean = meanEXP, sd = sd(MeansPerSimulation)))+
  annotate("text",x=6.5,y=.5, label="Exponential Distribution",colour="red")+
  annotate("text",x=6.5,y=.4, label=" Central Limit Theorem",colour="black")+
  labs(title = "Central Limit Theorem vs Exponential Distribution",tag = "Part 1C",x="X",y="Y")
plotgg3


#Part2
#Load the ToothGrowth data and perform some basic exploratory data analyses
data("ToothGrowth")
head(ToothGrowth)
str(ToothGrowth)
#Provide a basic summary of the data.
summary(ToothGrowth)

"
- clean data 
  1.separate the data in two groups 
    - OJ and VC 
      - remove column supp
  2. analyse data 
    - Independent t-distribution?
  3. Use confidence intervals 

"
"Use confidence intervals and/or hypothesis tests to compare tooth growth by 
supp and dose. (Only use the techniques from class, even if there's other 
approaches worth considering)"

VC_0.5<-t.test(filter(ToothGrowth,dose==0.5 & supp=="VC")$len,paired = FALSE)$conf.int
VC_1.0<-t.test(filter(ToothGrowth,dose==1.0 & supp=="VC")$len,paired = FALSE)$conf.int
VC_2.0<-t.test(filter(ToothGrowth,dose==2.0 & supp=="VC")$len,paired = FALSE)$conf.int
OJ_0.5<-t.test(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len,paired = FALSE)$conf.int
OJ_1.0=t.test(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len,paired = FALSE)$conf.int
OJ_2.0=t.test(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len,paired = FALSE)$conf.int


GrowthMeansPerSupplementandDose<-
  data.frame("VC_0.5"=mean(filter(ToothGrowth,dose==0.5 & supp=="VC")$len),
             "VC_1.0"=mean(filter(ToothGrowth,dose==1.0 & supp=="VC")$len),
             "VC_2.0"=mean(filter(ToothGrowth,dose==2.0 & supp=="VC")$len),
             "OJ_0.5"=mean(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len),
             "OJ_1.0"=mean(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len),
             "OJ_2.0"=mean(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len))
GrowthSDPerSupplementandDose<-
  data.frame("VC_0.5"=sd(filter(ToothGrowth,dose==0.5 & supp=="VC")$len),
             "VC_1.0"=sd(filter(ToothGrowth,dose==1.0 & supp=="VC")$len),
             "VC_2.0"=sd(filter(ToothGrowth,dose==2.0 & supp=="VC")$len),
             "OJ_0.5"=sd(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len),
             "OJ_1.0"=sd(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len),
             "OJ_2.0"=sd(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len))

supp.5<-t.test(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len,
               filter(ToothGrowth,dose==0.5 & supp=="VC")$len,paired = FALSE)$
  
  supp1<-t.test(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len,
                filter(ToothGrowth,dose==1.0 & supp=="VC")$len,paired = FALSE)$conf.int

supp2<-t.test(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len,
              filter(ToothGrowth,dose==2.0 & supp=="VC")$lenpaired = FALSE)$conf.int

#graphing data

#VC vs OJ 0.5 
VCOJ05<-ggplot()+
  stat_function()
VCOJ05

tG<- ggplot(ToothGrowth,aes(x=dose,y=len,colour = factor(supp)))+ 
  geom_point()+
  labs(title = "Tooth Growth by Supplement",tag ="Part 2C",x="Dose",y="Length")

tG
