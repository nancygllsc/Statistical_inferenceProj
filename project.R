
install.packages(c("dplyr","ggplot2",'knitr','manipulate' ,"rmarkdown","patchwork","LaTeX"))
library(dplyr)
library(ggplot2)
library(knitr)
library(manipulate)
library(datasets)
library(patchwork)
library(LaTeX)
"
CLT vs Exponent distribution


graph for exponent distribution.

population n=40000
sample n=40
theoretical

"

#part1

# 1. exponent distribution -theoretical
lambda<-.2
meanEXP<-1/lambda
sdEXP<-1/lambda
#Given the standard deviation 1/lambada, then
varEXP<-sdEXP^2
TheoreticalMean <- meanEXP


#2.Simulation.
require(stats)
set.seed(51497781)
RSimulation<-(rexp(40*1000,rate = lambda))
RSimulation1<-(rexp(40,rate = lambda))

SampleMean<-round(mean(RSimulation1),2)
SampleVariance<-round(var(RSimulation1),2)
SampleStandardDeviation <- round(sd(RSimulation1),2)

PopulationMean<-round(mean(RSimulation),2)
PopulationVariance<-round(var(RSimulation*(40000-1)/40000),2)
PopulationSD<-round(sd(RSimulation*(40000-1)/40000),2)

#2.1 Thousand-Simulation n=40 
x<-1:40
set.seed(51497781)
mod1<-lm(c(rexp(40,rate=lambda))~x)
Simulation<- simulate(mod1, nsim = 1000)
MeansPerSimulation<-matrix(lapply(Simulation, mean),nrow = 1000,ncol = 1)
Smean<-round(mean(as.numeric(c(MeansPerSimulation)),2))
Smedian<- round(median(as.numeric(MeansPerSimulation)),2)
Ssd<-round(sd(MeansPerSimulation),2 )
q05<-round(quantile(as.numeric(MeansPerSimulation),probs = .05),2)
q34<-round(quantile(as.numeric(MeansPerSimulation),probs = .34),2)
q68<-round(quantile(as.numeric(MeansPerSimulation),probs = .68),2)
q95<-round(quantile(as.numeric(MeansPerSimulation),probs = .95),2)
Ssd<-round(sd(MeansPerSimulation),2 )

VarPerSimultation<-matrix(lapply(Simulation, var),nrow = 1000,ncol = 1)#sample variance 
Svar<-round(var(as.numeric(VarPerSimultation)),2)
SvarMean<-round(mean(as.numeric(VarPerSimultation)),2)
Vq05<-round(quantile(as.numeric(VarPerSimultation),probs = .05),2)
Vq34<-round(quantile(as.numeric(VarPerSimultation),probs = .34),2)
Vq68<-round(quantile(as.numeric(VarPerSimultation),probs = .68),2)
Vq95<-round(quantile(as.numeric(VarPerSimultation),probs = .95),2)
Vsd<-round(sd(VarPerSimultation),2 )

#_____
#Graphs exp simulation
hist(as.numeric(MeansPerSimulation))
# plot<-ggplot(as.data.frame(simMeans),aes(as.numeric(simMeans)))+geom_histogram()x


#part 1A Show the sample mean and compare it to the theoretical mean of the distribution.
"in this graph: one simulaiton with n=40, 
shows its distrubution,calculated mean and theoretical mean of one simulaton, "

SimSample<-ggplot()+
  geom_histogram(mapping = aes(x=RSimulation1),bins = 30,color="white")+
  geom_vline(mapping = aes(xintercept=c(SampleMean,SampleVariance, SampleStandardDeviation),
                           color=c("Sample Mean","Sample Variance","Sample SD")))+
  scale_color_manual(name=" ",values = c("red","blue","purple"))+
  theme(legend.position = "bottom")+
  labs(title = "Exponential Distribution n=40",
       caption = paste("Sample n=40",
                       "\n", 
                       "Theoretical Mean: ", meanEXP,
                       "\n",
                       "Sample Mean: ", SampleMean,
                       "\n",
                       "Sample Variance: ", SampleVariance,
                       "\n",
                       "Sample SD", SampleStandardDeviation,
                       "\n"
                       ),
       tag = "Graph 1",
       x="Size",
       y="Frequency")
  
SimSample

# simulations n=40000 following exponent distribution
#The CLT states that the distribution of averages of iid variables becomes that of a standard then, 

SimPlot<- ggplot()+
  geom_histogram(
    mapping = aes(x=RSimulation),
    color="white",bins = 50
  )+
  geom_vline(
    mapping=
      aes(
        xintercept=c(TheoreticalMean,PopulationMean,PopulationVariance), 
        color=paste(c("Theoretical Mean","Population Mean","Population Variance"))
      )
  )+    
  scale_color_manual(name = "", values = paste(c("red", "purple","blue")) ) +

  theme(legend.position = "bottom") +
  
  labs(title = "Exponential Distribution n=40,000",
       caption = paste("Sample n=40,000",
                       "\n", 
                       "Theoretical Mean: ", meanEXP,
                       "\n",
                       "Population Mean: ", PopulationMean,
                       "\n",
                       "Population Variance: ", PopulationVariance,
                       "\n",
                       "Population SD: ",PopulationSD),
       tag = "Graph 2",
       x="Size",
       y="Frequency")
SimPlot



#1000 simulations n=40
"in this graph shows the distribution of the means for n=40 and 1000 simulations "

plotgg2<-ggplot() + 
  geom_histogram(mapping=
                   aes(x=as.numeric(MeansPerSimulation)),bins=30,color="white")+
  geom_vline(mapping=
    aes(xintercept=c(PopulationMean,meanEXP,Smean),
               color=paste(
                 c("Population Mean","Theoretical Mean", "Center of Mass"))
    ))+
  scale_color_manual(name = "", values = paste(c("blue", "red", "purple"))) +
  theme(legend.position = "bottom") +
  
  labs(title = "Sample Means Distribution",
       tag = "Graph 3",
       x="Size",y="Frequency",
       caption = paste("PopulationMean: ",PopulationMean,"\n",
                       "Theoretical mean: ",meanEXP,"\n",
                       "Means Distribution Mean: ", Smean,"\n",
                       "Variability: ",Svar,
                       "\n",
                       "The Samples Mean Distribution follows the Central Limit Theorem",
                       "\n",
                       "which states that as the sample size increases, the distribution","\n", 
                       "of sample means approaches a normal distribution.", "\n"
                       ))
plotgg2


#put togeter the graphs

part1A<-SimSample+SimPlot+plot_layout(ncol = 1)
part1A
#--------------------------------------------------------------------


#part 1A-B Show that the distribution is approximately normal.
"show that the behavior of a normal distribution is similar to the the 
behavior of the MeansPerSimulation distribution:
1. The mean, median and mode are exactly the same.
2. The distribution is symmetric about the mean—half the values fall below the mean and half above the mean.
3. The distribution can be described by two values: the mean and the standard deviation
4. Approximately 68\%, 95\% and 99\% of the normal density lies within 1, 2 and 3 standard
deviations from the mean, respectively
"

plotgg3<-ggplot() + 
  geom_histogram(mapping = aes(as.numeric(MeansPerSimulation)), 
                 alpha=.5, position="identity",
                 fill="white", col="black",bins = 30)+
  #normal distribution
  geom_vline(mapping=
               aes(xintercept=
                     c(q05,q34,q68,q95,
                       Smean) ,
                   color= paste(c("5%","34%","68%","95%",
                                  "Mean")
                                )
               )
             )+
  scale_color_manual(name = "", values = paste(c("blue", "blue",
                                                 "blue","blue",
                                                 "coral"
                                                   
                                                 ))) +
  theme(legend.position = "bottom") +
  
  labs(title = "Simulations Variance Distribution. "
       ,tag = "Part 1B",x="Size",y="Y",
       caption=paste("Mean: ",Smean ,
                     "\n",
                     "5% :",q05,
                     " ",
                     "34% :",q34,
                     " ",
                     "68% :",q68,
                     " ",
                     "95% : ",q95,
                     "\n",
                     "SD: ", Ssd,
                     "\n",
                     "Variability: ",Svar,
                     "\n"
                          
))+
  #stat_function(fun = rnorm, colour="red",size=1, 
                #args = list(n=40000,mean=meanEXP,sd=sdEXP))+
  labs(title = "Means Distribution",
       tag = "Graph 4",x="size",y="Frequency")
plotgg3

plotgg4<-ggplot() + 
  geom_histogram(mapping = aes(as.numeric(MeansPerSimulation)), 
                 alpha=.5, position="identity",
                 fill="white", col="black",bins = 30)+
  #normal distribution
  geom_vline(mapping=
               aes(xintercept=
                     c(
                       Smean,
                       Smean+Ssd,Smean-Ssd,
                       Smean+(2*Ssd),Smean-(2*Ssd)
                     ) ,
                   color= paste(c(
                                  "Mean",
                                  "SD1-R","SD1-L",
                                  "SD2-R","SD2-L"
                                  
                   ))
               )
  )+
  scale_color_manual(name = "", values = paste(c(
                                                 "coral",
                                                 "darkorchid","cyan",
                                                 "darkgreen","red"  
  ))) +
  theme(legend.position = "bottom") +
  
  labs(title = "Simulations Variance Distribution. "
       ,tag = "Graph 5",x="Size",y="Y",
       caption=paste("Mean: ",Smean ,
                    
                     "\n",
                     "SD: ", Ssd,
                     "\n",
                     "2SD-R",Smean+2*Ssd,
                     " ",
                     "1SD-R",Smean+Ssd,
                     "\n",
                     
                     "2SD-L",Smean-2*Ssd,
                     " ",
                     "1SD-L",Smean-Ssd,
                     "\n",
                     
                     "Theoretical SD", sdEXP,
                     "\n",
                     "Variability: ",Svar,
                     "\n"
       ))+
  #stat_function(fun = rnorm, colour="red",size=1, 
  #args = list(n=40000,mean=meanEXP,sd=sdEXP))+
  labs(title = "Means Distribution",
       tag = "Graph 5",x="Size",y="Frequency")
plotgg4

comparing<-plotgg3+plotgg4 + plot_layout(ncol = 1)
comparing

#--------varability
#part1B
#Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

varPlot<-ggplot() +
  geom_histogram(mapping = aes(as.numeric(VarPerSimultation)),
                 alpha=.5, position="identity",
                 fill="white", col="black",bins = 30)+
  geom_vline(mapping=
               aes(xintercept=c(Svar,SampleVariance,varEXP,SvarMean,PopulationVariance),
                   color=c("Distribution ","Sample","Theoretical ",
                                 "Distribution Mean","Population")
               ))+
  scale_color_manual(name = "", values = paste(c("blue", "cyan", "green","purple","red"))) +
  theme(legend.position = "bottom") +
  
  labs(title = "Simulations Variance Distribution. "
       ,tag = "Graph 6",x="Size",y="Frequency",
       caption=paste("Distribution Variability: ",Svar,
                     "\n",
                     "Sample Variability: ", SampleVariance,
                     "\n",
                     "Theoretical Variability: ", varEXP,
                     "\n",
                     "Distribution Mean:", SvarMean,
                     "\n",
                     "Population Variance", PopulationVariance
                     )
  )
varPlot

VarPlot2<-ggplot() + 
  geom_histogram(mapping = aes(as.numeric(VarPerSimultation)), 
                 alpha=.5, position="identity",
                 fill="white", col="black",bins = 30)+
  #normal distribution
  geom_vline(mapping=
               aes(xintercept=
                     c(
                       varEXP,
                       SvarMean+Vsd,SvarMean-Vsd,
                       SvarMean+(2*Vsd),SvarMean-(2*Vsd),
                       PopulationVariance
                     ) ,
                   color= paste(c(
                     "Theoretical Variance",
                     "SD1-R","SD1-L",
                     "SD2-R","SD2-L"
                     
                     
                   ))
               )
  )+
  scale_color_manual(name = "", values = paste(c(
    "coral",
    "darkorchid","cyan",
    "darkgreen","red", "blue" 
  ))) +
  theme(legend.position = "bottom") +
  
  labs(title = "Simulations Variance Distribution. "
       ,tag = "Graph 5",x="Size",y="Y",
       caption=paste("Mean: ",Smean ,
                     
                     "\n",
                     "SD: ", Vsd,
                     "\n",
                     "2SD-R",SvarMean+2*Vsd,
                     " ",
                     "1SD-R",SvarMean+Vsd,
                     "\n",
                     
                     "2SD-L",SvarMean-2*Vsd,
                     " ",
                     "1SD-L",SvarMean-Vsd,
                     "\n",
                     
                     "Theoretical SD", sdEXP,
                     "\n",
                     "Distribution Variability: ",Svar,
                     "\n"
       ))+
  #stat_function(fun = rnorm, colour="red",size=1, 
  #args = list(n=40000,mean=meanEXP,sd=sdEXP))+
  labs(title = "Means Distribution",
       tag = "Graph 7",x="Size",y="Frequency")
VarPlot2
varianceCompare<-varPlot+VarPlot2+plot_layout(ncol = 1)
"The distribution of the sample variance of a random sample from a population
is centered at the population variance. This means that on average, the sample
variances will be equal to the population variance. The sample variance is a 
measure of the spread of data within a sample, and it is calculated by taking 
the average of the squared differences between each data point and the sample mean. 
When multiple random samples are taken, the sample variances will vary around 
the population variance. However, the distribution of these sample variances is
centered at the population variance, indicating that the average sample variance
will be equal to the population variance."




#------------------------------------------------------------------------------
#Part2
install.packages("UsingR")
library(UsingR)

#Load the ToothGrowth data and perform some basic exploratory data analyses
data("ToothGrowth")
head(ToothGrowth)
str(ToothGrowth)
#Provide a basic summary of the data.
summary(ToothGrowth)

"
x<-ToothGrowth$dose
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(length(x)))
"

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

VC_0.5<-t.test(filter(ToothGrowth,dose == 0.5 & supp=="VC")$len,paired = FALSE)$conf.int
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
               paired = FALSE, var.equal = FALSE, 
               data = as.data.frame( filter(ToothGrowth,dose==0.5 & supp=="VC")$len))$conf
supp1<-t.test(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len,
              paired = FALSE, var.equal = FALSE, 
              data = as.data.frame( filter(ToothGrowth,dose==1.0 & supp=="VC")$len))$conf.int

supp2<-t.test(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len,
              paired = FALSE, var.equal = FALSE, 
              data = as.data.frame( filter(ToothGrowth,dose==2.0 & supp=="VC")$len))$conf.int

#graphing data

#VC vs OJ 0.5 
VCOJ05<-ggplot()+
  geom_boxplot(aes( colour=supp))
VCOJ05

tG<- ggplot(ToothGrowth,aes(x=dose,y=len,colour = factor(supp)))+ 
  geom_point()+
  labs(title = "Tooth Growth by Supplement",
       tag ="Part 2C",x="Dose",y="Length")

tG
----------
conIntVC05<-(mean(filter(ToothGrowth,dose==0.5 & supp=="VC")$len) + c(-1, 1) * qnorm(0.975) * sd(filter(ToothGrowth,dose==0.5 & supp=="VC")$len)/sqrt(length(filter(ToothGrowth,dose==0.5 & supp=="VC")$len)))
conIntVC1.0<-(mean(filter(ToothGrowth,dose==1.0 & supp=="VC")$len) + c(-1, 1) * qnorm(0.975) * sd(filter(ToothGrowth,dose==1.0 & supp=="VC")$len)/sqrt(length(filter(ToothGrowth,dose==1.0 & supp=="VC")$len)))
conIntVC2.0<-(mean(filter(ToothGrowth,dose==2.0 & supp=="VC")$len) + c(-1, 1) * qnorm(0.975) * sd(filter(ToothGrowth,dose==2.0 & supp=="VC")$len)/sqrt(length(filter(ToothGrowth,dose==2.0 & supp=="VC")$len)))
conIntOJ05<-(mean(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len) + c(-1, 1) * qnorm(0.975) * sd(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len)/sqrt(length(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len)))
conIntOJ1.0<-(mean(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len) + c(-1, 1) * qnorm(0.975) * sd(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len)/sqrt(length(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len)))
conIntOJ2.0<-(mean(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len) + c(-1, 1) * qnorm(0.975) * sd(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len)/sqrt(length(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len)))

conf1<- ggplot()+
  geom_point()+
  geom_errorbar( aes(x=filter(ToothGrowth,dose==0.5 & supp=="VC")$dose,y=filter(ToothGrowth,dose==0.5 & supp=="VC")$len, ymin = conIntVC05[1], ymax = conIntVC05[2],width = .1,colour="VC 0.5"))+
  geom_errorbar( aes(x=filter(ToothGrowth,dose==1.0 & supp=="VC")$dose,y=filter(ToothGrowth,dose==1.0 & supp=="VC")$len, ymin = conIntVC1.0[1], ymax = conIntVC1.0[2],width = .1,colour="VC 1.0"))+
  geom_errorbar( aes(x=filter(ToothGrowth,dose==2.0 & supp=="VC")$dose,y=filter(ToothGrowth,dose==2.0 & supp=="VC")$len,ymin = conIntVC2.0[1], ymax = conIntVC2.0[2],width = .1,colour="VC 2.0"))+
  geom_errorbar( aes(x=filter(ToothGrowth,dose==0.5 & supp=="OJ")$dose,y=filter(ToothGrowth,dose==0.5 & supp=="OJ")$len,ymin = conIntOJ05[1], ymax = conIntOJ05[2],width = .1,colour="OJ 0.5"))+
  geom_errorbar( aes(x=filter(ToothGrowth,dose==1.0 & supp=="OJ")$dose,y=filter(ToothGrowth,dose==1.0 & supp=="OJ")$len,ymin = conIntOJ1.0[1], ymax = conIntOJ1.0[2],width = .1,colour="OJ 1.0"))+
  geom_errorbar( aes(x=filter(ToothGrowth,dose==2.0 & supp=="OJ")$dose,y=filter(ToothGrowth,dose==2.0 & supp=="OJ")$len,ymin = conIntOJ2.0[1], ymax = conIntOJ2.0[2],width = .1,colour="OJ 2.0"))+
  theme(legend.position = "bottom")+
  labs(title = "Tooth Growth by Supplement",tag ="Part 2",x="Dose",y="Length",
       caption = paste("Means - Confidence Intervals","\n",
        "VC_0.5: ",mean(filter(ToothGrowth,dose==0.5 & supp=="VC")$len),
        " Min",round(conIntVC05[1],2),"Max ",round(conIntVC05[2],2),
       "\n",
       "VC_1.0: ",mean(filter(ToothGrowth,dose==1.0 & supp=="VC")$len),
       " Min",round(conIntVC1.0[1],2),"Max ",round(conIntVC1.0[2],2),
       "\n",
       "VC_2.0: ",mean(filter(ToothGrowth,dose==2.0 & supp=="VC")$len),
       " Min",round(conIntVC2.0[1],2),"Max ",round(conIntVC2.0[2],2),
       "\n",
       "OJ_0.5: ",mean(filter(ToothGrowth,dose==0.5 & supp=="OJ")$len),
       " Min",round(conIntOJ05[1],2),"Max ",round(conIntOJ05[2],2),
       "\n",
       "OJ_1.0: ",mean(filter(ToothGrowth,dose==1.0 & supp=="OJ")$len),
       " Min",round(conIntOJ1.0[1],2),"Max ",round(conIntOJ1.0[2],2),
       "\n",
       "OJ_2.0: ",mean(filter(ToothGrowth,dose==2.0 & supp=="OJ")$len),
       " Min",round(conIntOJ2.0[1],2),"Max ",round(conIntOJ2.0[2],2),
       "\n"))
conf1


