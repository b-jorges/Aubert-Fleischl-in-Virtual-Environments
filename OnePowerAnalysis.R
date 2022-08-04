source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              "/SimulateDataFunction.r"))
require(dplyr)
require(purrr)
require(lme4)
require(quickpsy)
require(ggplot2)
require(cowplot)
theme_set(theme_cowplot())
require(lmerTest)
set.seed(65)

RangeNs = c(20,30,40,50)
RangeRepetitions = c(50)
RangeEffectSizes = c(0.0025,0.005,0.0075,0.01)

ConditionOfInterest = c(0,1)
StandardValues = c(2,4,6)
PSE_Difference = 0.01
JND_Difference = 0
Multiplicator_PSE_Standard = 0
Multiplicator_SD_Standard = 0.15
Type_ResponseFunction = "normal"
SD_ResponseFunction = 0.1
Mean_Variability_Between = 0.2
SD_Variability_Between = 0.2

nIterations = 200
TimeStartSimulations = Sys.time()

PowerfulDataframe = data.frame()

for (nParticipants in RangeNs){
  for (reps in RangeRepetitions){
    for (PSE_Difference in RangeEffectSizes){
    TimeStartTrial = Sys.time() #get time at beginning of trial

    for(i in 1:nIterations){
      
      print(nParticipants)
      print(PSE_Difference)
      print(i)
      
      Psychometric = SimulatePsychometricData(nParticipants,
                                              ConditionOfInterest,
                                              StandardValues,
                                              reps,
                                              PSE_Difference,
                                              JND_Difference,
                                              Multiplicator_PSE_Standard,
                                              Multiplicator_SD_Standard,
                                              Type_ResponseFunction,
                                              SD_ResponseFunction,
                                              Mean_Variability_Between,
                                              SD_Variability_Between)
      
      lul = (quickpsy::quickpsy(Psychometric,x = Presented_TestStimulusStrength, k = Answer, 
                         grouping = .(ConditionOfInterest,StandardValues,ID), bootstrap = "none"))$parini %>% 
            filter(paran == "p1")
      
      LMM = lmer(par ~ ConditionOfInterest + 
                     (StandardValues| ID), 
                   data = lul)
      
      PowerfulDataframe = rbind(PowerfulDataframe,
                                c(nParticipants=nParticipants,
                                  pvalue_PSE = summary(LMM)$coefficients[10],
                                  iteration = i,
                                  Effect = PSE_Difference))
    }
    print(paste0("200 iterations took ", round(Sys.time() - TimeStartTrial), " seconds. The power for the current run through (",nParticipants," Participants, ", reps, " Repetitions) is ",mean(PowerfulDataframe$pvalue_PSE[PowerfulDataframe$nParticipants == nParticipants & PowerfulDataframe$PSE_Difference == PSE_Difference] < 0.05)))
    }
  }
}

# save(PowerfulDataframe,file =
#        paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SimulatedDataset_SpeedEstimation.RData"))

load(file =
       paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SimulatedDataset_SpeedEstimation.RData"))

colnames(PowerfulDataframe) = c("nParticipants","pvalue_PSE","iteration","Effect_Size")

alpha = 0.05

PowerfulDataframe = PowerfulDataframe %>% group_by(nParticipants,Effect_Size) %>% 
  mutate(Power_PSE = mean(pvalue_PSE < alpha))

PowerfulDataframe2 = (PowerfulDataframe %>% group_by(nParticipants,Effect_Size) %>% 
  slice(1))

PowerfulDataframe2

ggplot(PowerfulDataframe2,aes(nParticipants,Power_PSE,col = as.factor(Effect_Size*100))) +
  geom_line(size = 2) + 
  xlab("Number of Participants") +
  ylab("Power") +
  labs(col = "Effect Size %") +
  scale_color_manual(values = colorRampPalette(c("violet","pink"))(4))
ggsave("Figures/PowerAnalysis.jpg")
  
  
