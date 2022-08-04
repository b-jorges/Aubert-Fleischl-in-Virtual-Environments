library(ggplot)
library(quickpsy)
library(dplyr)

require(cowplot)
theme_set(theme_cowplot())

Data = read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                       "/Data.csv"))

lul = (quickpsy::quickpsy(Data,x = velH_Pest, 
                               k = Pest_Faster, 
                               grouping = .(Environment,Fixation,velH,ID), 
                               bootstrap = "none"))$parini %>% 
                 filter(paran == "p1")

hello = quickpsy::quickpsy(Data,x = velH_Pest, 
                   k = Pest_Faster, 
                   grouping = .(Environment,Fixation,velH))

LMM = lmer(par ~ Environment * Eyes + 
             (velH| ID), 
           data = lul)

LM = lm(par ~ Environment * Fixation + velH, 
           data = lul)
summary(LM)



ggplot(Data,aes(trial,velH_Pest,color = as.factor(velH))) +
  geom_point() +
  geom_line() + 
  facet_grid(Environment~Fixation)