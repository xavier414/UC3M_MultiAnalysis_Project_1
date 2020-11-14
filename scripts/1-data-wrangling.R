# List of colors

#List of color
color_1 <- "deepskyblue2"
color_2 <- "seagreen2"
color_3 <- "orange2"


load('data/crash2.rda')

library(dplyr)

clinical_trial <- crash2 %>% select(1,5,6,7,8,9,10,11,12,37,38,39,40,41,42,43,44)

clinical_trial_complete <-  clinical_trial[complete.cases(clinical_trial), ]

clinical_trial_complete <- clinical_trial_complete %>% mutate(btransf = factor(btransf),
                                                              bvii = factor(bvii)
                                                              )


save(clinical_trial_complete, file = 'rda/clinical_trial_complete.rda')
