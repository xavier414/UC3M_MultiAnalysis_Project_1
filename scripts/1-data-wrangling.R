# List of colors

#List of color
color_1 <- "deepskyblue2"
color_2 <- "seagreen2"
color_3 <- "orange2"

library(dplyr)


load('data/crash2.rda')

# Create discrete variable deth

crash2$death <- as.factor(ifelse(is.na(crash2$ddeath),0,1))

clinical_trial <- crash2 %>% select(1,5,6,7,8,9,10,11,12,23,37,38,39,40,41,42,45,35)

summary(clinical_trial)

# Review for life and death people
## death <- clinical_trial %>% filter(death == 1)
## summary(death)

clinical_trial_complete <-  clinical_trial[complete.cases(clinical_trial), ]


anyNA(clinical_trial_complete)


clinical_trial_complete <- clinical_trial_complete %>% mutate(btransf = factor(btransf),
                                                              bvii = factor(bvii),
                                                              bloading = factor(bloading))


summary(clinical_trial_complete)

save(clinical_trial_complete, file = 'rda/clinical_trial_complete.rda')
