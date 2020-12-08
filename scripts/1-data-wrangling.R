library(dplyr)


load('data/crash2.rda')

# Create discrete variable deth

crash2$death <- as.factor(ifelse(is.na(crash2$ddeath),0,1))

clinical_trial <- crash2 %>% select(1,5,6,7,8,9,10,11,12,23,37,38,39,40,41,42,45,35)

summary(clinical_trial)

clinical_trial_complete <-  clinical_trial[complete.cases(clinical_trial), ]


anyNA(clinical_trial_complete)


summary(clinical_trial_complete)

clinical_trial_complete <- clinical_trial_complete %>% mutate(btransf = factor(btransf),
                                                              bvii = factor(bvii),
                                                              bloading = factor(bloading),
                                                              nplasma = factor(ifelse(nplasma > 0,1,0)),
                                                              nplatelets = factor(ifelse(nplatelets > 0,1,0)),
                                                              ncryo = factor(ifelse(ncryo > 0,1,0))
)

clinical_trial_complete$log_injurytime <- log(clinical_trial_complete$injurytime + 1)
clinical_trial_complete$log_rr         <- log(clinical_trial_complete$rr)
clinical_trial_complete$log_cc         <- log(clinical_trial_complete$cc) 
clinical_trial_complete$log_ndaysicu   <- log(clinical_trial_complete$ndaysicu + 1)
clinical_trial_complete$log_ncell      <- log(clinical_trial_complete$ncell + 1)


summary(clinical_trial_complete)

## Recomendations:

#1) Take logarithms in right skewed distribution

# injurytime
# rr
# cc
# ndaysicu
# ncell


# transform - skip
# nplasma
# nplatelets
# ncryo

save(clinical_trial_complete, file = 'rda/clinical_trial_complete.rda')
