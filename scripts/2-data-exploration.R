# Load data
load('rda/clinical_trial_complete.rda')

source("scripts/functions/graph_functions.R")


X <- clinical_trial_complete

# Remove the file
rm(clinical_trial_complete)

str(X)


summary(X)


# 2.1 List of distinct vlues by column
apply(X, 2, function(x) length(unique(x)))

color_1 <- "deepskyblue2"

plothist(col_name = "age", df = X, ylabtext = "All participants",  color_1)

plothist(col_name = "injurytime", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "sbp", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "rr", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "hr", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "ndaysicu", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "ncell", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "nplasma", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "nplatelets", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "ncryo", df = X,ylabtext = "All participants", color_1)



X_quan <- X[,c(3,4,6,7,8,9,10,12,13,14,15)]


summary(X_quan)


library("andrews")
andrews(as.data.frame(cbind(X_quan,as.factor(X[,1]))),
        clr=8,
        ymax=4,
        main="Andrews' Plot for College in terms of Student")