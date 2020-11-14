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

plothist(col_name = "age", df = X, color_1)

plothist(col_name = "injurytime", df = X, color_1)

plothist(col_name = "sbp", df = X, color_1)

plothist(col_name = "rr", df = X, color_1)

