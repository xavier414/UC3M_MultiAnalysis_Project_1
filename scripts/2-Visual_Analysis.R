# Load data

library(ggplot2)

load('rda/clinical_trial_complete.rda')

source("scripts/functions/graph_functions.R")

summary(clinical_trial_complete)
colnames(clinical_trial_complete)

# Deleting some categorical variables
X <- clinical_trial_complete[,-c(1,11,13,14,15,16,18)]

#colors
color_1 <- "deepskyblue2"
color_2 <- "orange2"
color_3 <- "seagreen2"


# Remove the file
rm(clinical_trial_complete)

str(X[,-c(12:16)])

summary(X)

colnames(X)


# 2.1 List of distinct values by column
apply(X, 2, function(x) length(unique(x)))



# 1. age
plothist(col_name = "age", df = X, ylabtext = "All participants",  color_1, density_plot = FALSE)

# 2. injurytime

## High right skewed. Taking log
plothist(col_name = "injurytime", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "log_injurytime", df = X,ylabtext = "All participants", color_1, breaks = 10, density_plot = FALSE)

# 3. sbp
plothist(col_name = "sbp", df = X,ylabtext = "All participants", color_1)

# 4. rr 

## Skewed
plothist(col_name = "rr", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "log_rr", df = X,ylabtext = "All participants", color_1, breaks = 8 ,density_plot = FALSE)

# 5. cc

## skewed
plothist(col_name = "cc", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "log_cc", df = X,ylabtext = "All participants", color_1, breaks = 8, density_plot = FALSE)



# 6. hr
plothist(col_name = "hr", df = X,ylabtext = "All participants", color_1)



# 7. ndaysicu

## Skewed
plothist(col_name = "ndaysicu", df = X,ylabtext = "All participants", color_1)


plothist(col_name = "log_ndaysicu", df = X,ylabtext = "All participants", color_1, breaks = 10, density_plot = FALSE)

# 8. ncell

## Skewed
plothist(col_name = "ncell", df = X,ylabtext = "All participants", color_1)

plothist(col_name = "log_ncell", df = X,ylabtext = "All participants", color_1, breaks = 8 ,density_plot = FALSE)


# Categorical variables

## Sex
ggplot(X) +
  geom_bar(aes( x = sex, fill = sex)) +
  labs(title = "Distribution of sex") 


## death
ggplot(X) +
  geom_bar(aes( x = death, fill = death)) +
  labs(title = "Distribution of death") 

## death
ggplot(X) +
  geom_bar(aes( x = injurytype, fill = injurytype)) +
  labs(title = "Distribution of injurytype") 


########## 

# Take only the log of colums in the skewed cases
colnames(X)

X <- X[,-c(3,6,7,9,10)]
X_quan <- X[,-c(1,3,6)]
X_quan <- X_quan[,c(1,4,2,5,6,3,7,8)] 


plothist_factor("age", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("log_injurytime", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("sbp", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("log_rr", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("log_cc", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("hr", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("log_ndaysicu", "death", X, "Death", color_2, color_3, density_plot = FALSE)

plothist_factor("log_ncell", "death", X, "Death", color_2, color_3, density_plot = FALSE)



####### Complete observations
# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/pairs.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
pairs(X_quan, pch=19, col=color_1)

# Step 3: Run dev.off() to create the file!
dev.off()


####### 
# death

colnames(X_quan)
color_2 <- "seagreen2"
color_3 <- "orange2"
colors_death <- c(color_2,color_3)[1*(X$death==1)+1]

# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/pairs_death.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
pairs(X_quan, pch=19, col=colors_death,lower.panel=NULL)

# Step 3: Run dev.off() to create the file!
dev.off()


###########################################
# Parallel coordinates plots for the quantitative variables in the bank customers
#############################################

library("MASS")

# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/pcp.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
parcoord(X_quan,
         col=color_1,
         var.label=TRUE,
         main="PCP for individuals in Crash2 trial")

# Step 3: Run dev.off() to create the file!
dev.off()


####### Survival

# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/pcp_death.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
parcoord(X_quan,
         col=colors_death,
         var.label=TRUE,
         main="PCP for individuals in Crash2 trial in terms of death")

# Step 3: Run dev.off() to create the file!
dev.off()





colnames(X)



library("andrews")
# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/andrew_death.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code

andrews(as.data.frame(cbind(X_quan,as.factor(X[,6]))),
        clr=8,
        ymax=4,
        main="Andrews' Plot for individuals in Crash2 trial in terms of death")

# Step 3: Run dev.off() to create the file!
dev.off()

