load('rda/clinical_trial_complete.rda')
X <- clinical_trial_complete[,-c(1,11,13,14,15,16,18)]
X <- X[,-c(3,6,7,9,10)]
X_quan <- X[,-c(1,3,6)]

# Categorical variables
Y <- X[,c(1,3,6)]
X <- X_quan

# Remove the file
rm(clinical_trial_complete)
rm(X_quan)

# Define colors for plots

color_1 <- "deepskyblue2"
color_2 <- "seagreen2"
color_3 <- "orange2"
color_4 <- "darkorchid4"


#Define sample size and the dimension of the data matrix

n <- nrow(X)
p <- ncol(X)


##################################################################################################################
# Two possibilities: 

## 1. Obtain PCs on the whole data set 
## 2. A PC's for two (each) groups
# See what happens if we obtain PCs on the whole data set
# Use the sample correlation matrix

X_pcs <- prcomp(X,scale=TRUE)

##################################################################################################################
# PC scores

dim(X_pcs$x)
head(X_pcs$x)


# Plot the PC for each group
colnames(Y)

######### SEX
colors_sex <- c(color_2,color_3)[1*(Y[,1]=="male")+1]
plot(X_pcs$x[,1:2],pch=19,col=colors_sex, main = "First 2 PC by Sex")


######### PAIN
colors_pain <- c(color_2,color_3,color_4)[1 + ifelse(Y[,2]== "blunt", 1,
                                              ifelse(Y[,2]=="penetrating",2,
                                                     0))]
plot(X_pcs$x[,1:2],pch=19,col=colors_pain)


######### death 1 (death)

colors_death <- c(color_2,color_3)[1*(Y[,3]==0)+1]
plot(X_pcs$x[,1:2],pch=19,col=colors_death, main = "First 2 PC by death")


#######################################
# Have a look at these eigenvalues

# Screeplot

library(factoextra)
fviz_eig(X_pcs,ncp=17,addlabels=T,barfill=color_1,barcolor=`color_4`)

get_eigenvalue(X_pcs)

# 4 PC's explain 62% of the data


##################################################################################################################
# Interpretation of the first PC: Weights for the first PC

plot(1:p,X_pcs$rotation[,1],pch=19,col=color_1,main="Weights for the first PC",
     xlab="Variables",ylab="Score")
abline(h=0)
text(1:p,X_pcs$rotation[,1],labels=colnames(X),pos=1,col=color_4,cex=0.75)


# Interpretation of the second PC: Weights for the second PC

plot(1:p,X_pcs$rotation[,2],pch=19,col=color_1,main="Weights for the second PC",
     xlab="Variables",ylab="Score")
abline(h=0)
text(1:p,X_pcs$rotation[,2],labels=colnames(X),pos=1,col=color_4,cex=0.75)


##################################################################################################################
# Have a look at the important variables in the first two PCs
# Note the different groups in the data
# The radius is arbitrary

plot(X_pcs$rotation[,1:2],pch=19,col=color_1,main="Weights for the first two PCs")
abline(h=0,v=0)
text(X_pcs$rotation[,1:2],labels=colnames(X),pos=1,col=color_4,cex=0.75)
library(plotrix)
draw.circle(0,0,0.3,border=color_4,lwd=3)

##################################################################################################################
# The biplot is an alternative way to plot points and the first two PCs together 
# However, it is only useful when the data set is not too large

biplot(X_pcs,col=c(color_1,color_4),cex=c(0.5,0.8))

##################################################################################################################
# Plot the scores (the four PCs)

pairs(X_pcs$x[,1:4],col=colors_X,pch=19,main="The first four PCs")

# The PCs show that the second PC is the key to show the two groups

# The first one is a PC that creates a ranking of colleges. See the first ten colleges

sort(X_pcs$x[,1],decreasing=TRUE)[1:10]

##################################################################################################################
# Plot the correlations between the original data set and the scores
# This is useful to understand also which are the important variables in the data set in terms of variability
# First, plot all the PCs
# Second, plot only the first four PCs

library(corrplot)
corrplot(cor(X,X_pcs$x),is.corr=T)
corrplot(cor(X,X_pcs$x[,1:4]),is.corr=T)