options(digits=4)

# Define colors for plot

color_1 <- "deepskyblue2"
color_2 <- "seagreen2"
color_3 <- "orange2"


load('rda/clinical_trial_complete.rda')
X <- clinical_trial_complete[,-c(1,11,13,14,15,16,18)]

colnames(X)

X <- X[,-c(3,6,7,9,10)]
X_quan <- X[,-c(1,3,6)]



# Remove the file
rm(clinical_trial_complete)


# Compute sample mean vector, covariance matrix and correlation matrix

m <- colMeans(X_quan)
m
S <- cov(X_quan)
S
R <- cor(X_quan)
R

library("corrplot")
corrplot(R)

par(mfrow=c(2,2))
corrplot(R)
corrplot(R)

##################################################################################################################
# Data matrices with quantitative variables with students and non-students
##################################################################################################################

X_death <- X_quan[X$death==1,]
X_life <- X_quan[X$death==0,]

##################################################################################################################
# Compute sample mean vector, covariance matrix and correlation matrix for Students and non-Students

m_death <- colMeans(X_death)
m_death
S_death <- cov(X_death)
S_death
R_death <- cor(X_death)
R_death
corrplot(R_death)

m_life <- colMeans(X_life)
m_life
S_life <- cov(X_life)
S_life
R_life <- cor(X_life)
R_life
corrplot(R_life)

###########################
# outliers
###########################

library(rrcov)
MCD_est <- CovMcd(X_quan, alpha=0.75, nsamp="deterministic")
m_MCD <- MCD_est$center
m_MCD
S_MCD <- MCD_est$cov
S_MCD
R_MCD <- cov2cor(S_MCD)
R_MCD

##################################################################################################################
# Compare eigenvalues of both covariance matrices

eval_S <- eigen(S)$values
eval_S_MCD <- eigen(S_MCD)$values

min_y <- min(cbind(eval_S,eval_S_MCD)) - 1
max_y <- max(cbind(eval_S,eval_S_MCD)) + 1

plot(1:8,
     eval_S,
     col=color_1,
     type="b",
     xlab="Number",
     ylab="Eigenvalues",
     pch=19,
     ylim=c(min_y,max_y),
     main="Comparison of eigenvalues")
points(1:8,
       eval_S_MCD,
       col=color_2,
       type="b",
       pch=19)
legend(5,15,
       legend=c("Eigenvalues of S","Eigenvalues of S MCD"),
       col=c(color_1,color_2),
       lty=1,
       cex=1.2)


# no changes
par(mfrow=c(1,2))
corrplot(R)
corrplot(R_MCD)
par(mfrow=c(1,1))

n <- nrow(X_quan)
p <- ncol(X_quan)
X_sq_Mah_MCD <- MCD_est$mah
col_outliers_Mah_MCD <- rep(color_2,n)
outliers_Mah_MCD <- which(X_sq_Mah_MCD>qchisq(.99,p))
outliers_Mah_MCD
col_outliers_Mah_MCD[outliers_Mah_MCD] <- color_3

table(col_outliers_Mah_MCD)

par(mfrow=c(1,2))
plot(1:n,
     X_sq_Mah_MCD,
     pch=19,
     col=col_outliers_Mah_MCD,
     main="Squared Mahalanobis distances",
     xlab="Observation",
     ylab="Squared Mahalanobis distance")
abline(h=qchisq(.99,p),lwd=3,col=color_1)
plot(1:n,
     log(X_sq_Mah_MCD),
     pch=19,
     col=col_outliers_Mah_MCD,
     main="Log of squared Mahalanobis distances",
     xlab="Observation",
     ylab="Log of squared Mahalanobis distance")
abline(h=log(qchisq(.99,p)),lwd=3,col=color_1)
par(mfrow=c(1,1))



# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/pairs_outlier_ignre.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
pairs(X_quan,pch=19,col=col_outliers_Mah_MCD)

# Step 3: Run dev.off() to create the file!
dev.off()



# Step 1: Call the pdf command to start the plot
pdf(file = "figure_output/pcp_outlier_ignre.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
parcoord(X_quan,
         col=col_outliers_Mah_MCD,
         var.label=TRUE,
         main="PCP for individual in Crash2 Trial")

# Step 3: Run dev.off() to create the file!
dev.off()




library(MASS)
parcoord(X,col=col_outliers_Mah_MCD,var.label=TRUE,main="PCP for milk contents")
