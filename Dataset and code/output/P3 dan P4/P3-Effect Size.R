set.seed(123)
sample <- rnorm(n = 50, mean = 10, sd = 2)

mean(sample)

library(dplyr)

sample %>% hist()

# menghitung standar error
sd(sample)/sqrt(50)

#####################################################
# 3.2 Measures & Effect Sizes in Single Group Designs
#####################################################

# 3.2.1 MEAN

set.seed(123)
sample <- rnorm(n = 50, mean = 20, sd = 5)

# Calculate the mean
mean(sample)

# Calculate the standard error
sd(sample)/sqrt(50)

###########################################

# 3.2.2 Proportions

# We define the following values for k and n:
k <- 25
n <- 125

# Calculate the proportion
p <- k/n
p

# Calculate the standard error
sqrt((p*(1-p))/n)

############################################
# 3.2.3 Correlations

# 3.2.3.1 Pearson Product-Moment Correlation

# Simulate two continuous variables x and y
set.seed(12345)
x <- rnorm(20, 50, 10)
y <- rnorm(20, 10, 3)

# Calculate the correlation between x and y
r <- cor(x,y)
r

# Calculate Fisher's z
z <- 0.5*log((1+r)/(1-r))
z


############################################
# 3.3 Effect Sizes in Control Group Designs
############################################

# 3.3.1 (Standardized) Mean Differences

# 3.3.1.1 Between-Group Mean Difference

# Generate two random variables with different population means
set.seed(123)
x1 <- rnorm(n = 20, mean = 10, sd = 3)
x2 <- rnorm(n = 20, mean = 15, sd = 3)

# Calculate values we need for the formulas
s1 <- sd(x1)
s2 <- sd(x2)
n1 <- 20
n2 <- 20

# Calculate the mean difference
MD <- mean(x1) - mean(x2)
MD

# Calculate s_pooled
s_pooled <- sqrt(
  (((n1-1)*s1^2) + ((n2-1)*s2^2))/
    ((n1-1)+(n2-1))
)

# Calculate the standard error
se <- s_pooled*sqrt((1/n1)+(1/n2))
se


# 3.3.1.2 Between-Group Standardized Mean Difference

# Load esc package
library(esc)

# Define the data we need to calculate SMD/d
# This is just some example data that we made up
grp1m <- 50   # mean of group 1
grp2m <- 60   # mean of group 2
grp1sd <- 10  # sd of group 1
grp2sd <- 10  # sd of group 2
grp1n <- 100  # n of group1
grp2n <- 100  # n of group2

# Calculate effect size
esc_mean_sd(grp1m = grp1m, grp2m = grp2m, 
            grp1sd = grp1sd, grp2sd = grp2sd, 
            grp1n = grp1n, grp2n = grp2n)

# 3.3.1.3 Within-Group (Standardized) Mean Difference

# Define example data needed for effect size calculation
x1 <- 20    # mean at t1
x2 <- 30    # mean at t2
sd1 <- 13   # sd at t1
n <- 80     # sample size
r <- 0.5    # correlation between t1 and t2

# Caclulate the raw mean difference
md_within <- x2 - x1

# Calculate the smd:
# Here, we use the standard deviation at t1
# to standardize the mean difference
smd_within <- md_within/sd1
smd_within

# Calculate standard error
se_within <- sqrt(((2*(1-r))/n) + 
                    (smd_within^2/(2*n)))
se_within



# 3.3.2 Risk & Odds Ratios

# 3.3.2.1 Risk Ratio

# Define data
a <- 46         # events in the treatment group
c <- 77         # events in the control group
n_treat <- 248  # sample size treatment group
n_contr <- 251  # sample size control group

# Calculate the risks
p_treat <- a/n_treat
p_contr <- c/n_contr

# Calculate the risk ratio
rr <- p_treat/p_contr
rr

# Calculate the log-risk ratio and its standard error
log_rr <- log(rr)
log_rr

se_log_rr <- sqrt((1/a) + (1/c) - (1/n_treat) - (1/n_contr))
se_log_rr


# 3.3.2.2 Odds Ratio

library(esc)

# Define data
grp1yes <- 45  # events in the treatment group
grp1no <- 98   # non-events in the treatment group
grp2yes <- 67  # events in the control group
grp2no <- 76   # non-events in the control group

# Calculate OR by setting es.type to "or"
esc_2x2(grp1yes = grp1yes, grp1no = grp1no,
        grp2yes = grp2yes, grp2no = grp2no,
        es.type = "or")

# Calculate logOR by setting es.type to "logit"
esc_2x2(grp1yes = grp1yes, grp1no = grp1no,
        grp2yes = grp2yes, grp2no = grp2no,
        es.type = "logit")


# 3.3.3 Incidence Rate Ratios

# Define Data
e_treat <- 28    # Number of events in the treatment group
e_contr <- 28    # Number of events in the control group
t_treat <- 3025  # Person-time in the treatment group
t_contr <- 2380  # Person-time in the control group

# Calculate IRR
irr <- (e_treat/t_treat)/(e_contr/t_contr)
irr

# Calculate log-IRR
log_irr <- log(irr)

# Calculate standard error
se_log_irr <- sqrt((1/e_treat)+(1/e_contr))


###############################################
# 3.4 Effect Size Correction
###############################################

# 3.4.1 Small Sample Bias

# Load esc package
library(esc)

# Define uncorrected SMD and sample size n
SMD <- 0.5
n <- 30

# Convert to Hedges g
g <- hedges_g(SMD, n)
g


# 3.4.2 Unreliability

# Define uncorrected correlation and SMD with their standard error
r_xy <- 0.34
se_r_xy <- 0.09
smd <- 0.65
se_smd <- 0.18

# Define reliabilities of x and y
r_xx <- 0.8
r_yy <- 0.7

# Correct SMD for unreliability in x
smd_c <- smd/sqrt(r_xx)
smd_c

se_c <- se_smd/sqrt(r_xx)
se_c

# Correct correlation for unreliability in x and y
r_xy_c <- r_xy/(sqrt(r_xx)*sqrt(r_yy))
r_xy_c

se_c <- se_r_xy/(sqrt(r_xx)*sqrt(r_yy))
se_c



# 3.4.3 Range Restriction

# Define correlation to correct
r_xy <- 0.34
se_r_xy <- 0.09

# Define restricted and unrestricted SD
sd_restricted <- 11
sd_unrestricted <- 18

# Calculate U
U <- sd_unrestricted/sd_restricted

# Correct the correlation
r_xy_c <- (U*r_xy)/sqrt((U^2-1)*r_xy^2+1)
r_xy_c

# Correct the standard error
se_r_xy_c <- (r_xy_c/r_xy)*se_r_xy
se_r_xy_c

###############################################
# 3.5 Common Problems
###############################################





