
setwd('C:\\Users\\plainwhite\\Documents\\Statistik\\Tugas Koding R\\P3 dan P4')


# 4.1.1 The Fixed-Effect Model

# Load dmetar, esc and tidyverse (for pipe)
library(dmetar)
library(esc)
library(tidyverse)

# Load data set from dmetar
data(SuicidePrevention)

# Calculate Hedges' g and the Standard Error
# - We save the study names in "study".
# - We use the pmap_dfr function to calculate the effect size
#   for each row.
SP_calc <- pmap_dfr(SuicidePrevention, 
                    function(mean.e, sd.e, n.e, mean.c,
                             sd.c, n.c, author, ...){
                      esc_mean_sd(grp1m = mean.e,
                                  grp1sd = sd.e,
                                  grp1n = n.e,
                                  grp2m = mean.c,
                                  grp2sd = sd.c,
                                  grp2n = n.c,
                                  study = author,
                                  es.type = "g") %>% 
                        as.data.frame()}) 

# Let us catch a glimpse of the data
# The data set contains Hedges' g ("es") and standard error ("se")
glimpse(SP_calc)

# Calculate the inverse variance-weights for each study
SP_calc$w <- 1/SP_calc$se^2

# Then, we use the weights to calculate the pooled effect
pooled_effect <- sum(SP_calc$w*SP_calc$es)/sum(SP_calc$w)
pooled_effect



# 4.2 Effect Size Pooling in R
## 4.2.1 Pre-Calculated Effect Size Data

library(tidyverse) # needed for 'glimpse'
library(dmetar)
library(meta)

data(ThirdWave)
glimpse(ThirdWave)

m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

summary(m.gen)

m.gen$TE.random

m.gen$TE.fixed

m.gen_update <- update.meta(m.gen, 
                            method.tau = "PM")

# Get pooled effect
m.gen_update$TE.random

# Get tau^2 estimate
m.gen_update$tau2

save(m.gen, file = "meta-analysis.rda") # example path



# 4.2.2 (Standardized) Mean Differences

# Make sure meta and dmetar are already loaded
library(meta)
library(dmetar)
library(meta)

# Load dataset from dmetar (or download and open manually)
data(SuicidePrevention)

# Use metcont to pool results.
m.cont <- metacont(n.e = n.e,
                   mean.e = mean.e,
                   sd.e = sd.e,
                   n.c = n.c,
                   mean.c = mean.c,
                   sd.c = sd.c,
                   studlab = author,
                   data = SuicidePrevention,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Suicide Prevention")

summary(m.cont)




# 4.2.3 Binary Outcomes

library(dmetar)
library(tidyverse)
library(meta)

data(DepressionMortality)
glimpse(DepressionMortality)

m.bin <- metabin(event.e = event.e, 
                 n.e = n.e,
                 event.c = event.c,
                 n.c = n.c,
                 studlab = author,
                 data = DepressionMortality,
                 sm = "RR",
                 method = "MH",
                 MH.exact = TRUE,
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "PM",
                 hakn = TRUE,
                 title = "Depression and Mortality")
summary(m.bin)

m.bin_update <- update.meta(m.bin, 
                            method.tau = "REML")

exp(m.bin_update$TE.random)

m.bin_update$tau2

m.bin_or <- update.meta(m.bin, sm = "OR")
m.bin_or

# 4.2.3.1.5 Pooling Pre-Calculated Binary Effect Sizes

DepressionMortality$TE <- m.bin$TE
DepressionMortality$seTE <- m.bin$seTE

# Set seTE of study 7 to NA
DepressionMortality$seTE[7] <- NA

# Create empty columns 'lower' and 'upper'
DepressionMortality[,"lower"] <- NA
DepressionMortality[,"upper"] <- NA

# Fill in values for 'lower' and 'upper' in study 7
# As always, binary effect sizes need to be log-transformed
DepressionMortality$lower[7] <- log(1.26)
DepressionMortality$upper[7] <- log(2.46)

DepressionMortality[,c("author", "TE", "seTE", "lower", "upper")]

m.gen_bin <- metagen(TE = TE,
                     seTE = seTE,
                     lower = lower,
                     upper = upper,
                     studlab = author,
                     data = DepressionMortality,
                     sm = "RR",
                     method.tau = "PM",
                     fixed = FALSE,
                     random = TRUE,
                     title = "Depression Mortality (Pre-calculated)")

summary(m.gen_bin)


# 4.2.3.2 Incidence Rate Ratios

library(dmetar)
library(tidyverse)
library(meta)

data(EatingDisorderPrevention)

glimpse(EatingDisorderPrevention)

m.inc <- metainc(event.e = event.e, 
                 time.e = time.e,
                 event.c = event.c,
                 time.c = time.c,
                 studlab = Author,
                 data = EatingDisorderPrevention,
                 sm = "IRR",
                 method = "MH",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "PM",
                 hakn = TRUE,
                 title = "Eating Disorder Prevention")

summary(m.inc)



# 4.2.4 Correlations

library(dmetar)
library(tidyverse)
library(meta)

data(HealthWellbeing)
glimpse(HealthWellbeing)

m.cor <- metacor(cor = cor, 
                 n = n,
                 studlab = author,
                 data = HealthWellbeing,
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Health and Wellbeing")
summary(m.cor)



# 4.2.5 Means

library(dmetar)
library(tidyverse)
library(meta)
data(BdiScores)

# We only need the first four columns
glimpse(BdiScores[,1:4])

m.mean <- metamean(n = n,
                   mean = mean,
                   sd = sd,
                   studlab = author,
                   data = BdiScores,
                   sm = "MRAW",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "BDI-II Scores")
summary(m.mean)



# 4.2.6 Proportions

library(dmetar)
library(meta)
library(tidyverse)

data(OpioidMisuse)
glimpse(OpioidMisuse)

m.prop <- metaprop(event = event,
                   n = n,
                   studlab = author,
                   data = OpioidMisuse,
                   method = "GLMM",
                   sm = "PLOGIT",
                   fixed = FALSE,
                   random = TRUE,
                   hakn = TRUE,
                   title = "Opioid Misuse")
summary(m.prop)

