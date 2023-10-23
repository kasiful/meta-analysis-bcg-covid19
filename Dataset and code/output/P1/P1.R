
setwd('C:\\Users\\plainwhite\\Documents\\Statistik\\Tugas Koding R\\P1')

# daftar library ubuntu yang harus di install sebelum install package dari R
# sudo apt-get install cmake
# sudo apt-get install libfontconfig1-dev libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev 
# sudo apt-gete install r-cran-rcppeigen r-cran-lme4

install.packages('devtools')
install.packages("tidyverse")
install.packages("meta")
install.packages("metafor")

library(devtools)
library(tidyverse)
library(meta)
library(metafor)

R.Version()$version.string
# install.packages("devtools")

# sudo apt-get install r-base-dev
# sudo apt install jags
# sudo apt-get install libglpk-dev

# install.packages("rjags")

library(devtools)
# devtools::install_github("MathiasHarrer/dmetar", dependencies = TRUE)
# install.packages("openxlsx")

library(openxlsx)
SuicidePrevention <- read.xlsx("./dataset/SuicidePrevention.xlsx")

# Class Conversion
library(dplyr)
glimpse(SuicidePrevention)
SuicidePrevention$n.e
class(SuicidePrevention$n.e)

mean(SuicidePrevention$n.e)
SuicidePrevention$n.e <- as.numeric(SuicidePrevention$n.e)
SuicidePrevention$mean.e <- as.numeric(SuicidePrevention$mean.e)
SuicidePrevention$sd.e <- as.numeric(SuicidePrevention$sd.e)
SuicidePrevention$n.c <- as.numeric(SuicidePrevention$n.c)
SuicidePrevention$mean.c <- as.numeric(SuicidePrevention$mean.c)
SuicidePrevention$sd.c <- as.numeric(SuicidePrevention$sd.c)
SuicidePrevention$n.c <- as.numeric(SuicidePrevention$n.c)

SuicidePrevention$age_group <- as.factor(SuicidePrevention$age_group)
SuicidePrevention$control <- as.factor(SuicidePrevention$control)

levels(SuicidePrevention$age_group)
nlevels(SuicidePrevention$age_group)

new.factor.levels <- c("gen", "older")
new.factor.levels

levels(SuicidePrevention$age_group) <- new.factor.levels

SuicidePrevention$age_group
SuicidePrevention$pubyear
as.logical(SuicidePrevention$pubyear >= 2010)

 # Data Slicing

SuicidePrevention[2,]
SuicidePrevention[2, 1]
SuicidePrevention[2, 1]
SuicidePrevention[c(2,3), c(4,6)]
SuicidePrevention[, c("author", "control")]
filter(SuicidePrevention, n.e <= 50)
filter(SuicidePrevention, author %in% c("Meijer et al.",
                                        "Zaytsev et al."))
filter(SuicidePrevention, !author %in% c("Meijer et al.", 
                                         "Zaytsev et al."))

# Data Transformation

SuicidePrevention[2, "pubyear"] <- 2018
SuicidePrevention[2, "pubyear"]

SuicidePrevention$mean.e
SuicidePrevention$mean.e + 5

SuicidePrevention$mean.e - SuicidePrevention$mean.c
md <- SuicidePrevention$mean.e - SuicidePrevention$mean.c

SuicidePrevention$md <- SuicidePrevention$mean.e - SuicidePrevention$mean.c
SuicidePrevention$n.c %>% mean()
SuicidePrevention %>% 
  filter(pubyear > 2009) %>% 
  pull(n.c) %>% 
  mean() %>% 
  sqrt()

 # Saving Data

save(SuicidePrevention, file = "suicideprevention.rda")
write.csv(SuicidePrevention, file = "suicideprevention.csv")
