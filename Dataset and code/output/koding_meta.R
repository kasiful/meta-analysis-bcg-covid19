# hasil 1
# setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding\\hasil_1")

# hasil 2
# setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding\\hasil_2a")
# setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding\\hasil_2b")
# setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding\\hasil_2c")

# hasil 3
# setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding\\hasil_3a")
# setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding\\hasil_3b")


setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding")

# 4.2.3 Binary Outcomes

library(dmetar)
library(tidyverse)
library(meta)

# data(DepressionMortality)
# glimpse(DepressionMortality)

library(readxl)
dataset_meta_1 <- read_excel("dataset_meta_v3.xlsx", sheet = "data_1")
dataset_meta_2a <- read_excel("dataset_meta_v3.xlsx", sheet = "data_2a")
dataset_meta_2b <- read_excel("dataset_meta_v3.xlsx", sheet = "data_2b")
dataset_meta_2c <- read_excel("dataset_meta_v3.xlsx", sheet = "data_2c")

# dataset_meta <- dataset_meta[1:7,]
# View(dataset_meta)

m.bin_1 <- metabin(event.e = event.e, 
                 n.e = n.e,
                 event.c = event.c,
                 n.c = n.c,
                 studlab = author,
                 data = dataset_meta_1,
                 sm = "RR",
                 method = "MH",
                 MH.exact = TRUE,
                 fixed = TRUE,
                 random = FALSE,
                 method.tau = "PM",
                 hakn = TRUE,
                 level = 0.95,
                 title = "Fixed Effect Model - BCG Vaccines for Covid-19 - Infeksi")
summary(m.bin_1)

m.bin_update_1 <- update.meta(m.bin_1,
                            level = 0.95,
                            method.tau = "REML")

exp(m.bin_update_1$TE.random)

m.bin_update_1$tau2

m.bin_or_1 <- update.meta(m.bin_1, 
                        level = 0.95,
                        sm = "OR")
summary(m.bin_or_1)

sink("output-meta_1.txt")
print(summary(m.bin_or_1))
sink()  # returns output to the console


png(file="forest_1.png", width=800, height=350)
forest.meta(m.bin_or_1, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))
dev.off()


png(file="drapery_1.png", width=350, height=350)
drapery(m.bin_or_1, 
        labels = "studlab",
        type = "pval", 
        legend = FALSE)
dev.off()

png(file="funnel_1.png", width=350, height=350)
funnel.meta(m.bin_or_1)
dev.off()


sink("output-eggers_1.txt")
print(eggers.test(m.bin_or_1))
sink()  # returns output to the console



#####################################

m.bin_2a <- metabin(event.e = event.e, 
                    n.e = n.e,
                    event.c = event.c,
                    n.c = n.c,
                    studlab = author,
                    data = dataset_meta_2a,
                    sm = "RR",
                    method = "MH",
                    MH.exact = TRUE,
                    fixed = TRUE,
                    random = FALSE,
                    method.tau = "PM",
                    hakn = TRUE,
                    level = 0.95,
                    title = "Fixed Effect Model - BCG Vaccines for Covid-19 - Rumah Sakit")
summary(m.bin_2a)

m.bin_update_2a <- update.meta(m.bin_2a,
                               level = 0.95,
                               method.tau = "REML")

exp(m.bin_update_2a$TE.random)

m.bin_update_2a$tau2

m.bin_or_2a <- update.meta(m.bin_2a, 
                           level = 0.95,
                           sm = "OR")
summary(m.bin_or_2a)

sink("output-meta_2a.txt")
print(summary(m.bin_or_2a))
sink()  # returns output to the console


png(file="forest_2a.png", width=800, height=350)
forest.meta(m.bin_or_2a, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))
dev.off()


png(file="drapery_2a.png", width=350, height=350)
drapery(m.bin_or_2a, 
        labels = "studlab",
        type = "pval", 
        legend = FALSE)
dev.off()

png(file="funnel_2a.png", width=350, height=350)
funnel.meta(m.bin_or_2a)
dev.off()


sink("output-eggers_2a.txt")
print(eggers.test(m.bin_or_2a))
sink()  # returns output to the console

############################

m.bin_2b <- metabin(event.e = event.e, 
                    n.e = n.e,
                    event.c = event.c,
                    n.c = n.c,
                    studlab = author,
                    data = dataset_meta_2b,
                    sm = "RR",
                    method = "MH",
                    MH.exact = TRUE,
                    fixed = TRUE,
                    random = FALSE,
                    method.tau = "PM",
                    hakn = TRUE,
                    level = 0.95,
                    title = "Fixed Effect Model - BCG Vaccines for Covid-19 - ICU")
summary(m.bin_2b)

m.bin_update_2b <- update.meta(m.bin_2b,
                               level = 0.95,
                               method.tau = "REML")

exp(m.bin_update_2b$TE.random)

m.bin_update_2b$tau2

m.bin_or_2b <- update.meta(m.bin_2b, 
                           level = 0.95,
                           sm = "OR")
summary(m.bin_or_2b)

sink("output-meta_2b.txt")
print(summary(m.bin_or_2b))
sink()  # returns output to the console


png(file="forest_2b.png", width=800, height=350)
forest.meta(m.bin_or_2b, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))
dev.off()


png(file="drapery_2b.png", width=350, height=350)
drapery(m.bin_or_2b, 
        labels = "studlab",
        type = "pval", 
        legend = FALSE)
dev.off()

png(file="funnel_2b.png", width=350, height=350)
funnel.meta(m.bin_or_2b)
dev.off()


sink("output-eggers_2b.txt")
print(eggers.test(m.bin_or_2b))
sink()  # returns output to the console


#############################


m.bin_2c <- metabin(event.e = event.e, 
                    n.e = n.e,
                    event.c = event.c,
                    n.c = n.c,
                    studlab = author,
                    data = dataset_meta_2c,
                    sm = "RR",
                    method = "MH",
                    MH.exact = TRUE,
                    fixed = TRUE,
                    random = FALSE,
                    method.tau = "PM",
                    hakn = TRUE,
                    level = 0.95,
                    title = "Fixed Effect Model - BCG Vaccines for Covid-19 - Meninggal")
summary(m.bin_2c)

m.bin_update_2c <- update.meta(m.bin_2c,
                               level = 0.95,
                               method.tau = "REML")

exp(m.bin_update_2c$TE.random)

m.bin_update_2c$tau2

m.bin_or_2c <- update.meta(m.bin_2c, 
                           level = 0.95,
                           sm = "OR")
summary(m.bin_or_2c)

sink("output-meta_2c.txt")
print(summary(m.bin_or_2c))
sink()  # returns output to the console


png(file="forest_2c.png", width=800, height=350)
forest.meta(m.bin_or_2c, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))
dev.off()


png(file="drapery_2c.png", width=350, height=350)
drapery(m.bin_or_2c, 
        labels = "studlab",
        type = "pval", 
        legend = FALSE)
dev.off()

png(file="funnel_2c.png", width=350, height=350)
funnel.meta(m.bin_or_2c)
dev.off()


sink("output-eggers_2c.txt")
print(eggers.test(m.bin_or_2c))
sink()  # returns output to the console


sink("output-meta_all.txt")
print(summary(m.bin_or_1))
print("")
print("==============")
print("")
print(summary(m.bin_or_2a))
print("")
print("==============")
print("")
print(summary(m.bin_or_2b))
print("")
print("==============")
print("")
print(summary(m.bin_or_2c))
sink()  # returns output to the console


sink("output-eggers_all.txt")
print(eggers.test(m.bin_or_1))
print("")
print("==============")
print("")
print(eggers.test(m.bin_or_2a))
print("")
print("==============")
print("")
print(eggers.test(m.bin_or_2b))
print("")
print("==============")
print("")
print(eggers.test(m.bin_or_2c))
sink()  # returns output to the console
