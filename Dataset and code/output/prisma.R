setwd("D:\\GDrive\\Materi Kuliah S2\\Tugas Statistik\\Statistik\\Dataset dan koding")

data1 <- read.csv("./PRISMA/format_csv/crossref.csv")
data2 <- read.csv("./PRISMA/format_csv/google_scholar.csv")
data3 <- read.csv("./PRISMA/format_csv/openalex.csv")
data4 <- read.csv("./PRISMA/format_csv/pubmed.csv")
data5 <- read.csv("./PRISMA/format_csv/scopus.csv")

# data3_1 <- read.csv("./openalex_1.csv")
# data3_2 <- read.csv("./openalex_2.csv")
# data3_3 <- read.csv("./openalex_3.csv")
# data3 <- rbind(data3_1, data3_2, data3_3)

library(dplyr)

# tiap sumber data perlu di filter lagi hapus yang duplikat
# caranya sort file berdasarkan judul
# di urut per title, lalu ambil baris pertama
# begitu juga dengan DOI

data1 <- data1 %>%
  group_by(Title) %>%
  arrange(Title) %>%
  filter(row_number()==1) %>% 
  group_by(DOI) %>%
  arrange(DOI) %>%
  filter(row_number()==1)

data2 <- data2 %>%
  group_by(Title) %>%
  arrange(Title) %>%
  filter(row_number()==1) %>% 
  group_by(DOI) %>%
  arrange(DOI) %>%
  filter(row_number()==1)

data3 <- data3 %>%
  group_by(Title) %>%
  arrange(Title) %>%
  filter(row_number()==1) %>% 
  group_by(DOI) %>%
  arrange(DOI) %>%
  filter(row_number()==1)

data4 <- data4 %>%
  group_by(Title) %>%
  arrange(Title) %>%
  filter(row_number()==1) %>% 
  group_by(DOI) %>%
  arrange(DOI) %>%
  filter(row_number()==1)

data5 <- data5 %>%
  group_by(Title) %>%
  arrange(Title) %>%
  filter(row_number()==1) %>% 
  group_by(DOI) %>%
  arrange(DOI) %>%
  filter(row_number()==1)


print(paste("crossref.csv", nrow(data1)))
print(paste("google_scholar.csv", nrow(data2)))
print(paste("openalex.csv", nrow(data3)))
print(paste("pubmed.csv", nrow(data4)))
print(paste("scopus.csv", nrow(data5)))


# semua data dikumpulkan ke satu variabel dataset
dset <- rbind(data1, data2, data3, data4, data5)

print(paste("total", nrow(dset)))


# filter lagi berdasarkan judul seperti diatas
dset2 <- dset %>%
  group_by(Title) %>%
  arrange(Title) %>%
  filter(row_number()==1) %>% 
  group_by(DOI) %>%
  arrange(DOI) %>%
  filter(row_number()==1)

nrow(dset) - nrow(dset2)

nrow(dset2)

# filter pencarian crossref sama google masih belum bagus, perlu dibersihkan lagi

# ("COVID" OR "SARS-CoV-2" OR "COVID-19") AND ("BCG" OR "BCG Vaccine" OR "Mycobacterium Bovis") AND ("Randomized Controlled Trials" OR "RCT" OR "Odd Ratio")

dset2$Title <- tolower(dset2$Title)
dset2$Abstract.Note <- tolower(dset2$Abstract.Note)

dset3 <- dset2 %>%
  mutate(judul_abstrak = paste(Title, Abstract.Note)) %>%
  # mutate(judul_abstrak = Title) %>%
  filter(grepl("covid|sars-cov-2|covid-19|vpm1002", judul_abstrak)) %>%
  filter(grepl("bcg|mycobacterium bovis|bacillus calmette", judul_abstrak)) %>%
  filter(grepl("randomized controlled trials|rct|odd ratio|hazard ratio|hr|cohort", judul_abstrak))

nrow(dset3)

dset3 %>% write.csv("./PRISMA/dset3.csv")
