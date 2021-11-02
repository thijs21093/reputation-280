library(tm.plugin.factiva)
library(tm)
library(purrr)
library(tidytext)
library(dplyr)
library(stringr)

# All 
files.list <- list.files(full.names = TRUE) # List of paths
html <- str_extract(files.list, "\\w*\\.html") # List of htmls
files <- html[!is.na(html)] # Creates a list of all valid files in folder
path <- str_c(getwd(), "/", files) # Construct paths

# Loading each agency separately

# ACER
acer <- str_c(getwd(), 
                   "/",
                   str_subset(files, "acer[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ACER")

# BEREC OFFICE
berec <- str_c(getwd(), 
              "/",
              str_subset(files, "berec[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "BEREC")

# CEDEFOP
cedefop <- str_c(getwd(), 
               "/",
               str_subset(files, "cedefop[:digit:]")) %>% # Path to htmls
  map(FactivaSource) %>% 
  map(Corpus, readerControl = list(language = NA)) %>% 
  map_dfr(tidy) %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "CEDEFOP")

# CPVO
## cpvo <- str_c(getwd(), 
##                 "/",
##                  str_subset(files, "cpvo[:digit:]")) %>% # Path to htmls
##   map(FactivaSource) %>% 
## map(Corpus, readerControl = list(language = NA)) %>% 
##  map_dfr(tidy) %>% 
##  mutate(date = as.POSIXct(datetimestamp),
##         acronym = "CPVO")

# INCORRECT: data needs not be dowloaded again

easa1 <- FactivaSource("./EASA/EASA1.html")
easa2 <- FactivaSource("./EASA/EASA2.html")
easa3 <- FactivaSource("./EASA/EASA3.html")

easa.corpus1 <- Corpus(easa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EASA")

easa.corpus2 <- Corpus(easa2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EASA")

easa.corpus3 <- Corpus(easa3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EASA")

easa.total <- rbind(easa.corpus1, easa.corpus2, easa.corpus3)

easo1 <- FactivaSource("./EASO/EASO.html")
easo.total <- Corpus(easo1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EASO")

eba1 <- FactivaSource("./EBA/EBA1.html")
eba2 <- FactivaSource("./EBA/EBA2.html")
eba3 <- FactivaSource("./EBA/EBA3.html")
eba4 <- FactivaSource("./EBA/EBA3.html")

eba.corpus1 <- Corpus(eba1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EBA")

eba.corpus2 <- Corpus(eba2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EBA")

eba.corpus3 <- Corpus(eba3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EBA")

eba.corpus4 <- Corpus(eba4, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EBA")

eba.total <- rbind(eba.corpus1, eba.corpus2, eba.corpus3, eba.corpus4)

ecdc1 <- FactivaSource("./ECDC/ECDC1.html")
ecdc2 <- FactivaSource("./ECDC/ECDC2.html")

ecdc.corpus1 <- Corpus(ecdc1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ECDC") 

ecdc.corpus2 <- Corpus(ecdc2, readerControl = list(language = NA))%>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ECDC") 

ecdc.total <- rbind(ecdc.corpus1, ecdc.corpus2)

echa1 <- FactivaSource("./ECHA/ECHA.html")
echa.total <- Corpus(echa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ECHA") 

ecsel.ju1 <- FactivaSource("./ECSEL/ECSEL JU.html")
ecsel.ju.total <- Corpus(ecsel.ju1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ECSEL JU") 

eda1 <- FactivaSource("./EDA/EDA.html")
eda.total <- Corpus(eda1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EDA")

eea1 <- FactivaSource("./EEA/EEA1.html")
eea2 <- FactivaSource("./EEA/EEA2.html")

eea.corpus1 <- Corpus(eea1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EEA") 

eea.corpus2 <- Corpus(eea2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EEA") 

eea.total <- rbind(eea.corpus1, eea.corpus2)

efca1 <- FactivaSource("./EFCA/EFCA.html")
efca.total <- Corpus(efca1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EFCA") 

efsa1 <- FactivaSource("./EFSA/EFSA1.html")
efsa2 <- FactivaSource("./EFSA/EFSA2.html")
efsa3 <- FactivaSource("./EFSA/EFSA3.html")

efsa.corpus1 <- Corpus(efsa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EFSA") 

efsa.corpus2 <- Corpus(efsa2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EFSA") 

efsa.corpus3 <- Corpus(efsa3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EFSA") 

efsa.total <- rbind(efsa.corpus1, efsa.corpus2, efsa.corpus3)

eige1 <- FactivaSource("./EIGE/EIGE.html")
eige.total <- Corpus(eige1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EIGE") 

eiopa1 <- FactivaSource("./EIOPA/EIOPA.html")
eiopa.total <- Corpus(eiopa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EIOPA") 

eit1 <- FactivaSource("./EIT/EIT.html")
eit.total <- Corpus(eit1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EIT") 

ema1 <- FactivaSource("./EMA/EMA1.html")
ema2 <- FactivaSource("./EMA/EMA2.html")
ema3 <- FactivaSource("./EMA/EMA3.html")
ema4 <- FactivaSource("./EMA/EMA4.html")
ema5 <- FactivaSource("./EMA/EMA5.html")
ema6 <- FactivaSource("./EMA/EMA6.html")
ema7 <- FactivaSource("./EMA/EMA7.html")

ema.corpus1 <- Corpus(ema1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.corpus2 <- Corpus(ema2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.corpus3 <- Corpus(ema3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.corpus4 <- Corpus(ema4, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.corpus5 <- Corpus(ema5, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.corpus6 <- Corpus(ema6, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.corpus7 <- Corpus(ema7, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMA") 

ema.total <- rbind(ema.corpus1,
                   ema.corpus2,
                   ema.corpus3,
                   ema.corpus4,
                   ema.corpus5,
                   ema.corpus6,
                   ema.corpus7)

emcdda1 <- FactivaSource("./EMCDDA/EMCDDA.html")
emcdda.total <- Corpus(emcdda1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMCDDA") 

emsa1 <- FactivaSource("./EMSA/EMSA.html")
emsa.total <- Corpus(emsa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EMSA") 

enisa1 <- FactivaSource("./ENISA/ENISA.html")
enisa.total <- Corpus(enisa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ENISA") 

era1 <- FactivaSource("./ERA/ERA.html")
era.total <- Corpus(era1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ERA") 

esma1 <- FactivaSource("./ESMA/ESMA1.html")
esma2 <- FactivaSource("./ESMA/ESMA2.html")

esma.corpus1 <- Corpus(esma1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ESMA") 

esma.corpus2 <- Corpus(esma2, readerControl = list(language = NA))%>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ESMA") 

esma.total <- rbind(esma.corpus1, esma.corpus2)

etf1 <- FactivaSource("./ETF/ETF.html")
etf.total <- Corpus(etf1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "ETF") 


euipo1 <- FactivaSource("./EUIPO/EUIPO.html")
euipo.total <- Corpus(euipo1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUIPO")

euiss1 <- FactivaSource("./EUISS/EUISS.html")
euiss.total <- Corpus(euiss1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUISS")

# EU-LISA, no hits

eurofound1 <- FactivaSource("./EUROFOUND/EUROFOUND.html")
eurofound.total <- Corpus(eurofound1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROFOUND") 

eurojust1 <- FactivaSource("./EUROJUST/EUROJUST1.html")
eurojust2 <- FactivaSource("./EUROJUST/EUROJUST2.html")
eurojust3 <- FactivaSource("./EUROJUST/EUROJUST3.html")

eurojust.corpus1 <- Corpus(eurojust1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROJUST") 

eurojust.corpus2 <- Corpus(eurojust2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROJUST") 

eurojust.corpus3 <- Corpus(eurojust3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROJUST") 

eurojust.total <- rbind(eurojust.corpus1, eurojust.corpus2, eurojust.corpus3)

europol1 <- FactivaSource("./EUROPOL/EUROPOL1.html")
europol2 <- FactivaSource("./EUROPOL/EUROPOL2.html")
europol3 <- FactivaSource("./EUROPOL/EUROPOL3.html")
europol4 <- FactivaSource("./EUROPOL/EUROPOL4.html")
europol5 <- FactivaSource("./EUROPOL/EUROPOL5.html")
europol6 <- FactivaSource("./EUROPOL/EUROPOL6.html")
europol7 <- FactivaSource("./EUROPOL/EUROPOL7.html")
europol8 <- FactivaSource("./EUROPOL/EUROPOL8.html")
europol9 <- FactivaSource("./EUROPOL/EUROPOL9.html")
europol10 <- FactivaSource("./EUROPOL/EUROPOL10.html")

europol.corpus1 <- Corpus(europol1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus2 <- Corpus(europol2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus3 <- Corpus(europol3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus4 <- Corpus(europol4, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus5 <- Corpus(europol5, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus6 <- Corpus(europol6, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus7 <- Corpus(europol7, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus8 <- Corpus(europol8, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus9 <- Corpus(europol9, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.corpus10 <- Corpus(europol10, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EUROPOL") 

europol.total <- rbind(europol.corpus1,
                       europol.corpus2,
                       europol.corpus3,
                       europol.corpus4,
                       europol.corpus5,
                       europol.corpus6,
                       europol.corpus7,
                       europol.corpus8,
                       europol.corpus9,
                       europol.corpus10)

f4e1 <- FactivaSource("./F4E/F4E.html")
f4e.total <- Corpus(f4e1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "F4E JU") 

fch.ju1 <- FactivaSource("./FCH JU/FCH JU.html")
fch.ju.total <- Corpus(fch.ju1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FCH 2 JU") 

fra1 <- FactivaSource("./FRA/FRA.html")
fra.total <- Corpus(fra1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRA") 

frontex1 <- FactivaSource("./FRONTEX/FRONTEX1.html")
frontex2 <- FactivaSource("./FRONTEX/FRONTEX2.html")
frontex3 <- FactivaSource("./FRONTEX/FRONTEX3.html")
frontex4 <- FactivaSource("./FRONTEX/FRONTEX4.html")
frontex5 <- FactivaSource("./FRONTEX/FRONTEX5.html")
frontex6 <- FactivaSource("./FRONTEX/FRONTEX6.html")
frontex7 <- FactivaSource("./FRONTEX/FRONTEX7.html")
frontex8 <- FactivaSource("./FRONTEX/FRONTEX8.html")
frontex9 <- FactivaSource("./FRONTEX/FRONTEX9.html")
frontex10 <- FactivaSource("./FRONTEX/FRONTEX10.html")
frontex11 <- FactivaSource("./FRONTEX/FRONTEX11.html")
frontex12 <- FactivaSource("./FRONTEX/FRONTEX12.html")
frontex13 <- FactivaSource("./FRONTEX/FRONTEX13.html")

frontex.corpus1 <- Corpus(frontex1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus2 <- Corpus(frontex2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus3 <- Corpus(frontex3, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus4 <- Corpus(frontex4, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus5 <- Corpus(frontex5, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus6 <- Corpus(frontex6, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus7 <- Corpus(frontex7, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus8 <- Corpus(frontex8, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus9 <- Corpus(frontex9, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus10 <- Corpus(frontex10, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus11 <- Corpus(frontex11, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus12 <- Corpus(frontex12, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.corpus13 <- Corpus(frontex13, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "FRONTEX") 

frontex.total <- rbind(frontex.corpus1,
                       frontex.corpus2,
                       frontex.corpus3,
                       frontex.corpus4,
                       frontex.corpus5,
                       frontex.corpus6,
                       frontex.corpus7,
                       frontex.corpus8,
                       frontex.corpus9,
                       frontex.corpus10,
                       frontex.corpus11,
                       frontex.corpus12,
                       frontex.corpus13)

gsa1 <- FactivaSource("./GSA/GSA.html")
gsa.total <- Corpus(gsa1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "GSA") 


euosha1 <- FactivaSource("./EU-OSHA/EU-OSHA.html")
euosha.total <- Corpus(euosha1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "EU-OSHA") 

imi.ju1 <- FactivaSource("./IMI JU/IMI JU.html")
imi.ju.total <- Corpus(imi.ju1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "IMI JU") 


# S2R, no hits

# SatCen, no hits

sesar.ju1 <- FactivaSource("./SESAR/SESAR.html")
sesar.total <- Corpus(sesar.ju1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "SESAR JU") 

srb1 <- FactivaSource("./SRB/SRB1.html")
srb2 <- FactivaSource("./SRB/SRB2.html")

srb.corpus1 <- Corpus(srb1, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "SRB") 

srb.corpus2 <- Corpus(srb2, readerControl = list(language = NA)) %>%
  tidy() %>% 
  mutate(date = as.POSIXct(datetimestamp),
         acronym = "SRB") 

srb.total <- rbind(srb.corpus1, srb.corpus2)

media.total <- rbind(acer.total,
                     bbi.ju.total,
                     berec.total,
                     cedefop.total,
                     cepol.total,
                     cpvo.total,
                     easa.total,
                     easo.total,
                     eba.total,
                     ecdc.total,
                     echa.total,
                     ecsel.ju.total,
                     eda.total,
                     eea.total,
                     efca.total,
                     efsa.total,
                     eige.total,
                     eiopa.total,
                     eit.total,
                     ema.total,
                     emcdda.total,
                     emsa.total,
                     enisa.total,
                     era.total,
                     esma.total,
                     etf.total,
                     euipo.total,
                     euiss.total,
                     euosha.total,
                     eurofound.total,
                     eurojust.total,
                     europol.total,
                     f4e.total,
                     fch.ju.total,
                     fra.total,
                     frontex.total,
                     gsa.total,
                     imi.ju.total,
                     sesar.total,
                     srb.total)

media.2019 <- media.total %>%
  filter(date >= "2019-01-01" & date <= "2019-12-31") %>%
  dplyr::group_by(acronym) %>% 
  dplyr::summarise(media.2019 = n(), .groups = "keep")


# 2019
write.csv(media.2019, "media.csv")

# By month
media.month <- media.total %>%
  filter(date >= "2018-01-01" & date <= "2019-12-31") %>%
  dplyr::group_by(acronym, month = cut(date, "month"),  .drop = FALSE)  %>% 
  dplyr::summarise(media.salience = n(), .groups = "keep")
write.csv(media.month, "media-month.csv")
