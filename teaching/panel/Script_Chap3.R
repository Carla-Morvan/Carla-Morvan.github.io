# Application R chapitre 3 --- Inférence statistique avec des modèles de panel

# ---- 1. les packages nécessaires --------
library(tidyverse)
library(plm)
library(fixest)
library(sandwich)
library(lmtest)

# ---- 2. reconstruire la base (identique au chapitre 2) -----------
pays_ocde <- c("Australia","Austria","Belgium","Canada","Chile","Colombia",
               "Czechia","Denmark","Estonia","Finland","France","Germany",
               "Greece","Hungary","Iceland","Ireland","Israel","Italy",
               "Japan","South Korea","Latvia","Lithuania","Luxembourg",
               "Mexico","Netherlands","New Zealand","Norway","Poland",
               "Portugal","Slovakia","Slovenia","Spain","Sweden",
               "Switzerland","Turkey","United Kingdom","United States")

data_depenses <- read_csv(paste0("https://ourworldindata.org/grapher/health-expenditure-",
                                 "and-financing-per-capita.csv?v=1&csvType=full&",
                                 "useColumnShortNames=false")) %>%
  rename(pays = Entity, iso3c = Code, annee = Year,
         depenses_sante = `Health expenditure per capita - Total`)

data_esp <- read_csv("https://ourworldindata.org/grapher/life-expectancy.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, esperance_vie = 4)

data_pib <- read_csv(paste0("https://ourworldindata.org/grapher/gdp-per-capita-",
                            "worldbank-constant-usd.csv?v=1&csvType=full&",
                            "useColumnShortNames=false")) %>%
  rename(pays = Entity, iso3c = Code, annee = Year, pib_hab = `GDP per capita`)

data_urbain <- read_csv("https://ourworldindata.org/grapher/share-of-population-urban.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, part_urbaine = 4)

data_pollution <- read_csv("https://ourworldindata.org/grapher/pm25-air-pollution.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, pm25 = 4)

data_pop <- read_csv("https://ourworldindata.org/grapher/population-unwpp.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, population = 4)

data_vieux <- read_csv("https://ourworldindata.org/grapher/age-dependency-ratio-old.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, ratio_dependance_agee = 4)

data_complete <- data_depenses %>%
  left_join(data_esp, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_pib, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_urbain, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_pollution, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_pop, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_vieux, by = c("pays", "iso3c", "annee")) %>%
  filter(pays %in% pays_ocde, annee >= 2000, annee <= 2023)

pdata <- pdata.frame(data_complete, index = c("pays", "annee"))
pdim(pdata)


# ---- 3. reprendre le modèle à effets fixes du chapitre 2 -----------
fe_plm <- plm(esperance_vie ~ depenses_sante,
              data = pdata, model = "within")
summary(fe_plm)   # erreurs standards NON corrigées, affichées par défaut


# ---- 4. erreurs standards classiques vs robustes (White) -----------

# 4.1 Erreurs classiques (IID) -- ce que plm affiche par défaut
coeftest(fe_plm)

# 4.2 Erreurs robustes à l'hétéroscédasticité SEULE (White), sans clustering
#     Attention : par défaut, plm::vcovHC() utilise method = "arellano",
#     qui est déjà une version clusterisée ! Pour du White pur (sans
#     autocorrélation), il faut forcer method = "white1".
coeftest(fe_plm, vcov = vcovHC(fe_plm, method = "white1", type = "HC1"))


# ---- 5. erreurs standards clusterisées -----------

# 5.1 Avec plm (clustering par pays, méthode Arellano : gère
#     hétéroscédasticité ET autocorrélation intra-individu)
coeftest(fe_plm, vcov = vcovHC(fe_plm, method = "arellano", cluster = "group"))

# 5.2 Avec fixest : clustering automatique dès qu'il y a un effet fixe
fe_fixest <- feols(esperance_vie ~ depenses_sante | pays,
                   data = data_complete)
summary(fe_fixest)   # déjà clusterisé par pays par défaut !

# 5.3 Pour comparer avec des erreurs IID (non corrigées) sous fixest
fe_fixest_iid <- feols(esperance_vie ~ depenses_sante | pays,
                       data = data_complete, vcov = "iid")
summary(fe_fixest_iid)


# ---- 6. clustering multi-way (pays ET année) -----------

fe_fixest_twoway <- feols(esperance_vie ~ depenses_sante | pays,
                          data = data_complete, vcov = ~pays + annee)
summary(fe_fixest_twoway)


# ---- 7. comparer toutes les versions d'un coup d'oeil -----------

etable(fe_fixest_iid, fe_fixest, fe_fixest_twoway,
       headers = c("IID", "Cluster: pays", "Cluster: pays & annee"))

