# Application R chapitre 2

# ---- 1. les packages nécessaires --------
library(tidyverse)
library(plm)

# ---- 2. importation des données -----------
pays_ocde <- c("Australia","Austria","Belgium","Canada","Chile","Colombia",
               "Czechia","Denmark","Estonia","Finland","France","Germany",
               "Greece","Hungary","Iceland","Ireland","Israel","Italy",
               "Japan","South Korea","Latvia","Lithuania","Luxembourg",
               "Mexico","Netherlands","New Zealand","Norway","Poland",
               "Portugal","Slovakia","Slovenia","Spain","Sweden",
               "Switzerland","Turkey","United Kingdom","United States")

# Dépenses de santé
data_depenses <- read_csv(paste0("https://ourworldindata.org/grapher/health-expenditure-",
                                 "and-financing-per-capita.csv?v=1&csvType=full&",
                                 "useColumnShortNames=false")) %>%
  rename(pays = Entity, iso3c = Code, annee = Year,
         depenses_sante = `Health expenditure per capita - Total`)

# Espérance de vie
data_esp <- read_csv("https://ourworldindata.org/grapher/life-expectancy.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, esperance_vie = 4)

# PIB par habitant (contrôle)
data_pib <- read_csv(paste0("https://ourworldindata.org/grapher/gdp-per-capita-",
                            "worldbank-constant-usd.csv?v=1&csvType=full&",
                            "useColumnShortNames=false")) %>%
  rename(pays = Entity, iso3c = Code, annee = Year, pib_hab = `GDP per capita`)

# Part urbaine (contrôle)
data_urbain <- read_csv("https://ourworldindata.org/grapher/share-of-population-urban.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, part_urbaine = 4)

# Pollution PM2.5 (contrôle environnemental)
data_pollution <- read_csv("https://ourworldindata.org/grapher/pm25-air-pollution.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, pm25 = 4)

# Population totale
data_pop <- read_csv("https://ourworldindata.org/grapher/population-unwpp.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, population = 4)

# Ratio de dépendance des personnes âgées (contrôle vieillissement)
data_vieux <- read_csv("https://ourworldindata.org/grapher/age-dependency-ratio-old.csv?v=1&csvType=full&useColumnShortNames=false") %>%
  rename(pays = Entity, iso3c = Code, annee = Year, ratio_dependance_agee = 4)


# ----- 3. création de la base finale et vérifications -----------
# Fusion complète
data_complete <- data_depenses %>%
  left_join(data_esp, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_pib, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_urbain, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_pollution, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_pop, by = c("pays", "iso3c", "annee")) %>%
  left_join(data_vieux, by = c("pays", "iso3c", "annee")) %>%
  filter(pays %in% pays_ocde, annee >= 2000, annee <= 2023)

# Déclarer le panel
pdata <- pdata.frame(data_complete, index = c("pays", "annee"))
pdim(pdata)

# summary
summary(pdata)
summary(pdata$esperance_vie)

# ------ 4. Stats descriptives -------

data_complete %>%
  group_by(annee) %>%
  summarise(moy = mean(esperance_vie, na.rm = TRUE),
            se = sd(esperance_vie, na.rm = TRUE) / sqrt(n()),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = annee, y = moy)) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                color = "blue", width = 0.3) +
  geom_line(color = "blue", linewidth = 1) +
  labs(x = "Année", y = "Espérance de vie (années)",
       title = "Espérance de vie moyenne, OCDE (2000-2023)") +
  theme_minimal()


data_complete %>%
  filter(pays %in% c("France", "United States",
                     "Japan", "Mexico", "Turkey", "Greece")) %>%
  ggplot(aes(x = annee, y = esperance_vie)) +
  geom_line(color = "darkred", linewidth = 1) +
  facet_wrap(~ pays) +
  labs(x = "Année", y = "Espérance de vie",
       title = "Espérance de vie, 2000-2023") +
  theme_minimal()


data_complete %>%
  group_by(pays) %>%
  summarise(moy = mean(esperance_vie, na.rm = TRUE),
            se = sd(esperance_vie, na.rm = TRUE) / sqrt(n()),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = reorder(pays, moy), y = moy)) +
  geom_pointrange(aes(ymin = ic_bas, ymax = ic_haut),
                  color = "darkviolet", size = 1) +
  coord_flip() +
  labs(x = NULL, y = "Espérance de vie",
       title = "Niveau moyen de l'espérance de vie par pays (2000-2023)") +
  theme_minimal()


ggplot(data_complete, aes(x = depenses_sante, y = esperance_vie)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE,
              linetype = "dashed") +
  labs(x = "Dépenses de santé par habitant ($)",
       y = "Espérance de vie (années)") +
  theme_minimal()


library(Polychrome)
palette37 <- createPalette(37, c("#010101", "#ff0000"))
palette37 <- unname(palette37)   # <- la correction clé

ggplot(data_complete, aes(x = depenses_sante, y = esperance_vie, color = pays)) +
  geom_point(alpha = 0.6, show.legend = FALSE) +
  scale_color_manual(values = palette37) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  labs(x = "Dépenses de santé par habitant ($)",
       y = "Espérance de vie (années)",
       title = "Une couleur par pays") +
  theme_minimal()


library(ggrepel)

# Une seule ligne par pays pour le label (ex: sa dernière année, 2023)
pays_exemple <- c("France", "United States", "Japan", "Mexico", "Turkey", "Greece")

data_labels <- data_complete %>%
  filter(pays %in% pays_exemple, annee == max(annee))

ggplot(data_complete, aes(x = depenses_sante, y = esperance_vie)) +
  geom_point(color = "grey80", alpha = 0.4) +
  geom_point(data = data_complete %>% filter(pays %in% pays_exemple),
             aes(color = pays), alpha = 0.8) +
  geom_text_repel(data = data_labels, aes(label = pays, color = pays),
                  fontface = "bold", size = 4.5, nudge_y = 1.2, show.legend = FALSE) +
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  labs(x = "Dépenses de santé par habitant ($)",
       y = "Espérance de vie (années)",
       title = "Quelques pays mis en évidence") +
  theme_minimal() +
  theme(legend.position = "none")


# ------ 5. Estimations -----------

## ----- pooling -------
library(fixest)

pooling_plm <- plm(esperance_vie ~ depenses_sante,
                   data = pdata, model = "pooling")
summary(pooling_plm)

pooling_fixest <- feols(esperance_vie ~ depenses_sante,
                        data = data_complete)
summary(pooling_fixest)

## ----- between ------
between_plm <- plm(esperance_vie ~ depenses_sante,
                   data = pdata, model = "between")
summary(between_plm)

## ----- within ---------
fe_plm <- plm(esperance_vie ~ depenses_sante,
              data = pdata, model = "within")
summary(fe_plm)

fe_fixest <- feols(esperance_vie ~ depenses_sante | pays,
                   data = data_complete)
summary(fe_fixest)

## ---- différences premières ------
d1_plm <- plm(esperance_vie ~ depenses_sante,
              data = pdata, model = "fd")
summary(d1_plm)

## ----- twfe ---------
twfe_plm <- plm(esperance_vie ~ depenses_sante,
                data = pdata, model = "within", effect = "twoways")
summary(twfe_plm)

twfe_fixest <- feols(esperance_vie ~ depenses_sante | pays + annee,
                     data = data_complete)
summary(twfe_fixest)

## ----- effets aléatoires ------
random_plm <- plm(esperance_vie ~ depenses_sante,
                  data = pdata, model = "random")
summary(random_plm)

# --- 6. tests ----

pFtest(fe_plm, pooling_plm)

plmtest(pooling_plm, type = "bp")

phtest(fe_plm, random_plm)
