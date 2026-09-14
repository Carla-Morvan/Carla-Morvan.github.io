# Application R chapitre 1 --- Comprendre et manipuler les données de panel

# ---- 1. les packages nécessaires --------
library(tidyverse)
library(plm)

# ---- 2. importer les données (fichier fourni sur Moodle) -----------
data <- read_csv("data_depenses_sante_ocde.csv")
head(data)
str(data)

# ---- 3. passer du format wide au format long -----------
data_long <- data %>%
  pivot_longer(cols = starts_with("depenses_sante_"),
               names_to = "annee",
               names_prefix = "depenses_sante_",
               values_to = "depenses") %>%
  mutate(annee = as.integer(annee))   # important : annee doit être numérique

# ---- 4. déclarer le panel et stats descriptives de base -----------
pdata <- pdata.frame(data_long, index = c("pays", "annee"))

pdim(pdata)                 # structure du panel (N, T, cylindré ?)
summary(pdata$depenses)     # décomposition within / between

# ---- 5. visualiser le panel -----------
# Astuce : pour tous les graphiques, on utilise data_long (pas pdata) :
# dans un pdata.frame, la colonne "annee" devient un facteur, ce qui
# fausserait l'axe du temps si on la reconvertit avec as.numeric().

# 5.1 facet wrap : quelques pays, une trajectoire par pays
data_long %>%
  filter(pays %in% c("France", "United States",
                     "Japan", "Mexico", "Turkey", "Greece")) %>%
  ggplot(aes(x = annee, y = depenses)) +
  geom_line(color = "darkred", linewidth = 1) +
  facet_wrap(~ pays) +
  labs(x = "Année", y = "Dépenses de santé ($)",
       title = "Dépenses de santé par habitant, 2000-2020") +
  theme_minimal()

# 5.2 moyenne +/- écart-type par année
data_long %>%
  group_by(annee) %>%
  summarise(moy = mean(depenses, na.rm = TRUE),
            ecart_type = sd(depenses, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = moy)) +
  geom_ribbon(aes(ymin = moy - ecart_type, ymax = moy + ecart_type),
              fill = "#028090", alpha = 0.15) +
  geom_line(color = "#028090", linewidth = 1) +
  geom_point(color = "#028090", size = 1.5) +
  labs(x = "Année", y = "Dépenses de santé ($)",
       title = "Dépenses de santé : moyenne \u00b1 écart-type (OCDE)") +
  theme_minimal()

# 5.3 moyenne simple par année
data_long %>%
  group_by(annee) %>%
  summarise(depenses_moy = mean(depenses, na.rm = TRUE)) %>%
  ggplot(aes(x = annee, y = depenses_moy)) +
  geom_line(color = "#028090", linewidth = 1) +
  geom_point(color = "#028090", size = 1.5) +
  labs(x = "Année", y = "Dépenses de santé moyennes ($)",
       title = "Dépenses de santé moyennes par habitant (OCDE), 2000-2020") +
  theme_minimal()

# 5.4 moyenne par année avec intervalle de confiance à 95%
data_long %>%
  group_by(annee) %>%
  summarise(moy = mean(depenses, na.rm = TRUE),
            ecart_type = sd(depenses, na.rm = TRUE),
            n = n(),
            se = ecart_type / sqrt(n),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = annee, y = moy)) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                color = "darkblue", width = 0.3) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  labs(x = "Année", y = "Dépenses de santé ($)",
       title = "Dépenses de santé moyennes par habitant (OCDE) avec IC à 95%") +
  theme_minimal()

# 5.5 niveau moyen par pays (quelques pays), avec IC
data_long %>%
  filter(pays %in% c("France", "United States",
                     "Japan", "Mexico", "Turkey", "Greece")) %>%
  group_by(pays) %>%
  summarise(moy = mean(depenses, na.rm = TRUE),
            se = sd(depenses, na.rm = TRUE) / sqrt(n()),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = reorder(pays, moy), y = moy)) +
  geom_pointrange(aes(ymin = ic_bas, ymax = ic_haut),
                  color = "darkviolet", size = 1) +
  coord_flip() +
  labs(x = NULL, y = "Dépenses de santé moyennes ($)",
       title = "Niveau moyen de dépenses par habitant par pays (2000-2020)") +
  theme_minimal()

