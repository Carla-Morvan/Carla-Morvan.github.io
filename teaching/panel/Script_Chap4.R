# Application R chapitre 4 --- Différences-en-différences

# ---- 1. les packages nécessaires --------
library(tidyverse)
library(fixest)
# install.packages("causaldata")  # si pas déjà installé

# ---- 2. importer les données + stats descriptives -----------
data_did <- causaldata::organ_donations
data_did

summary(data_did)

data_did %>%
  group_by(Quarter_Num) %>%
  summarise(moy = mean(Rate, na.rm = TRUE),
            se = sd(Rate, na.rm = TRUE) / sqrt(n()),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = Quarter_Num, y = moy)) +
  geom_errorbar(aes(ymin = ic_bas, ymax = ic_haut),
                color = "blue", width = 0.3) +
  geom_line(color = "blue", linewidth = 1) +
  labs(x = "Trimestre", y = "Taux de donneurs d'organes",
       title = "Taux de don d'organes moyen, tous États confondus") +
  theme_minimal()

data_did %>%
  ggplot(aes(x = as.numeric(Quarter_Num), y = Rate)) +
  geom_line(color = "darkred", linewidth = 1) +
  facet_wrap(~ State) +
  labs(x = "Trimestre", y = "% de donneurs d'organes",
       title = "Donneurs d'organes : 2010-2012") +
  theme_minimal()

data_did %>%
  group_by(State) %>%
  summarise(moy = mean(Rate, na.rm = TRUE),
            se = sd(Rate, na.rm = TRUE) / sqrt(n()),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = reorder(State, moy), y = moy)) +
  geom_pointrange(aes(ymin = ic_bas, ymax = ic_haut),
                  color = "darkviolet", size = 1) +
  coord_flip() +
  labs(x = NULL, y = "% de donneurs d'organes",
       title = "Niveau moyen de donneurs d'organes : 2010-2012") +
  theme_minimal()

# 2.1 stats descriptives avant le choc uniquement
data_did %>%
  filter(Quarter_Num < 4) %>%   # avant juillet 2011, donc jusqu'au trimestre 2 de 2011
  group_by(State) %>%
  summarise(moy = mean(Rate, na.rm = TRUE),
            se = sd(Rate, na.rm = TRUE) / sqrt(n()),
            ic_bas = moy - 1.96 * se,
            ic_haut = moy + 1.96 * se) %>%
  ggplot(aes(x = reorder(State, moy), y = moy)) +
  geom_pointrange(aes(ymin = ic_bas, ymax = ic_haut),
                  color = "darkviolet", size = 1) +
  coord_flip() +
  labs(x = NULL, y = "% de donneurs d'organes",
       title = "Niveau moyen de donneurs d'organes avant la réforme en Californie") +
  theme_minimal()


# ---- 3. créer la variable de traitement -----------
# Variable de traitement : Californie, à partir de Q3 2011
data_did <- data_did %>%
  mutate(traitement = ifelse(State == 'California', 1, 0),
         post = ifelse(Quarter %in% c('Q32011','Q42011','Q12012'), 1, 0))


# ---- 4. observer les tendances -----------
# si les tendances ne sont pas (du tout) parallèles, ça ne sert à rien de continuer
data_did %>%
  group_by(Quarter, traitement) %>%
  summarise(taux_moyen = mean(Rate)) %>%
  ggplot(aes(x = Quarter, y = taux_moyen,
             color = factor(traitement, labels = c("Contrôle", "Traitement")),
             group = traitement)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Contrôle" = "#2C3E7B", "Traitement" = "#B22222")) +
  scale_y_continuous(limits = c(0, 0.75)) +
  geom_vline(xintercept = "Q32011", linetype = "dashed", color = "gray40") +
  labs(x = "Trimestre", y = "Taux de don d'organes moyen",
       title = "Tendances : Californie vs autres États",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ---- 5. estimer le modèle -----------
model_2x2 <- feols(Rate ~ traitement * post,
                   data = data_did, vcov = ~State)
summary(model_2x2)

model_TWFE <- feols(Rate ~ traitement * post | State + Quarter,
                    data = data_did, vcov = ~State)
summary(model_TWFE)


# ---- 6. test placebo -----------
od_placebo <- data_did %>%
  filter(Quarter_Num <= 3) %>%   # on garde seulement la période avant le choc
  mutate(FakeTreat1 = ifelse(State == 'California' & Quarter %in% c('Q12011','Q22011'), 1, 0),
         FakeTreat2 = ifelse(State == 'California' & Quarter == 'Q22011', 1, 0))

placebo1 <- feols(Rate ~ FakeTreat1 | State + Quarter,
                  data = od_placebo, vcov = ~State)
placebo2 <- feols(Rate ~ FakeTreat2 | State + Quarter,
                  data = od_placebo, vcov = ~State)
etable(placebo1, placebo2)


# ---- 7. DiD dynamique -----------
event_study <- feols(
  Rate ~ i(Quarter_Num, traitement, ref = 3) | State + Quarter_Num,
  data = data_did, vcov = ~State
)

iplot(event_study)
