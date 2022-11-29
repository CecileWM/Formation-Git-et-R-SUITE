rm(list = ls())

# Librairies -------------------------------------

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")

library(tidyverse)
library(MASS)
library(dplyr)

api_token <- yaml::read_yaml("env.yaml")[[API_TOKEN]]

source("R/functions.R", encoding = "UTF-8")

# Import des données -----------------------------

# import des données avec read_csv2 (séparateur = ;) ,
df <- readr::read_csv2("individu_reg.csv",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
                "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
                "trans", "ur")
)

# Restructuration des données ---------------------
df2 <- df %>%
  dplyr::select(c("region", "dept", "aemm", "aged", "anai", "catl", "cs1",
                  "cs2", "cs3", "couple", "na38", "naf08", "pnai12", "sexe",
                  "surf", "tp", "trans", "ur"))
print(df2, 20)

# Statistiques descriptives ------------------------------

# combien de professions
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs3[!is.na(cs3)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs1[!is.na(cs1)])))))

summarise(group_by(df2, aged), n())

decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %% 10)
}

# stats surf par statut
df3 <- tibble(df2 %>%
                group_by(couple, surf) %>%
                summarise(x = n()) %>%
                group_by(couple) %>%
                mutate(y = 100 * x / sum(x)))

# stats trans par statut
df3 <- tibble(df2 %>%
                group_by(couple, trans) %>%
                summarise(x = n()) %>%
                group_by(couple) %>%
                mutate(y = 100 * x / sum(x)))

# Graphiques ------------------------------------------------------
df2 %>%
  select(aged) %>%
  ggplot(.) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50, c(3, 4)], aes(
  x = as.numeric(aged), # x = as.numeric(aged) - as.numeric(aged) %% 5,
  y = ..density.., fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
), alpha = 0.2) +
  geom_histogram() # position = "dodge") + scale_fill_viridis_d()

# part d'homme dans chaque cohorte
ggplot(df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1)) +
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity") +
  geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

ggplot(df3) %>%
  geom_bar(aes(x = surf, y = y, color = couple), stat = "identity", position = "dodge")

p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity", position = "dodge")

dir.create("output")
ggsave("p.png", p)

df2[df2$na38 == "ZZ", "na38"] <- NA
df2[df2$trans == "Z", "trans"] <- NA
df2[df2$tp == "Z", "tp"] <- NA
df2[df2$naf08 == "ZZZZZ", "naf08"] <- NA
# df2[df2$aemm == "ZZZZ","aemm"] <- NA

str(df2)
df2[, nrow(df2) - 1] <- factor(df2[, nrow(df2) - 1])
df2$ur <- factor(df2$ur)
library(forcats)
df2$sexe <-
  fct_recode(df2$sexe, "Homme" = "0", "Femme" = "1")


fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")


fonction_de_stat_agregee(df %>%
  filter(sexe == "Homme") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged))
fonction_de_stat_agregee(df2 %>%
  filter(sexe == "Femme") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged))
fonction_de_stat_agregee(df2 %>%
  filter(sexe == "Homme" & couple == "2") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged))
fonction_de_stat_agregee(df2 %>%
  filter(sexe == "Femme" & couple == "2") %>%
  mutate(aged = as.numeric(aged)) %>%
  pull(aged))

# modelisation ------------------------------------------------

df3 <- df2 %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = T)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)
polr(surf ~ cs1 + factor(ur), df3)
