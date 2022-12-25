library(shiny)
library(markdown)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(shinydashboardPlus)

repartition_sexe <- read.csv("./data/repartition_sexe_clean.csv")
repartition_cat <- read.csv("./data/repartition_cat_clean.csv")
repartition_cat_id <- read.csv("./data/repartition_cat_id_clean.csv")
repartition_grade <- read.csv("./data/repartition_grade_clean.csv")
repartition_grade_total <- read.csv("./data/repartition_grade_total_clean.csv")
repartition_age <- read.csv("./data/repartition_age_clean.csv")
repartition_corps <- read.csv("./data/repartition_corps_clean.csv")
repartition_residence <- read.csv("./data/repartition_residence_clean.csv")
repartition_residence_globale1 <- read.csv("./data/residence_globale_clean1.csv")
repartition_residence_globale2 <- read.csv("./data/residence_globale_clean2.csv")
repartition_position_depart <- read.csv("./data/repartition_position_depart_clean.csv")
repartition_position_admin <- read.csv("./data/repartition_position_admin_clean.csv")
repartition_recrutement <- read.csv("./data/repartition_recrutement_clean.csv")
repartition_metier <- read.csv("./data/repartition_metier_clean.csv")





repartition_age$n <- as.numeric(repartition_age$n)
repartition_age$age <- as.numeric(repartition_age$age)