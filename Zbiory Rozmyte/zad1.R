install.packages("dplyr")  
library(dplyr)


install.packages("sets")
library(sets)

data <- read.csv("/home/artsiom/Загрузки/zad5.csv")

head(data)
df <- as.data.frame(data)


fuzzy_triangle <- function(x, params) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  
  return(pmax(0, pmin((x - a) / (b - a), (c - x) / (c - b))))
}

fuzzy_membership <- function(x, params) {
  return(fuzzy_triangle(x, params))
}

df$max_predkosc_low <- fuzzy_membership(df$max_predkosc, c(0, 120, 140))
df$max_predkosc_medium <- fuzzy_membership(df$max_predkosc, c(120, 160, 180))
df$max_predkosc_high <- fuzzy_membership(df$max_predkosc, c(160, 200, 300))

df$zuzycie_paliwa_low <- fuzzy_membership(df$zuzycie_paliwa, c(0, 6, 8))
df$zuzycie_paliwa_medium <- fuzzy_membership(df$zuzycie_paliwa, c(6, 7, 9))
df$zuzycie_paliwa_high <- fuzzy_membership(df$zuzycie_paliwa, c(8, 10, 12))

df$ocena_low <- fuzzy_membership(df$ocena, c(0, 5, 7))
df$ocena_medium <- fuzzy_membership(df$ocena, c(6, 7, 8))
df$ocena_high <- fuzzy_membership(df$ocena, c(7, 8, 10))


fuzzy_rule1 <- function(row) {
  min(row$max_predkosc_high, row$zuzycie_paliwa_low)
}

df$ocena_high_proposed_rule <- apply(df, 1, fuzzy_rule1)


# Создание нечёткой системы (пример)
fuzzy_system <- function(row) {
  # Пример: Просто усредним нечёткие значения из нескольких атрибутов
  (row$max_predkosc_medium + row$zuzycie_paliwa_medium) / 2
}

df$fuzzy_classification <- apply(df, 1, fuzzy_system)


df$core <- apply(df[, c("max_predkosc_medium", "zuzycie_paliwa_medium", "ocena_medium")], 1, max)

alpha_level <- 0.8
df$alpha_section <- apply(df[, c("max_predkosc_medium", "zuzycie_paliwa_medium", "ocena_medium")], 1, function(row) {
  (row$max_predkosc_medium + row$zuzycie_paliwa_medium + row$ocena_medium) / 3 >= alpha_level
})

print("DataFrame with Fuzzy Memberships:")
print(df)

print("First few rows of DataFrame:")
print(head(df))

head(df)
