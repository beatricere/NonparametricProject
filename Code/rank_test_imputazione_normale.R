
rm(list = ls())

# Load the data
library(Hmisc)
getHdata(bacteremia)
data <- bacteremia



X <- data$ap # SCEGLIERE COVARIATA DI INTERESSE e sostituire solo qui
Bacteremia <- data$bacteremia
Sex <- data$sex

# Riempio NAs con mediana
mediana <- median(X, na.rm = TRUE)
X[is.na(X)] <- mediana

# Creare i gruppi
X1 <- X[Bacteremia == 1] # Pazienti con batteriemia
X2 <- X[Bacteremia == 0] # Pazienti senza batteriemia

set.seed(09122024)

# Wilcoxon Rank Sum Test (Mann-Whitney U-Test)
test_result <- wilcox.test(X1, X2, paired = FALSE) # paired = TRUE sarebbe per dati accoppiati
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di X differiscono significativamente tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di X tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
}




## Ripeto ma separo maschi e femmine

# Maschi e femmine con batteriemia
X1_male <- X[Bacteremia == 1 & Sex == "male"]
X1_female <- X[Bacteremia == 1 & Sex == "female"]

# Maschi e femmine senza batteriemia
X2_male <- X[Bacteremia == 0 & Sex == "male"]
X2_female <- X[Bacteremia == 0 & Sex == "female"]

## Femmine

set.seed(09122024)

# Wilcoxon Rank Sum Test (Mann-Whitney U-Test)
test_result <- wilcox.test(X1_female, X2_female, paired = FALSE) # paired = TRUE sarebbe per dati accoppiati
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di X differiscono significativamente tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di X tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Maschi

set.seed(09122024)

# Wilcoxon Rank Sum Test (Mann-Whitney U-Test)
test_result <- wilcox.test(X1_male, X2_male, paired = FALSE) # paired = TRUE sarebbe per dati accoppiati
print(test_result)


# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di X differiscono significativamente tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di X tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
}

