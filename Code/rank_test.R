
rm(list = ls())

# Load the data
data <- readRDS("../Data/bacteremia_no_NAs.rds")  # funziona per tutti se la wd è settata giusta




## Con il codice di Vantini

X <- data$neu # SCEGLIERE COVARIATA DI INTERESSE e sostituire tutti
Bacteremia <- data$bacteremia

# Creare i gruppi
X1 <- X[Bacteremia == 1] # Pazienti con batteriemia
X2 <- X[Bacteremia == 0] # Pazienti senza batteriemia
n1 <- length(X1)
n2 <- length(X2)
n <- length(X)

# Calcolare i ranghi
ranks.X <- rank(X)

# Somma dei ranghi per i due gruppi
R1 <- sum(ranks.X[Bacteremia == 1])
U1 <- R1 - n1 * (n1 + 1) / 2 # Numero di "vittorie" per il primo gruppo

R2 <- sum(ranks.X[Bacteremia == 0])
U2 <- R2 - n2 * (n2 + 1) / 2 # Numero di "vittorie" per il secondo gruppo

# Verifica del numero di confronti
cat("Numero totale di confronti possibili:", n1 * n2, "\n")

# Squilibrio rispetto alla media sotto l'ipotesi nulla
cat("Squilibrio per il primo gruppo:", U1 - n1 * n2 / 2, "\n")
cat("Squilibrio per il secondo gruppo:", U2 - n1 * n2 / 2, "\n")

# Simulazione Monte Carlo per stimare il p-value
set.seed(08122024)
B <- 100000
U1.sim <- numeric(B)
U2.sim <- numeric(B)

for (k in 1:B) {
  ranks.temp <- sample(1:n)
  R1.temp <- sum(ranks.temp[1:n1])
  R2.temp <- sum(ranks.temp[(n1 + 1):(n1 + n2)])
  U1.temp <- R1.temp - n1 * (n1 + 1) / 2
  U2.temp <- R2.temp - n2 * (n2 + 1) / 2
  U1.sim[k] <- U1.temp
  U2.sim[k] <- U2.temp
}

# Istogrammi per visualizzare la distribuzione simulata
hist(U1.sim, breaks = 50, main = "Distribuzione di U1 simulato")
abline(v = c(U1, U2), col = 'red')
abline(v = n1 * n2 / 2, lwd = 3)

hist(U2.sim, breaks = 50, main = "Distribuzione di U2 simulato")
abline(v = c(U1, U2), col = 'red')
abline(v = n1 * n2 / 2, lwd = 3)

# Calcolo del p-value
U.star <- max(U1, U2)
p.value <- 2 * sum(U1.sim >= U.star) / B
cat("p-value:", p.value, "\n")

# Trasformazione monotona non lineare
X.exp <- -exp(-X)
X1.exp <- -exp(-X1)
X2.exp <- -exp(-X2)

plot(X, X.exp, col = Bacteremia + 1, main = "Trasformazione non lineare", xlab = "X", ylab = "X trasformato")
boxplot(X, main = "Boxplot di X")
boxplot(X.exp, main = "Boxplot di X trasformato")

# Ripetere i calcoli con la trasformazione
ranks.X.exp <- rank(X.exp)
R1.exp <- sum(ranks.X.exp[Bacteremia == 1])
U1.exp <- R1.exp - n1 * (n1 + 1) / 2

U.star.exp <- n1 * n2 / 2 + abs(U1.exp - n1 * n2 / 2)

# p-value per la versione trasformata
p.value.exp <- 2 * sum(U1.sim >= U.star.exp) / B
cat("p-value con trasformazione:", p.value.exp, "\n")

# Confronto con il t-test
cat("t-test originale:", t.test(X1, X2)$p.value, "\n")
cat("t-test trasformato:", t.test(X1.exp, X2.exp)$p.value, "\n")




## Ripeto ma separo maschi e femmine

data_female <- subset(data, sex == "female")
data_male <- subset(data, sex == "male")

# Funzione per il test Mann-Whitney U con simulazione Monte Carlo
mann_whitney_mc <- function(group1, group2, B = 100000, seed = 08122024) {
  n1 <- length(group1)
  n2 <- length(group2)
  n <- n1 + n2
  ranks <- rank(c(group1, group2))
  R1 <- sum(ranks[1:n1])
  U1 <- R1 - n1 * (n1 + 1) / 2
  
  set.seed(seed)
  U1.sim <- numeric(B)
  for (k in 1:B) {
    ranks_temp <- sample(1:n)
    R1_temp <- sum(ranks_temp[1:n1])
    U1.sim[k] <- R1_temp - n1 * (n1 + 1) / 2
  }
  
  U.star <- n1 * n2 / 2 + abs(U1 - n1 * n2 / 2)
  p.value <- 2 * sum(U1.sim >= U.star) / B
  return(list(U1 = U1, p.value = p.value))
}

# Analisi per femmine
cat("Analisi per femmine:\n")
X_female_1 <- data_female$neu[data_female$bacteremia == 1]
X_female_0 <- data_female$neu[data_female$bacteremia == 0]

result_female <- mann_whitney_mc(X_female_1, X_female_0)
cat("U1:", result_female$U1, "\n")
cat("p-value:", result_female$p.value, "\n")

# Analisi per maschi
cat("\nAnalisi per maschi:\n")
X_male_1 <- data_male$neu[data_male$bacteremia == 1]
X_male_0 <- data_male$neu[data_male$bacteremia == 0]

result_male <- mann_whitney_mc(X_male_1, X_male_0)
cat("U1:", result_male$U1, "\n")
cat("p-value:", result_male$p.value, "\n")




## Con il comando R

set.seed(09122024)

# Sottogruppi: pazienti con e senza batteriemia
group1 <- data$neu[data$bacteremia == 1]  # Pazienti con batteriemia
group2 <- data$neu[data$bacteremia == 0]  # Pazienti senza batteriemia

# Wilcoxon Rank Sum Test (Mann-Whitney U-Test)
test_result <- wilcox.test(group1, group2, paired = FALSE) # paired = TRUE sarebbe per dati accoppiati
print(test_result)

# Comando equivalente più sintetico
test_result <- wilcox.test(neu ~ bacteremia, data = data, alternative = "less")
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di X differiscono significativamente tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di X tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
}




## Ripeto ma separo maschi e femmine

## Femmine

set.seed(09122024)

# Sottogruppi: donne con e senza batteriemia
group1 <- data_female$neu[data_female$bacteremia == 1]  # Donne con batteriemia
group2 <- data_female$neu[data_female$bacteremia == 0]  # Donne senza batteriemia

# Wilcoxon Rank Sum Test (Mann-Whitney U-Test)
test_result <- wilcox.test(group1, group2, paired = FALSE) # paired = TRUE sarebbe per dati accoppiati
print(test_result)

# Comando equivalente più sintetico
test_result <- wilcox.test(neu ~ bacteremia, data = data_female, alternative = greater)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di X differiscono significativamente tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di X tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Maschi

set.seed(09122024)

# Sottogruppi: uomini con e senza batteriemia
group1 <- data_male$neu[data_male$bacteremia == 1]  # Uomini con batteriemia
group2 <- data_male$neu[data_male$bacteremia == 0]  # Uomini senza batteriemia

# Wilcoxon Rank Sum Test (Mann-Whitney U-Test)
test_result <- wilcox.test(group1, group2, paired = FALSE) # paired = TRUE sarebbe per dati accoppiati
print(test_result)

# Comando equivalente più sintetico
test_result <- wilcox.test(neu ~ bacteremia, data = data_male, alternative = greater)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di X differiscono significativamente tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di X tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
}
