
rm(list = ls())

# Load the data
data <- readRDS("C:\\Users\\jclaz\\OneDrive\\Documenti\\GitHub\\NonparametricProject\\Data\\bacteremia_no_NAs.rds")  # funziona per tutti se la wd è settata giusta

## Con il comando R

set.seed(09122024)

#### ALAT - NAs -> 987####

# Comando equivalente più sintetico
test_result <- wilcox.test(alat ~ bacteremia, data = data)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Ripeto ma separo maschi e femmine

data_female <- subset(data, sex == "female")
data_male <- subset(data, sex == "male")

## Femmine

set.seed(09122024)

# Comando equivalente più sintetico
test_result <- wilcox.test(alat ~ bacteremia, data = data_female)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Maschi

set.seed(09122024)

# Comando equivalente più sintetico
test_result <- wilcox.test(alat ~ bacteremia, data = data_male)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
}

#### AMY - NAs -> 3913 ####

# Comando equivalente più sintetico
test_result <- wilcox.test(amy ~ bacteremia, data = data)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Ripeto ma separo maschi e femmine

data_female <- subset(data, sex == "female")
data_male <- subset(data, sex == "male")

## Femmine

set.seed(09122024)

# Comando equivalente più sintetico
test_result <- wilcox.test(amy ~ bacteremia, data = data_female)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Maschi

set.seed(09122024)

# Comando equivalente più sintetico
test_result <- wilcox.test(amy ~ bacteremia, data = data_male)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
}

#### AP - NAs -> 1400 ####

# Comando equivalente più sintetico
test_result <- wilcox.test(ap ~ bacteremia, data = data)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra i pazienti con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Ripeto ma separo maschi e femmine

data_female <- subset(data, sex == "female")
data_male <- subset(data, sex == "male")

## Femmine

set.seed(09122024)

# Comando equivalente più sintetico
test_result <- wilcox.test(ap ~ bacteremia, data = data_female)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra le donne con e senza batteriemia (p =", test_result$p.value, ")\n")
}

## Maschi

set.seed(09122024)

# Comando equivalente più sintetico
test_result <- wilcox.test(ap ~ bacteremia, data = data_male)
print(test_result)

# Interpretazione
if (test_result$p.value < 0.05) {
  cat("I livelli di WBC differiscono significativamente tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
} else {
  cat("Non ci sono differenze significative nei livelli di WBC tra gli uomini con e senza batteriemia (p =", test_result$p.value, ")\n")
}
