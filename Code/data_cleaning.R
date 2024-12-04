# Load the dataset
library(Hmisc)
getHdata(bacteremia)


# Remove observations having wbc > 95 or wbc = NA
dataset_1 <- bacteremia[which(bacteremia$wbc <= 95),]


# Build a second dataset filtering the rows having 30 <= wbc <= 95

# Definisci la funzione per costruire dataset_2 e stampare le righe sopravvissute ed eliminate
create_dataset_2 <- function(dataset_1) {
  # Crea un dataset vuoto per le osservazioni che rispondono alla condizione
  dataset_2 <- dataset_1
  
  # Seleziona solo le osservazioni con wbc tra 30 e 95
  dataset_1_filtered <- dataset_1[dataset_1$wbc >= 30 & dataset_1$wbc <= 95, ]
  
  # Somma le variabili lym, mono, eos, baso, neu per ogni osservazione (ignorando NA)
  dataset_1_filtered$sum_variables <- apply(dataset_1_filtered[, c("lym", "mono", "eos", "baso", "neu")], 1, function(x) sum(x, na.rm = TRUE))
  
  # Calcola l'80% del valore di wbc per ogni osservazione
  dataset_1_filtered$threshold <- 0.80 * dataset_1_filtered$wbc
  
  # Filtra solo le osservazioni in cui la somma delle variabili è almeno l'80% di wbc
  dataset_1_filtered_passed <- dataset_1_filtered[dataset_1_filtered$sum_variables >= dataset_1_filtered$threshold, ]
  
  # Rimuovi le colonne temporanee
  dataset_1_filtered_passed <- dataset_1_filtered_passed[, !colnames(dataset_1_filtered_passed) %in% c("sum_variables", "threshold")]
  
  # Seleziona le righe eliminate (quelle con wbc tra 30 e 95, ma che non soddisfano la condizione)
  dataset_1_filtered_failed <- dataset_1_filtered[!rownames(dataset_1_filtered) %in% rownames(dataset_1_filtered_passed), ]
  
  # Stampa le righe sopravvissute
  print("Righe sopravvissute al filtraggio (wbc tra 30 e 95 e somma >= 80% di wbc):")
  print(dataset_1_filtered_passed[, c("lym", "mono", "eos", "baso", "neu","wbc")])
  
  # Stampa le righe eliminate
  print("Righe eliminate (wbc tra 30 e 95 ma somma < 80% di wbc):")
  print(dataset_1_filtered_failed[, c("lym", "mono", "eos", "baso", "neu", "wbc")])
  
  # Aggiungi le osservazioni con wbc < 30 (senza modifiche) a dataset_2
  dataset_2 <- rbind(dataset_2[dataset_2$wbc < 30, ], dataset_1_filtered_passed)
  
  return(dataset_2)
}

# Usa la funzione per creare dataset_2
dataset_2 <- create_dataset_2(dataset_1)


# Count how many obs still have at least one NA
rows_with_na <- sum(apply(dataset_2, 1, function(x) any(is.na(x))))
print(paste("Numero di righe con almeno un NA in dataset_2:", rows_with_na))

# Count how many NAs for each feature
na_per_column <- colSums(is.na(dataset_2))
print("Numero di NA per ciascuna variabile in dataset_2:")
print(na_per_column)

