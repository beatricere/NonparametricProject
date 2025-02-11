library(Hmisc)
getHdata(bacteremia)

### NA TREATMENT DIFFERENT APPROACH ###

summary(bacteremia)

#### STEP 0 ####
## We eliminate subjects who are less than 18 years old, since we are making a diagnostic tool for adults. There are 
## only 29 of these obs anyways, doesn't make a difference. 
length(which(bacteremia$age < 18))
bact_0 <- bacteremia[-which(bacteremia$age < 18),]

#### STEP 1 ####
## For 3 variables: pamy (48.4%), trig (34.4%) and chol (34.3%), the percentage of NAs is VERY high 
## for now we will eliminate them from the dataset, this can always be thought about more later

bact_1 <- bact_0[, !colnames(bact_0) %in% c("pamy", "trig", "chol")]

#### STEP 2 #### 
## We remove the observations where more than 15 / 50 variables have NAs 

# Count the number of NAs in each row
na_count_per_row <- apply(bact_1, 1, function(row) sum(is.na(row)))

row_indices <- which(na_count_per_row > 15)

bact_2 <- bact_1[-row_indices,]

# there is an obvious relationship between white blood cell count and ratio of that type of white blood 
# cell, for the observations where both are missing, we will discard eg if baso and basor are both missing

missing_baso <- apply(bact_2[, c("baso", "basor")], 1, function(x) all(is.na(x)))
rows_with_missing_baso <- bact_2[missing_baso, ]
sum(missing_baso)

missing_eos <- apply(bact_2[, c("eos", "eosr")], 1, function(x) all(is.na(x)))
rows_with_missing_eos <- bact_2[missing_eos, ]
sum(missing_eos)

#### STEP 3 #### 
# Now we focus on white blood cells - for each white blood cell we have two pieces of information: its
# total count and its percentage in the blood, if we're missing one of these but not the other (and wbc
# is present), we can impute. If both are missing, we can't directly. Let's take a look. 

# If a row is missing all 5 of its counts and all 5 of its ratios, we throw it away 

# Identify rows which is missing all 5 counts
missing_counts <- apply(bact_2[, c("baso", "eos", "mono", "lym", "neu")], 1, function(x) all(is.na(x)))

# idxs
idxs_missing_counts <- which(missing_counts)

# Identify rows which is missing all 5 ratios
missing_ratios <- apply(bact_2[, c("basor", "eosr", "monor", "lymr", "neur")], 1, function(x) all(is.na(x)))

# idxs
idxs_missing_ratios <- which(missing_ratios)

missing_counts_and_ratios <- intersect(idxs_missing_counts,idxs_missing_ratios)

# remove these from the dataset
bact_3 <- bact_2[-missing_counts_and_ratios,]

#### Now we will look at the summary again
summary(bact_3)

#### We save it 
saveRDS(bact_3, 
        file = "/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/dataset_filtrato_ma_con_NA.rds")

# Fixing NAs --------------------------------------------------------------

data <- readRDS("/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/dataset_filtrato_ma_con_NA.rds")

View(data)

# Now we want to do NA imputation, we will do this using the median except for the ratios where
# we will compute the ratio manually 

data$plt[is.na(data$plt)] <- median(data$plt, na.rm=TRUE)
data$rdw[is.na(data$rdw)] <- median(data$rdw, na.rm=TRUE)
data$mpv[is.na(data$mpv)] <- median(data$mpv, na.rm=TRUE)

data$lym[is.na(data$lym)] <- median(data$lym, na.rm=TRUE)
data$mono[is.na(data$mono)] <- median(data$mono, na.rm=TRUE)
data$eos[is.na(data$eos)] <- median(data$eos, na.rm=TRUE)
data$baso[is.na(data$baso)] <- median(data$baso, na.rm=TRUE)
data$nt[is.na(data$nt)] <- median(data$nt, na.rm=TRUE)

data$aptt[is.na(data$aptt)] <- median(data$aptt, na.rm=TRUE)
data$fib[is.na(data$fib)] <- median(data$fib, na.rm=TRUE)
data$sodium[is.na(data$sodium)] <- median(data$sodium, na.rm=TRUE)
data$potass[is.na(data$potass)] <- median(data$potass, na.rm=TRUE)
data$ca[is.na(data$ca)] <- median(data$ca, na.rm=TRUE)

data$phos[is.na(data$phos)] <- median(data$phos, na.rm=TRUE)
data$mg[is.na(data$mg)] <- median(data$mg, na.rm=TRUE)
data$crea[is.na(data$crea)] <- median(data$crea, na.rm=TRUE)
data$bun[is.na(data$bun)] <- median(data$bun, na.rm=TRUE)
data$hs[is.na(data$hs)] <- median(data$hs, na.rm=TRUE)

data$gbil[is.na(data$gbil)] <- median(data$gbil, na.rm=TRUE)
data$tp[is.na(data$tp)] <- median(data$tp, na.rm=TRUE)
data$alb[is.na(data$alb)] <- median(data$alb, na.rm=TRUE)
data$amy[is.na(data$amy)] <- median(data$amy, na.rm=TRUE)
data$lip[is.na(data$lip)] <- median(data$lip, na.rm=TRUE)

data$che[is.na(data$che)] <- median(data$che, na.rm=TRUE)
data$ap[is.na(data$ap)] <- median(data$ap, na.rm=TRUE)
data$asat[is.na(data$asat)] <- median(data$asat, na.rm=TRUE)
data$alat[is.na(data$alat)] <- median(data$alat, na.rm=TRUE)
data$ggt[is.na(data$ggt)] <- median(data$ggt, na.rm=TRUE)

data$ldh[is.na(data$ldh)] <- median(data$ldh, na.rm=TRUE)
data$ck[is.na(data$ck)] <- median(data$ck, na.rm=TRUE)
data$glu[is.na(data$glu)] <- median(data$glu, na.rm=TRUE)
data$crp[is.na(data$crp)] <- median(data$crp, na.rm=TRUE)
data$neu[is.na(data$neu)] <- median(data$neu, na.rm=TRUE)

data$pdw[is.na(data$pdw)] <- median(data$pdw, na.rm=TRUE)
data$rbc[is.na(data$rbc)] <- median(data$rbc, na.rm=TRUE)
data$wbc[is.na(data$wbc)] <- median(data$wbc, na.rm=TRUE)

summary(data)

# for the ratios 
## NB for some reason patient id 7142 has a 0 wbc which is obviously impossible, so they will also 
# be removed from the dataset 
idx <- which(data$wbc == 0)
data <- data[-idx,]

impute_by_ratio <- function (count, ratio, wbc) {
  for (i in 1:length(count)) {
    if (is.na(ratio[i])) {
      ratio[i] = count[i]/wbc[i]
    }
  }
  return (ratio)
}

wbc = data$wbc

# fixing basor 
count = data$baso
ratio = data$basor
data[,"basor"] <- impute_by_ratio(count, ratio, wbc)

# fixing eosr 
count = data$eos
ratio = data$eosr
data[,"eosr"] <- impute_by_ratio(count, ratio, wbc)

# fixing lymr 
count = data$lym
ratio = data$lymr
data[,"lymr"] <- impute_by_ratio(count, ratio, wbc)

# fixing monor 
count = data$mono
ratio = data$monor
data[,"monor"] <- impute_by_ratio(count, ratio, wbc)

# fixing neur
count = data$neu
ratio = data$neur
data[,"neur"] <- impute_by_ratio(count, ratio, wbc)

sum(is.na(data))

# Dataset is NA free, save it!
saveRDS(data, 
        file = "/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/dataset_NAs_fixed.rds")


# plots -------------------------------------------------------------------
library(ggpubr)

bact <- readRDS("/Users/giuliadesanctis/Desktop/POLIMI/mag_4_SEM_39/Non-parametric statistics/dataset_NAs_fixed.rds")

no_bact <- rep("no bacteremia", 13006)
yes_bact <- rep("bacteremia", 1128)

df <- data.frame(c(bact$wbc[which(bact$bacteremia == 0)], bact$wbc[which(bact$bacteremia == 1)]), c(no_bact, yes_bact))
colnames(df) <- (c("WBC", "Bacteremia"))

ggboxplot(df, x = "Bacteremia", y = "WBC", 
          main = "Comparing WBC for bacteremic and non-bacteremic patients",
          legend = "right",
          color = "Bacteremia", palette = c("#00AFBB", "#E7B800"),
          ylab = "WBC level", xlab = "Groups")

# H0: wbc_sick = wbc_healthy
# H1: wbc_sick != wbc_healthy

test <- wilcox.test(x = bact$wbc[which(bact$bacteremia == 0)], 
                    y = bact$wbc[which(bact$bacteremia == 1)],
                    alternative = "two.sided", paired = FALSE, correct = TRUE,
                    conf.int = FALSE, conf.level = 0.95)

test$p.value > 0.05

######

df <- data.frame(c(bact$crp[which(bact$bacteremia == 0)], bact$crp[which(bact$bacteremia == 1)]), c(no_bact, yes_bact))
colnames(df) <- (c("CRP", "Bacteremia"))

ggboxplot(df, x = "Bacteremia", y = "CRP", 
          main = "Comparing CRP for bacteremic and non-bacteremic patients",
          legend = "right",
          color = "Bacteremia", palette = c("#00AFBB", "#E7B800"),
          ylab = "CRP level", xlab = "Groups")

# H0: wbc_sick = wbc_healthy
# H1: wbc_sick != wbc_healthy

test <- wilcox.test(x = bact$crp[which(bact$bacteremia == 0)], 
                    y = bact$crp[which(bact$bacteremia == 1)],
                    alternative = "two.sided", paired = FALSE, correct = TRUE,
                    conf.int = FALSE, conf.level = 0.95)

test$p.value > 0.05

######

df <- data.frame(c(bact$amy[which(bact$bacteremia == 0)], bact$amy[which(bact$bacteremia == 1)]), c(no_bact, yes_bact))
colnames(df) <- (c("AMY", "Bacteremia"))

ggboxplot(df, x = "Bacteremia", y = "AMY", 
          main = "Comparing AMY for bacteremic and non-bacteremic patients",
          legend = "right",
          color = "Bacteremia", palette = c("#00AFBB", "#E7B800"),
          ylab = "AMY level", xlab = "Groups")

# H0: wbc_sick = wbc_healthy
# H1: wbc_sick != wbc_healthy

test <- wilcox.test(x = bact$amy[which(bact$bacteremia == 0)], 
                    y = bact$amy[which(bact$bacteremia == 1)],
                    alternative = "two.sided", paired = FALSE, correct = TRUE,
                    conf.int = FALSE, conf.level = 0.95)

test$p.value > 0.05
