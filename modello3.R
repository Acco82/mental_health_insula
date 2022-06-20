##caricati i dati direttamente da stata

INSULA_lavoratori_variabili_modello_3 <- read_dta("C:/Users/xf52639/OneDrive - INAIL/INAIL/INSULA 2/parte 3/STATA/dta/INSULA lavoratori_variabili modello 3.dta")


##normalizziamo i dati su una scala compresa tra 0 e 100, come fatto su articolo su dati europei

norm_100 <- function(x) {
  return ((x / max(x))*100)
}

##essendoci dei valori nulli, ci sono dei problemi con la funzione max, pertanto
##dobbiamo crearne una apposta inserendo manualmente il valore massimo

norm_a8_ateco_add <- function(x) {
  return ((x / 1.78)*100)
}



insula.norm <- as.data.frame(INSULA_lavoratori_variabili_modello_3)


insula.norm$A1_1 <- norm_100(insula.norm$A1_1)
insula.norm$A1_2 <- norm_100(insula.norm$A1_2)
insula.norm$A1_3 <- norm_100(insula.norm$A1_3)
insula.norm$A1_4 <- norm_100(insula.norm$A1_4)
insula.norm$A1_5 <- norm_100(insula.norm$A1_5)
insula.norm$A1_6 <- norm_100(insula.norm$A1_6)
insula.norm$A1_7 <- norm_100(insula.norm$A1_7)
insula.norm$A1_8 <- norm_100(insula.norm$A1_8)
insula.norm$A1_9 <- norm_100(insula.norm$A1_9)
insula.norm$A1_10 <- norm_100(insula.norm$A1_10)
insula.norm$A1_11 <- norm_100(insula.norm$A1_11)
insula.norm$A1_12 <- norm_100(insula.norm$A1_12)
insula.norm$A1_13 <- norm_100(insula.norm$A1_13)
insula.norm$A1_14 <- norm_100(insula.norm$A1_14)
insula.norm$A1_15 <- norm_100(insula.norm$A1_15)
insula.norm$A1_16 <- norm_100(insula.norm$A1_16)
insula.norm$A1_17 <- norm_100(insula.norm$A1_17)
insula.norm$A1_18 <- norm_100(insula.norm$A1_18)

insula.norm$B5 <- norm_100(insula.norm$B5)
insula.norm$B5_tot <- norm_100(insula.norm$B5_tot)
insula.norm$B5_wb <- norm_100(insula.norm$B5_wb)
insula.norm$B5_rev <- norm_100(insula.norm$B5_rev)
insula.norm$B5depres_bin <- norm_100(insula.norm$B5depres_bin)
insula.norm$B5_five <- norm_100(insula.norm$B5_five)
insula.norm$B5_rev5 <- norm_100(insula.norm$B5_rev5)
insula.norm$B5_1 <- norm_100(insula.norm$B5_1)
insula.norm$B5_2<- norm_100(insula.norm$B5_2)
insula.norm$B5_3 <- norm_100(insula.norm$B5_3)
insula.norm$B5_4 <- norm_100(insula.norm$B5_4)
insula.norm$B5_5 <- norm_100(insula.norm$B5_5)


insula.norm$A8_ateco_dat <- norm_100(insula.norm$A8_ateco_dat)
insula.norm$A8_ateco_add_dat <- norm_a8_ateco_add(insula.norm$A8_ateco_add_dat)



##CFA su DEMANDS e RESOURCES
##https://lavaan.ugent.be/tutorial/est.html

library(lavaan)


dem_res_model <- ' demands =~ A1_1 + A1_2 + A1_3 + A1_6 + A1_11 + A1_15 + A1_16 + A1_17
                resources =~ A1_4 + A1_5 + A1_7 + A1_8 + A1_9 + A1_12 + A1_13 + A1_14 + A1_18 '



##modello con stimatore maximum likelihood

demres_cfa_ml <- cfa(dem_res_model, 
                  data=insula.norm,
                  estimator = "ML")

summary(demres_cfa_ml, fit.measures=TRUE)


##default modello utilizza ML. nel caso di variabili discrete e ordinali come likert potrebbe non essere la scelta migliore.
##ML utilizza correlazioni di Pearson, nel likert meglio utilizzare correlazioni polychoric.
##lavaan mette a disposizione diversi stimatori:
##GLS (minimi quadrati generalizzati): quando sussiste correlazione tra errori di modello
##WLS (minimi quadrati pesati): quando la distanza media tra y osservate e predette è troppo grande (errore quadratico medio) allora ipotizzo che
##la distanza tra le singole y osservate e predette non sia trascurabile e allora assegno a ogni osservazione un peso wi=1/(sigmayi)^2.
##questi pesi vengono poi inseriti nella stima dei parametri quali intercetta e coefficienti angolari (beta) 
##https://www2.pd.infn.it/~montag/didattica/ottica/errori_04.pdf
##https://www.no-regime.com/ru-it/wiki/Generalized_least_squares

##WLS: 

##modello con stimatore unweighted least squares

demres_cfa_uls <- cfa(dem_res_model, 
                  data=insula.norm,
                  estimator = "ULS",
                  check.gradient = FALSE)

summary(demres_cfa_uls, fit.measures=TRUE)


##modello con stimatore unweighted least squares e varianti robuste


demres_cfa_ulsmv <- cfa(dem_res_model, 
                      data=insula.norm,
                      estimator = "ULSMV",
                      check.gradient = FALSE)

summary(demres_cfa_ulsmv, fit.measures=TRUE)



demres_cfa_ulsmvs <- cfa(dem_res_model, 
                      data=insula.norm,
                      estimator = "ULSMVS",
                      check.gradient = FALSE)

summary(demres_cfa_ulsmvs, fit.measures=TRUE)




##seguendo l'articolo, verifichiamo la consistenza interna degli indicatori
##utilizziamo due metodi per alpha di cronbach. uno più semplice (https://www.statology.org/cronbachs-alpha-in-r/), 
##altro che restituisce anche il valore di alpha se si escludesse quel singolo item (https://www.youtube.com/watch?v=k2JLXBOG7p4)

library(ltm)
library(psych)

##prima selezioniamo le variabili di interesse per demands
##https://www.r-bloggers.com/2016/11/5-ways-to-subset-a-data-frame-in-r/

insula.demands <- data.frame(insula.norm[,c(2:4, 7, 12, 16:18)])


cronbach.alpha(insula.demands)

alpha(insula.demands)


##resources

insula.resources <- data.frame(insula.norm[,c(5:6, 8:10 , 13:15, 19)])


cronbach.alpha(insula.resources)

alpha(insula.resources)

##well being


insula.wbeing <- data.frame(insula.norm[,c(27:31)])


cronbach.alpha(insula.wbeing)

alpha(insula.wbeing)

##creiamo le variabili demands e resources

library(dplyr)

insula.norm <-  mutate(
                insula.norm, 
                demands = rowMeans
                (x = insula.norm[,c(2:4, 7, 12, 16:18)]))



insula.norm <-  mutate(
                insula.norm, 
                resources = rowMeans
                (x = insula.norm[,c(5:6, 8:10 , 13:15, 19)]))



##prima di fare il modello vero e proprio

##https://lavaan.ugent.be/tutorial/est.html

##variabili

A8_ateco_dat

demands

resources

B5depres_bin


library(lavaan)

sem.insula <- 'demands ~ A8_ateco_dat
              resources ~ A8_ateco_dat
              B5depres_bin ~ A8_ateco_dat
              B5depres_bin ~ demands
              B5depres_bin ~ resources
              demands ~~ resources'



fit.insula <- sem(sem.insula, data = insula.norm)
              summary(fit.insula, standardized = TRUE,
                      likelihood = "MLR")
