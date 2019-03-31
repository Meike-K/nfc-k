################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# Teil 1: Kurzskalenselektion
################################################################################
# I.1 Berechnung des Full Information Approach
################################################################################


# Einstellungen, Pakete und Daten
################################################################################
if(!require("psych")){
  install.packages("psych")
}
if(!require("GPArotation")){
  install.packages("GPArotation")
}
if(!require("Rcsdp")){
  install.packages("Rcsdp")
}
if(!require("lm.beta")){
  install.packages("lm.beta")
}
if(!require("foreign")){
  install.packages("foreign")
}

setwd("2_Full_Information_Approach/")

# Datenaufbereitung
################################################################################
# SPSS-Datensatz: "0_Daten/Datensatz der Selektion.sav"
# - N = 282
# - Alle Variablen
# - Alle berechneten Skalen
# - Subjekt-ID: Teilnehmer

raw_data <- read.spss("../0_Daten/Datensatz der Selektion.sav", 
                      to.data.frame=TRUE, use.value.labels = FALSE)
head(raw_data)


##### Übersichtsstatistiken
round(cor(raw_data[, c("NFC", "TIE", "Deliberation", "Offenheit", "Lernziel_Gesamt", "SozErwuenscht")], 
          use="pairwise.complete.obs"), 3)


##### Daten in .txt speichern als MPlus Datenquelle
selected_variables <- c(
  # NFC-Skala
  "v_95", "v_96", "v_97", "v_98r", "v_25", "v_26r", "v_27r", "v_28r",
  "v_253r", "v_254r", "v_255r", "v_256r", "v_257", "v_258", "v_259r", "v_260r",
  "TIE", "Deliberation", "Offenheit", "Lernziel_Gesamt", "SozErwuenscht",
  "Bildungkategorie", "Geschlecht")

data <- raw_data[, selected_variables]
data[is.na(data)] <- -77
write.table(data, "Zwischenergebnisse/NFC_data.txt", col.names = FALSE, row.names = FALSE)

# Datenwerden auch zur Messinvarianzanalyse abgelegt
write.table(data, "../3_Sensitivitätsanalyse/Zwischenergebnisse/NFC_data.txt", col.names = FALSE, row.names = FALSE)



# 1) Population Grid der zu testenden Skalen berechnen
################################################################################

# Set aller möglichen Itemkombinationen
items.long.scale		<- 16 # items in NFC scale
population.matrix		<- as.matrix(expand.grid(lapply(numeric(items.long.scale), function(x) c(0, 1))))
variable.names	<- c("v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r","v10r","v11r","v12r","v13","v14","v15r","v16r")
# TODO: Selektion final raus und ids umschreiben
population.matrix		<- population.matrix[-c(which(rowSums(population.matrix) < 3)),]

# Speichern und Lesen
write.table(population.matrix, "Zwischenergebnisse/population.matrix.txt", col.names = F, row.names = F)
population.matrix		<- read.table("Zwischenergebnisse/population.matrix.txt")


population.matrix		<- population.matrix[-c(which(rowSums(population.matrix) > 7)),]
# TODO: Further selection of runs (TEMPORARY)
population.matrix		<- population.matrix[-c(which(rowSums(population.matrix) > 5)),]	

# Remove scales with negative degrees of freedom for the correlated error terms model
n <- rowSums(population.matrix)
n_r <- rowSums(population.matrix[, c(4, 6, 7, 8, 9, 10, 11, 12, 15, 16)])


# Function to compute degrees of freedom
calc_df <- function(n, n_r){
  # n: number of items
  # n_r: number of negatively worded items
  free_params <- 0.5*n*(n+1)
  cov_params <- 0.5* n_r*abs(n_r - 1)
  est_params <- n + n + cov_params
  df <- free_params - est_params
  return(df)
}
dfs <- calc_df(n, n_r)

# remove all scales with negative degrees of freedom
population.matrix <- population.matrix[dfs >= 0,]
population			<- nrow(population.matrix)




# 2) Fit Statistiken für alle NFC-Kurzformen berechnen 
################################################################################
compute_fi <- function(population.matrix, 
                       correlated_errors = FALSE,
                       file_in, 
                       stats_out, 
                       extract_output = TRUE,
                       out_name = "",
                       mplus_path = NULL){
  
  # Berechnet die Fit_statistiken aller Kurzskalen in einer Schleife mittels
  # MPlus und aggregiert die Ergebnisse innerhalb R 
  # Input-Syntax muss Faktorvarianz auf 1 gefixt haben
  # 
  # Parameter: 
  # - population.matrix: matris aller zu testenden Itemkombinationen
  # - file_in: Namen der Input-Syntax aus MPlus (muss im gleichen Ordner liegen) 
  # - stats_out: Namen des aggregierten Statistik-Outputs
  
  population <- nrow(population.matrix)
  statistics		<- matrix(NA, 1, 8)
  
  for (K in 1:population) {
    
    # I) Input-Syntax anpassen
    input             <- scan(file = paste(file_in, "_base", ".inp", sep=""), 
                              what = "character", sep ="\n", quote = NULL)
    
    
    # Anpassung USEVARIABLES
    position1         <- grep("usevariables", input, ignore.case = TRUE)
    input[position1] 	<- paste("usevariables are ")
    items.sel 		  <- variable.names[which(population.matrix[K,] == 1)]
    input[position1+1] 	<- paste(c(items.sel, ";"), collapse = " ") 
    factor.position   	<- grep(paste("NFC","BY"), input, ignore.case = TRUE)
    input[factor.position]  <- paste(c("NFC BY", items.sel[1], "*", items.sel[-1], ";"), collapse =" ")
    
    # Spezifikation der korrelierten Fehlerterme (falls vorhanden) und Aktualisierung der Faktorvarianz
    if(correlated_errors){
      negative.worded		      <- items.sel[grep("r", items.sel)]
      with.position	 	        <- grep(paste("WITH"), input, ignore.case = TRUE)
      if (identical(with.position,integer(0))) with.position <- factor.position + 1
      input[with.position]	  <- paste(" ", collapse = " ")
      at.position		 	        <- grep(paste("@"), input, ignore.case = TRUE)
      input[at.position]	    <- paste(" ", collapse =" ")
      
      if(length(negative.worded) > 1) {
        for (r in 1:(length(negative.worded) - 1)){
          temp 		<- negative.worded[c((r+1):length(negative.worded))]
          input[factor.position+r] <- paste(c(negative.worded[r], "WITH", temp, ";"), collapse=" ")
        }
        input[max(with.position)+r] <- paste(c("NFC @1;"))
      }
      
      if(length(negative.worded)<=1) {
        input[with.position]  <- paste(" ",collapse =" ")
        input[max(with.position)+1] <- paste(c("NFC @1;"))
      }
    }
    
    input[is.na(input)] 	 <- paste(" ", collapse = " ")
    print(file_in)
    write.table(input, file = paste(file_in, ".inp", sep = ""), 
                quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    # II) MPLUS Schätzung laufen lassen mit spezifizierter Syntax
    if(.Platform$OS.type == "windows"){
      # Windows
      if(is.null(mplus_path)){
        mplus_path <- "mplus.exe"
      }
      system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, out_name, ".out", sep=""), wait = TRUE)
      
    } else {
      if(tolower(Sys.info()["sysname"]) == "linux"){
        # Linux
        if(is.null(mplus_path)){
          mplus_path <- "/opt/mplusdemo/mpdemo"
        }
        system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, out_name, ".out", sep=""), wait = TRUE)
        
        
      } else {
        # Mac
        if(is.null(mplus_path)){
          mplus_path <- "/Applications/Mplus/mplus"
        }
        system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, out_name, ".out", sep=""), wait = TRUE) 
      }
    }
    
    if(extract_output){
      # III) Ergebnisse des MPlus-Outputs einsammeln 
      output 			<- scan(file = paste(file_in, ".out", sep=""),
                        what = "character", sep ="\n", quote = NULL)
      
      position1         <- grep("Chi-Square Test of Model Fit", output, ignore.case = TRUE)
      statistics[1,1]		<- as.numeric(substr(output[position1[1]+1], 40, nchar(output[position1[1]+1])))
      statistics[1,2]		<- as.numeric(substr(output[position1[1]+2], 40, nchar(output[position1[1]+2])))
      statistics[1,3]		<- as.numeric(substr(output[position1[1]+3], 40, nchar(output[position1[1]+3])))
      
      position1         <- grep("cfi", output, ignore.case = TRUE)
      statistics[1,4]		<- as.numeric(substr(output[position1[2]], 16, nchar(output[position1[2]])))
      position1          <- grep("tli", output, ignore.case = TRUE)
      statistics[1,5]		<- as.numeric(substr(output[position1[2]], 16, nchar(output[position1[2]])))
      position1         <- grep("rmsea", output, ignore.case = TRUE) + 1
      statistics[1,6]		<- as.numeric(substr(output[position1[1]], 20, nchar(output[position1[1]])))
      position1         <- grep("AIC", output, ignore.case = TRUE)
      statistics[1,7]		<- as.numeric(substr(output[position1[1]], 25, nchar(output[position1[1]])))
      statistics[1,8]		<- as.numeric(substr(output[position1[1] + 1], 25, nchar(output[position1[1]])))
      position1         <- grep("Chi-Square Test of Model Fit", output, ignore.case = TRUE)
      
      output 			<- scan(file = paste(file_in, ".out", sep=""),
                        what = "character", sep ="\n", quote = NULL)
      
      id <- as.numeric(rownames(population.matrix)[K])
      
      write.table(cbind(id, statistics), stats_out, 
                  col.names = FALSE, row.names = FALSE, append=TRUE)
      
    }
  }
}


###### 3a) Modell ohne korrelierte Fehlerterme
compute_fi(population.matrix, file_in = "Zwischenergebnisse/input_uncorr", correlated_errors = FALSE,
           stats_out = "Zwischenergebnisse/scales_uncorrelated.txt")

###### 3b) Modell mit korrelierten Fehlertermen
compute_fi(population.matrix, file_in = "Zwischenergebnisse/input_corr", correlated_errors = TRUE,
           stats_out = "Zwischenergebnisse/scales_correlated.txt")



# 4) Reliabilitäts- und Validitätskennzahlen für alle Kurzskalen berechnen
################################################################################

# Datensatz für die Population aller Kurzskalen
data.chromosomes	<- list()
NFC <- raw_data[, c("v_95", "v_96", "v_97", "v_98r", "v_25", "v_26r", "v_27r", 
                "v_28r", "v_253r", "v_254r", "v_255r", "v_256r", "v_257", 
                "v_258", "v_259r", "v_260r")]
for (pop in 1:population) {	
		data.chromosomes[[pop]] <- as.matrix(NFC[, which(population.matrix[pop,] == 1)])
}

# Set-up für batchweise Berechnung
batchsize <- min(nrow(population.matrix), 5000)
n_batches <- (population %/% batchsize) + 1
batch 	<- matrix(NA, n_batches, 2)

batch[1, ] <- c(1, batchsize)
for (i in 2:(n_batches - 1)){
  batch[i, ] <- c(batchsize*(i-1) + 1, batchsize*i)
}
batch[i+1, ]   <- c(batchsize*i + 1, population)


items		<- rowSums(population.matrix)
negative	<- rowSums(population.matrix[, c(4, 6, 7, 8, 9, 10, 11, 12, 15, 16)])


##### Mittlere Item-Rest-Korrelation & Cronbachs alpha
for (b in 1:nrow(batch)) {		
    print(paste("Computing Batch ", b))
  
		a		<- data.chromosomes[c(batch[b, 1]:batch[b, 2])]
		temp		<- lapply(a, function(a) alpha(a, use="complete.obs"))
		item.rest	<- lapply(temp, `[[`, 3)
		item.rest	<- lapply(item.rest, `[[`, 5)
		item.rest	<- as.numeric(lapply(item.rest, mean))
		alpha		<- lapply(temp, `[[`, 1)
		alpha		<- lapply(alpha, `[[`, 1)
		alpha		<- as.numeric(alpha)

		statistics2	<- cbind(items[c(batch[b, 1]:batch[b, 2])], negative[c(batch[b, 1]:batch[b, 2])],
		                     item.rest, alpha)
		statistics2	<- round(statistics2, 4)
		write.table(statistics2, "Zwischenergebnisse/alpha.txt", 
		            col.names = FALSE, row.names = FALSE, append=TRUE)

}


##### Inter-scale Korrelationen
for (pop in 1:population){
	temp <- round(cor(cbind(rowMeans(data.chromosomes[[pop]]), 
	                        raw_data[, c("TIE", "Deliberation", "Offenheit", "Lernziel_Gesamt", "SozErwuenscht")]),
	                  use = "complete.obs"), 4)[1, ]
	write.table(cbind(pop, matrix(temp, 1, 6)), "Zwischenergebnisse/correlations.txt", 
	            col.names = FALSE, row.names = FALSE, append=TRUE)
}


##### Reliabilität: GLB 
for(pop in 1:population) {
		a		<- data.chromosomes[pop]
		glb		<- lapply(a, function(a) cov(a, use="complete.obs"))
		glb		<- lapply(glb, function(glb) glb.algebraic(glb))
		glb		<- as.numeric(lapply(glb, `[[`, 1))
		write.table(cbind(pop, glb), "Zwischenergebnisse/glb.txt", 
		            col.names = FALSE, row.names = FALSE, append=TRUE)

}

##### Reliabilität: Omega
results <- matrix(NA, population, 2)

for(pop in 1:population) {
  data	<- data.chromosomes[pop]
  data  <- matrix(unlist(data[[1]]), nrow(data[[1]]))

  temp1  <- omega(data, nfactors = 1, plot = FALSE)
  results[pop, 1] <- unlist(temp1[4])
}

write.table(results, "Zwischenergebnisse/omega.txt", col.names = FALSE, row.names = FALSE)


###### Aggregieren der verschiedenen Statistiken
scales_uncorr <- read.table("Zwischenergebnisse/scales_uncorrelated.txt")
scales_corr <- read.table("Zwischenergebnisse/scales_correlated.txt")
alpha <- read.table("Zwischenergebnisse/alpha.txt")
correlations <- read.table("Zwischenergebnisse/correlations.txt")
glb <- read.table("Zwischenergebnisse/glb.txt")
omega <- read.table("Zwischenergebnisse/omega.txt")

results	<- cbind(scales_corr[, 1], items, negative, 
                 population.matrix,
                 glb[, 2], alpha[, 4], alpha[, 3], omega[, 1],
                 correlations[, c(3:7)],
                 scales_corr[, c(2:9)],
                 scales_uncorr[, c(2:9)])

colnames(results) <- c("id","no.items","negativ.worded",
                       "v_95", "v_96", "v_97", "v_98r", "v_25", "v_26r", "v_27r", "v_28r", 
                       "v_253r", "v_254r", "v_255r", "v_256r", "v_257", "v_258", "v_259r", "v_260r",
                       "glb","alpha", "mean_item_rest_cor", "omega", 
                       "corTIE", "corDELIB", "corOPEN", "corLEARN", "corSOCDES",
                       "chi2_korr", "df_korr", "p_korr",
                       "CFI_korr", "TLI_korr", "RMSEA_korr", "AIC_korr", "BIC_korr",
                       "chi2_unkorr", "df_unkorr", "p_unkorr",
                       "CFI_unkorr", "TLI_unkorr", "RMSEA_unkorr", "AIC_unkorr", "BIC_unkorr")


# Aufbereitung der Ergebnisse
################################################################################
results$mean_cor <- apply(results[, c("corTIE", "corDELIB", "corOPEN", "corLEARN")], 1, mean)


###### Korrelation der Langform mit verwandten Konstrukten
# TIE
corr_tie <- cor(raw_data$NFC, raw_data$TIE, use = "pairwise.complete.obs")
results$diff_tie <- results$corTIE - corr_tie

# Präferenz für Deliberation
corr_delib <- cor(raw_data$NFC, raw_data$Deliberation, use = "pairwise.complete.obs")
results$diff_delib <- results$corDELIB - corr_delib

# Offenheit
corr_open <- cor(raw_data$NFC, raw_data$Offenheit, use = "pairwise.complete.obs")
results$diff_open <- results$corOPEN - corr_open

# Lernzielorientierung
corr_learn <- cor(raw_data$NFC, raw_data$Lernziel_Gesamt, use = "pairwise.complete.obs")
results$diff_learn <- results$corLEARN - corr_learn


# TODO
# Check statistics of the selected short scales
results[results$id %in% c(423, 5226), ]

##### Abspeichern der Ergebnisse
write.table(results, "Zwischenergebnisse/Full_Information_Statistiken.txt",
            col.names = TRUE, row.names = FALSE)
save(results, file = "Zwischenergebnisse/Full_Information_Statistiken.RData")

setwd("..")


