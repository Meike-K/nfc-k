################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# Teil 1: Kurzskalenselektion
################################################################################
# I.5 Berechnung der Sensitivitätsanalyse MLR-Schätzer
################################################################################


# Einstellungen, Pakete und Daten
################################################################################
setwd("3_Sensitivitätsanalyse")



# Laden und Reduktion der Population Matrix
################################################################################
# MLR wird nur auf den besten Skalen berechnet, basierend auf:
# - Skalen mit 4 oder 5 Items
# - Skalen mit mindestens 25% und maximal 75% verneinter Items
# - Skalen mit Cronbachs alpha und GLB >= 0.7
# - Skalen mit Mindestfit CFI >= 0.95
# - Skalen ohne Item v_96 (Item 2), da zu komplex formuliert

# Laden der Populationsmatrix
population.matrix		<- read.table("../2_Full_Information_Approach/Zwischenergebnisse/population.matrix.txt")
variable.names	<- c("v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r","v10r","v11r","v12r","v13","v14","v15r","v16r")

# Skalenauswahl aus Full Information Approach
load("../2_Full_Information_Approach/Zwischenergebnisse/Full_Information_Skalenauswahl.RData")
population.matrix <- population.matrix[rownames(population.matrix) %in% fi_results$id, ]
dim(population.matrix)[1] == dim(fi_results)[1]



# Fit Statistiken für alle NFC-Kurzformen berechnen 
################################################################################
compute_fi <- function(population.matrix, correlated_errors = FALSE,
                       file_in, stats_out, mplus_path = NULL){
  
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
      system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, ".out", sep=""), wait = TRUE)
      
    } else {
      if(tolower(Sys.info()["sysname"]) == "linux"){
        # Linux
        if(is.null(mplus_path)){
          mplus_path <- "/opt/mplusdemo/mpdemo"
        }
        system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, ".out", sep=""), wait = TRUE)
        
        
      } else {
        # Mac
        if(is.null(mplus_path)){
          mplus_path <- "/Applications/Mplus/mplus"
        }
        system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, ".out", sep=""), wait = TRUE) 
      }
    }
    
    
    # III) Ergebnisse des MPlus-Outputs einsammeln 
    output 			<- scan(file = paste(file_in, ".out", sep=""),
                      what = "character", sep ="\n", quote = NULL)
    
    position1         <- grep("Chi-Square Test of Model Fit", output, ignore.case = TRUE)
    statistics[1,1]		<- as.numeric(sub("\\*", "", substr(output[position1[1]+1], 40, nchar(output[position1[1]+1]))))
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


###### Modell ohne korrelierte Fehlerterme
compute_fi(population.matrix, file_in = "Zwischenergebnisse/input_uncorr_MLR", correlated_errors = FALSE,
           stats_out = "Zwischenergebnisse/scales_uncorrelated_MLR.txt")

###### Modell mit korrelierten Fehlertermen
compute_fi(population.matrix, file_in = "Zwischenergebnisse/input_corr_MLR", correlated_errors = TRUE,
           stats_out = "Zwischenergebnisse/scales_correlated_MLR.txt")


# Datenaufbereitung 
################################################################################
scales_uncorr <- read.table("Zwischenergebnisse/scales_uncorrelated_MLR.txt")
scales_corr <- read.table("Zwischenergebnisse/scales_correlated_MLR.txt")

results	<- cbind(scales_corr[, 1],  
                 population.matrix,
                 scales_corr[, c(2:9)],
                 scales_uncorr[, c(2:9)])

colnames(results) <- c("id",
                       "v_95", "v_96", "v_97", "v_98r", "v_25", "v_26r", "v_27r", "v_28r", 
                       "v_253r", "v_254r", "v_255r", "v_256r", "v_257", "v_258", "v_259r", "v_260r",
                       "chi2_korr", "df_korr", "p_korr",
                       "CFI_korr", "TLI_korr", "RMSEA_korr", "AIC_korr", "BIC_korr",
                       "chi2_unkorr", "df_unkorr", "p_unkorr",
                       "CFI_unkorr", "TLI_unkorr", "RMSEA_unkorr", "AIC_unkorr", "BIC_unkorr")################################################################################
nrow(results)



# Vergleich zwischen ML und MLR
################################################################################
agg_results <- merge(fi_results, results, by = "id", suffixes = c("_ML", "_MLR")) 


###### Plots zum Vergleich der Fit-Statistiken
pdf("MLR_vs_ML.pdf")
par(mfrow=c(2, 2))
plot(agg_results$CFI_korr_MLR, agg_results$CFI_korr_ML,
     xlab = "CFIkorr MLR",  ylab = "CFIkorr ML", main = "MLR vs. ML (korrelierte Fehler)")
plot(agg_results$CFI_unkorr_MLR, agg_results$CFI_unkorr_ML,
     xlab = "CFIunkorr MLR",  ylab = "CFIunkorr ML", main = "MLR vs. ML (unkorrelierte Fehler)")
plot(agg_results$RMSEA_korr_MLR, agg_results$RMSEA_korr_ML,
     xlab = "RMSEAkorr MLR",  ylab = "RMSEAkorr ML", main = "MLR vs. ML (unkorrelierte Fehler)")
plot(agg_results$RMSEA_unkorr_MLR, agg_results$RMSEA_unkorr_ML,
     xlab = "RMSEAunkorr MLR",  ylab = "RMSEAunkorr ML", main = "MLR vs. ML (unkorrelierte Fehler)")
dev.off() 

# Was ist der höchste RMSEA?
max(agg_results$RMSEA_korr_MLR[agg_results$RMSEA_korr_ML < 0.05])
max(agg_results$RMSEA_unkorr_MLR[agg_results$RMSEA_unkorr_ML < 0.05])

sum(agg_results$CFI_korr_MLR > 0.95)
sum(agg_results$CFI_unkorr_MLR > 0.95)



##### Abspeichern der Ergebnisse
write.csv(results, file="MLR_Skalenauswahl.csv", row.names = FALSE)

selection <-  results[results$id %in% final_scales, ]
write.csv(selection, file="MLR_selektierte_Skalen.csv", row.names = FALSE)



setwd("..")