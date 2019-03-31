################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# Teil 1: Kurzskalenselektion
################################################################################
# I.4 Berechnung der Sensitivitätsanalyse Messinvarianz
################################################################################


# Einstellungen, Pakete und Daten
################################################################################
setwd("3_Sensitivitätsanalyse")


# 1) Laden und Rgenderktion der Population Matrix
################################################################################
# Nur MI test der vorab selektierten harten Kriterien:

# Laden der Populationsmatrix
population.matrix		<- read.table("../2_Full_Information_Approach/Zwischenergebnisse/population.matrix.txt")
variable.names	<- c("v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r","v10r","v11r","v12r","v13","v14","v15r","v16r")

# Langskala
longform <- population.matrix[nrow(population.matrix),]


# Skalenauswahl aus Full Information Approach
load("../2_Full_Information_Approach/Zwischenergebnisse/Full_Information_Skalenauswahl.RData")
population.matrix <- population.matrix[rownames(population.matrix) %in% fi_results$id, ]
dim(population.matrix)[1] == dim(fi_results)[1]



# 2) Fit Statistiken für alle NFC-Kurzformen berechnen 
################################################################################

compute_mi <- function(population.matrix,
                       correlated_errors = TRUE,
                       file_in = "input_mi_gender",
                       stats_out = "mi_statistics1.txt",
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
  statistics		<- matrix(NA, 1, 42)
  
  for (K in 1:population) {
    
    # I) Input-Syntax anpassen
    input             <- scan(file = paste(file_in, "_base", ".inp", sep=""), 
                              what = "character", sep ="\n", quote = NULL)
    
    
    # Anpassung USEVARIABLES
    position1         <- grep("usevariables", input, ignore.case = TRUE)
    input[position1] 	<- paste("usevariables are ")
    items.sel 		  <- variable.names[which(population.matrix[K,] == 1)]
    input[position1+1] 	<- paste(c(items.sel, ";"), collapse = " ") 
    factor.position   	<- grep(paste("NFC", "BY"), input, ignore.case = TRUE)
    input[factor.position]  <- paste(c("NFC BY", items.sel[1], "*", items.sel[-1], ";"), collapse =" ")
    
    # Spezifikation der korrelierten Fehlerterme (falls vorhanden) und Aktualisierung der Faktorvarianz
    if(correlated_errors){
      negative.worded		      <- items.sel[grep("r", items.sel)]
      with.position	 	        <- grep(paste("WITH"), input, ignore.case = TRUE)
      if (identical(with.position, integer(0))) with.position <- factor.position + 1
      input[with.position]	  <- paste(" ", collapse = " ")
      at.position		 	        <- grep(paste("@"), input, ignore.case = TRUE)
      input[at.position]	    <- paste(" ", collapse = " ")
      
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
      # III) Ergebnisse des MPlus-Outputs "Invariance Testing" einsammeln 
      output 			<- scan(file = paste(file_in, ".out", sep=""),
                        what = "character", sep ="\n", quote = NULL)
      
      # Chi² Configural
      position1           <- grep("Configural", output, ignore.case = TRUE)
      x1                  <- unlist(strsplit(output[position1][2], " "))
      x1                  <- x1[-which(x1=="")]
      if(length(x1)==5){statistics[1, 1:5]  <- x1}
      if(length(x1)<5) {statistics[1, 1:5]  <- rep(NA, 5)}
      
      # Chi² Metric
      position1           <- grep("Metric", output, ignore.case = TRUE)
      x1                  <- unlist(strsplit(output[position1][2], " "))
      x1                  <- x1[-which(x1=="")]
      if(length(x1)==5){statistics[1, 6:10]  <- x1}
      if(length(x1)<5) {statistics[1, 6:10]  <- rep(NA, 5)}
      
      # Chi² Scalar
      position1           <- grep("Scalar", output, ignore.case = TRUE)
      x1                  <- unlist(strsplit(output[position1][2], " "))
      x1                  <- x1[-which(x1=="")]
      if(length(x1)==5){statistics[1, 11:15]  <- x1}
      if(length(x1)<5) {statistics[1, 11:15]  <- rep(NA, 5)}
      
      # Difference tests
      position1           <- grep("Metric against Configural", output, ignore.case = TRUE)
      x1                  <- unlist(strsplit(output[position1], " "))
      x1                  <- x1[-which(x1=="")]
      if(length(x1)==6){statistics[1, 16:21]  <- x1}
      if(length(x1)==0){statistics[1, 16:21]  <- rep(NA, 6)}
      
      position1           <- grep("Scalar against Configural", output, ignore.case = TRUE)
      x1                  <- unlist(strsplit(output[position1], " "))
      x1                  <- x1[-which(x1=="")]
      if(length(x1)==6){statistics[1, 22:27]  <- x1}
      if(length(x1)==0){statistics[1, 22:27]  <- rep(NA, 6)}
      
      position1           <- grep("Scalar against Metric", output, ignore.case = TRUE)
      x1                  <- unlist(strsplit(output[position1], " "))
      x1                  <- x1[-which(x1=="")]
      if(length(x1)==6){statistics[1, 28:33]  <- x1}
      if(length(x1)==0){statistics[1, 28:33]  <- rep(NA, 6)}
      
      position_X          <- grep("MODEL FIT INFORMATION FOR THE CONFIGURAL MODEL",output) 
      position_A          <- grep("MODEL FIT INFORMATION FOR THE METRIC MODEL",output) 
      position_B          <- grep("MODEL FIT INFORMATION FOR THE SCALAR MODEL",output) 
      
      # Fit statistics for the configural model
      if(length(position_X)==1){
        sub.output <- output[c(position_X:(position_X+30))]
        
        position1           <- grep("RMSEA", sub.output, ignore.case = TRUE)
        x1                  <- unlist(strsplit(sub.output[position1+1]," "))
        rmsea_config        <- x1[-which(x1=="")][2]
        statistics[34]      <- rmsea_config
        
        position1           <- grep("CFI", sub.output, ignore.case = TRUE)
        x1                  <- unlist(strsplit(sub.output[position1+1], " "))
        cfi_config          <- x1[-which(x1=="")][2]
        tli_config          <- x1[-which(x1=="")][4]
        statistics[35]      <- cfi_config
        statistics[36]      <- tli_config
      }	  
      
      # Fit statistics for the metric model
      if(length(position_A)==1){
        sub.output <- output[c(position_A:(position_A+30))]
        
        position1           <- grep("RMSEA", sub.output, ignore.case = TRUE)
        x1                  <- unlist(strsplit(sub.output[position1+1], " "))
        rmsea_metric        <- x1[-which(x1=="")][2]
        statistics[37]      <- rmsea_metric
        
        position1           <- grep("CFI", sub.output, ignore.case = TRUE)
        x1                  <- unlist(strsplit(sub.output[position1+1], " "))
        cfi_metric          <- x1[-which(x1=="")][2]
        tli_metric          <- x1[-which(x1=="")][4]
        statistics[38]      <- cfi_metric
        statistics[39]      <- tli_metric
      }
      
      #  Fit statistics for the scalar model    
      if(length(position_B)==1){
        sub.output <- output[c(position_B:(position_B+30))]
        
        position1           <- grep("RMSEA",sub.output,ignore.case=T)
        x1                  <- unlist(strsplit(sub.output[position1+1]," "))
        rmsea_scalar        <- x1[-which(x1=="")][2]
        statistics[40]      <- rmsea_scalar
        
        position1           <- grep("CFI",sub.output,ignore.case=T)
        x1                  <- unlist(strsplit(sub.output[position1+1]," "))
        cfi_scalar         <- x1[-which(x1=="")][2]
        tli_scalar         <- x1[-which(x1=="")][4]  
        statistics[41]      <- cfi_scalar
        statistics[42]      <- tli_scalar
      }      
      
      write.table(cbind(rownames(population.matrix)[K], population.matrix[K,], statistics),
                  stats_out, col.names = FALSE, row.names = FALSE, append=TRUE)
      
    }
  }
}
#TODO:
longform <- population.matrix[1, ]


###### 2a) Messinvarianz bezüglich Geschlecht
# Selektierte Kurzskalen
compute_mi(population.matrix, file_in = "Zwischenergebnisse/input_mi_gender", 
           correlated_errors = TRUE,
           stats_out = "Zwischenergebnisse/mi_gender_scales_correlated.txt")


# Langskala
compute_mi(longform, file_in = "Zwischenergebnisse/input_mi_gender", 
           correlated_errors = TRUE,
           stats_out = "Zwischenergebnisse/mi_gender_longform_correlated.txt")


###### 2b) Messinvarianz bezüglich Bildungslevel
# Kurzskalen
compute_mi(population.matrix, file_in = "Zwischenergebnisse/input_mi_edu", 
           correlated_errors = TRUE,
           stats_out = "Zwischenergebnisse/mi_education_scales_correlated.txt")

# Langskala
compute_mi(longform, file_in = "Zwischenergebnisse/input_mi_edu", 
           correlated_errors = TRUE,
           stats_out = "Zwischenergebnisse/mi_education_longform_correlated.txt")


# 3) Aufbereiten der Ergebnisse
################################################################################
col_names <- c("id","v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r",
                  "v10r","v11r","v12r","v13","v14","v15r","v16r",
                  "Configural","params_config","chi2_config","df_config","p_config",
                  "Metric","params_metric","chi2_metric","df_metric","p_metric",
                  "Scalar","params_scalar","chi2_scalar","df_scalar","p_scalar",
                  "Metric","against","Configural",
                  "d_chi2_metric","d_df_metric","d_p_metric",
                  "Scalar","against","Configural","d_chi2_scalar2",
                  "d_df_scalar2","d_p_scalar2",
                  "Scalar","against","Metric",
                  "d_chi2_scalar","d_df_scalar","d_p_scalar",
                  "rmsea_config","cfi_config","tli_config",
                  "rmsea_metric","cfi_metric","tli_metric",
                  "rmsea_scalar","cfi_scalar","tli_scalar")

col_selection <- c("id","v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r",
                   "v10r","v11r","v12r","v13","v14","v15r","v16r",
                   "params_config","chi2_config","df_config","p_config",
                   "params_metric","chi2_metric","df_metric","p_metric",
                   "params_scalar","chi2_scalar","df_scalar","p_scalar",
                   "d_chi2_metric","d_df_metric","d_p_metric",
                   "d_chi2_scalar2","d_df_scalar2","d_p_scalar2",
                   "d_chi2_scalar","d_df_scalar","d_p_scalar",
                   "rmsea_config","cfi_config","tli_config",
                   "rmsea_metric","cfi_metric","tli_metric",
                   "rmsea_scalar","cfi_scalar","tli_scalar")

col_selection_red <- c("id",
                   "params_config","chi2_config","df_config","p_config",
                   "params_metric","chi2_metric","df_metric","p_metric",
                   "params_scalar","chi2_scalar","df_scalar","p_scalar",
                   "d_chi2_metric","d_df_metric","d_p_metric",
                   "d_chi2_scalar2","d_df_scalar2","d_p_scalar2",
                   "d_chi2_scalar","d_df_scalar","d_p_scalar",
                   "rmsea_config","cfi_config","tli_config",
                   "rmsea_metric","cfi_metric","tli_metric",
                   "rmsea_scalar","cfi_scalar","tli_scalar")


##### Aggregation der Datensätze

# Lesen und Aufbereiten der einzelnen Datensätze
mi_statistics_gender    <- read.table("Zwischenergebnisse/mi_gender_scales_correlated.txt")
colnames(mi_statistics_gender) <- col_names
mi_statistics_gender <- mi_statistics_gender[, col_selection]

mi_statistics_edu    <- read.table("Zwischenergebnisse/mi_education_scales_correlated.txt")
colnames(mi_statistics_edu ) <- col_names
mi_statistics_edu <- mi_statistics_edu[, col_selection_red]
mi_statistics_shortscales <- merge(mi_statistics_gender, mi_statistics_edu, by = "id",
                       suffixes = c("_gender", "_edu"))

# Lesen und Aufbereiten der Langformberechnung
mi_statistics_gender_longform    <- read.table("Zwischenergebnisse/mi_gender_longform_correlated.txt")
colnames(mi_statistics_gender_longform) <- col_names
mi_statistics_gender_longform <- mi_statistics_gender_longform[, col_selection]

mi_statistics_edu_longform    <- read.table("Zwischenergebnisse/mi_education_longform_correlated.txt")
colnames(mi_statistics_edu_longform) <- col_names
mi_statistics_edu_longform <- mi_statistics_edu_longform[, col_selection_red]

mi_statistics_longform <- merge(mi_statistics_gender_longform, mi_statistics_edu_longform, 
                                by = "id", suffixes = c("_gender", "_edu"))

# Zusammenfügen von Langformen und Kurzform
mi_statistics <- rbind(mi_statistics_shortscales, mi_statistics_longform)


### Messinvarianz zwischen Bildungsgruppen (gering, mittel, hoch)
mi_statistics$d_rmsea_metric_edu <- mi_statistics$rmsea_metric_edu -  mi_statistics$rmsea_config_edu
mi_statistics$d_rmsea_scalar_edu <- mi_statistics$rmsea_scalar_edu - mi_statistics$rmsea_metric_edu

mi_statistics$d_cfi_metric_edu <- mi_statistics$cfi_config_edu - mi_statistics$cfi_metric_edu
mi_statistics$d_cfi_scalar_edu <- mi_statistics$cfi_metric_edu - mi_statistics$cfi_scalar_edu


# Messinvarianz nach Geschlecht
mi_statistics$d_rmsea_metric_gender <- mi_statistics$rmsea_metric_gender -  mi_statistics$rmsea_config_gender
mi_statistics$d_rmsea_scalar_gender <- mi_statistics$rmsea_scalar_gender - mi_statistics$rmsea_metric_gender

mi_statistics$d_cfi_metric_gender <- mi_statistics$cfi_config_gender - mi_statistics$cfi_metric_gender
mi_statistics$d_cfi_scalar_gender <- mi_statistics$cfi_metric_gender - mi_statistics$cfi_scalar_gender



selected_variables <- c("id", "v1", "v2", "v3", "v4r", "v5", "v6r", "v7r", "v8r", 
                        "v9r", "v10r", "v11r", "v12r", "v13", "v14", "v15r", "v16r", 
                        "chi2_config_gender", "df_config_gender", "p_config_gender",
                        "chi2_metric_gender", "df_metric_gender", "p_metric_gender",
                        "chi2_scalar_gender", "df_scalar_gender", "p_scalar_gender",
                        "d_chi2_metric_gender", "d_df_metric_gender", "d_p_metric_gender",    
                        "d_chi2_scalar_gender", "d_df_scalar_gender", "d_p_scalar_gender",
                        "rmsea_config_gender", "cfi_config_gender", "tli_config_gender",
                        "rmsea_metric_gender", "cfi_metric_gender", "tli_metric_gender",    
                        "rmsea_scalar_gender", "cfi_scalar_gender", "tli_scalar_gender", 
                        "d_rmsea_metric_gender", "d_rmsea_scalar_gender", "d_cfi_metric_gender", 
                        "d_cfi_scalar_gender",
                        "chi2_config_edu", "df_config_edu", "p_config_edu",
                        "chi2_metric_edu", "df_metric_edu", "p_metric_edu",
                        "chi2_scalar_edu", "df_scalar_edu", "p_scalar_edu",
                        "d_chi2_metric_edu", "d_df_metric_edu", "d_p_metric_edu",    
                        "d_chi2_scalar_edu", "d_df_scalar_edu", "d_p_scalar_edu",
                        "rmsea_config_edu", "cfi_config_edu", "tli_config_edu",
                        "rmsea_metric_edu", "cfi_metric_edu", "tli_metric_edu",    
                        "rmsea_scalar_edu", "cfi_scalar_edu", "tli_scalar_edu",
                        "d_rmsea_metric_edu", "d_rmsea_scalar_edu", "d_cfi_metric_edu", 
                        "d_cfi_scalar_edu")
mi_statistics <- mi_statistics[, selected_variables]


# 4) Berechnungen
################################################################################
### Anzahl Fehler
models <- c("chi2_config_gender", "chi2_metric_gender", "chi2_scalar_gender",
            "chi2_config_edu", "chi2_metric_edu", "chi2_scalar_edu")
for (model in models){
  print(paste0("Number of NAs in ", model, ": ", sum(is.na(mi_statistics[[model]]))))
}


n <- nrow(mi_statistics)
selection <-  mi_statistics[mi_statistics$id %in% final_scales, ]

##### Vergleich zwischen Anteil von Skalen mit erfüllter Messinvarianz und 
# den selektierten SUbskalen

### Geschlecht
# Metric
round(sum(mi_statistics$d_p_metric_gender > 0.05)/n, 2)
selection$d_p_metric_gender > 0.05

round(sum(mi_statistics$d_cfi_metric_gender < 0.01)/n, 2)
selection$d_cfi_metric_gender < 0.01

round(sum(mi_statistics$d_rmsea_metric_gender < 0.01)/n, 2)
selection$d_rmsea_metric_gender < 0.01


# Scalar
round(sum(mi_statistics$d_cfi_scalar_gender < 0.01)/n, 2)
selection$d_cfi_scalar_gender < 0.01

round(sum(mi_statistics$d_p_scalar_gender > 0.05)/n, 2)
selection$d_p_scalar_gender > 0.05

round(sum(mi_statistics$d_rmsea_scalar_gender < 0.01)/n, 2)
selection$d_rmsea_scalar_gender < 0.01


### Bildung
# Metric
round(sum(mi_statistics$d_p_metric_edu > 0.05, na.rm = TRUE)/(n-1), 2)
selection$d_p_metric_edu > 0.05

round(sum(mi_statistics$d_cfi_metric_edu < 0.01)/n, 2)
selection$d_cfi_metric_edu < 0.01

round(sum(mi_statistics$d_rmsea_metric_edu < 0.01)/n, 2)
selection$d_rmsea_metric_edu < 0.01

# Scalar
round(sum(mi_statistics$d_p_scalar_edu > 0.05)/n, 2)
selection$d_p_scalar_edu > 0.05

round(sum(mi_statistics$d_cfi_scalar_edu < 0.01)/n, 2)
selection$d_cfi_scalar_edu < 0.01

round(sum(mi_statistics$d_rmsea_scalar_edu < 0.01)/n, 2)
selection$d_rmsea_scalar_edu < 0.01


##### Wie viele Skalen haben alle Kriterien erfüllt
### Bildung
sum((mi_statistics$d_cfi_metric_edu < 0.01) & 
    (mi_statistics$d_cfi_metric_edu < 0.01) & 
    (mi_statistics$d_p_metric_edu > 0.05) & 
    (mi_statistics$d_cfi_scalar_edu < 0.01) & 
    (mi_statistics$d_p_scalar_edu > 0.05) &
    (mi_statistics$d_cfi_metric_gender < 0.01) & 
    (mi_statistics$d_cfi_metric_gender < 0.01) & 
    (mi_statistics$d_p_metric_gender > 0.05) & 
    (mi_statistics$d_cfi_scalar_gender < 0.01) & 
    (mi_statistics$d_p_scalar_edu > 0.05))/n


##### Output
write.csv(mi_statistics, file="Measurement_Invariance_Skalenauswahl.csv", row.names = FALSE)

selection <-  mi_statistics[mi_statistics$id %in% final_scales, ]
write.csv(selection, file="Measurement_Invariance_selektierte_Skalen.csv", row.names = FALSE)

setwd("..")
