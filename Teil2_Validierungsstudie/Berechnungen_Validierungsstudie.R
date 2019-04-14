################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# Teil 2: Kurzskalenvalidierung
################################################################################



# Einstellungen und Pakete
################################################################################
#!# Anpassen des aktuellen Arbeitsverzeichnisses
path <- "Teil2_Selektionsstudie/"
setwd(path)


#!# Anpassen des Pfades zum MPLus-Programm, z.B.
# Windows: "mplus.exe"
# Linux: "/opt/mplusdemo/mpdemo"
# Mac: "/Applications/Mplus/mplus"
mplus_path <<- "/opt/mplusdemo/mpdemo"


# Notwendige Pakete
if(!require("foreign")){
  install.packages("foreign")
}
if(!require("cocron")){
  install.packages("cocron")
}
if(!require("psych")){
  install.packages("cocron")
}
if(!require("Hmisc")){
  install.packages("Hmisc")
}



# Datenaufbereitung
################################################################################
# SPSS-Datensatz: "0_Daten/Datensatz der Validierung.sav"
# - N = 530
# - Alle Variablen
# - Alle berechneten Skalen
# - Subjekt-ID: lfdn

data <- read.spss("0_Daten/Datensatz der Validierung.sav", 
                      to.data.frame=TRUE, use.value.labels = FALSE)


# Deskriptive Ergebnisse zur Stichprobe via SPSS
################################################################################
# siehe "0_Daten/Syntax Datenaufbereitung.sps"




# Reliabilität (Tabelle 2)
################################################################################

##### Reliabilität
# Skala NFC-K_klassisch 
nfck_k1 <- data[, c("nfc_k_k1", "nfc_k_k2_r", "nfc_k_k3_r", "nfc_k_k4", "nfc_k_k5")]

# Skala NFC-K_2: 423
nfck_k2 <- data[, c("nfc_k423_1", "nfc_k423_2", "nfc_k423_3_r", "nfc_k423_4_r", "nfc_k423_5_r")]

# Skala NFC-K_3
nfck_k3 <- data[, c("nfc_k5226_1", "nfc_k5226_2_r", "nfc_k5226_3_r", "nfc_k5226_4_r", "nfc_k5226_5")]

## Vergleich der Reliabilitäten
cocron(list(nfck_k1, nfck_k2, nfck_k3))
# Kurzskalen unterscheiden sich nicht signifikant in ihrem Cronbachs alpha



# Modellfit (Tabelle 2 und Anhang 4 in ESM 1)
################################################################################
write_data <- data
write_data[is.na(write_data)] <- -77
write.table(nfck_k1[write_data$Variante_NFC_K == 1, ], file="1_Analysen/nfc_k_1.txt",
            col.names = FALSE, row.names = FALSE)
write.table(nfck_k2[write_data$Variante_NFC_K == 2, ], file="1_Analysen/nfc_k_2.txt",
            col.names = FALSE, row.names = FALSE)
write.table(nfck_k3[write_data$Variante_NFC_K == 3, ], file="1_Analysen/nfc_k_3.txt",
            col.names = FALSE, row.names = FALSE)

compute_fit <-  function(file_in, 
                         extract_output = TRUE,
                         out_name = "",
                         mplus_path = NULL){
    
    # Stößt die MPlus-Berechnung basierend auf dem Input-File an
    # gibt Statistiken aus in R
    # Parameter: 
    # - file_in: Namen der Input-Syntax aus MPlus (muss im gleichen Ordner liegen) 
    # - stats_out: Namen des aggregierten Statistik-Outputs
    
    statistics		<- matrix(NA, 1, 8)
    colnames(statistics) <- c("chi2", "df", "p", "CFI", "TLI", "RMSEA", "AIC", "BIC")
     
    if(.Platform$OS.type == "windows"){
      # Windows
      if(is.null(mplus_path)){
        mplus_path <- "mplus.exe"
      }
      system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, out_name, ".out", sep=""), 
             wait = TRUE, ignore.stdout = TRUE)
      
    } else {
      if(tolower(Sys.info()["sysname"]) == "linux"){
        # Linux
        if(is.null(mplus_path)){
          mplus_path <- "/opt/mplusdemo/mpdemo"
        }
        system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, out_name, ".out", sep=""), 
               wait = TRUE, ignore.stdout = TRUE)
        
        
      } else {
        # Mac
        if(is.null(mplus_path)){
          mplus_path <- "/Applications/Mplus/mplus"
        }
        system(paste(paste0(mplus_path, " "), file_in, ".inp ", file_in, out_name, ".out", sep=""), 
               wait = TRUE, ignore.stdout = TRUE) 
      }
    }
    
    if(extract_output){
      # III) Ergebnisse des MPlus-Outputs einsammeln 
      output 			<- scan(file = paste(file_in, ".out", sep=""),
                        what = "character", sep ="\n", quote = NULL, quiet = TRUE)
      
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
      
      
    }
    return(statistics)
}


compute_fit("1_Analysen/input_nfc_k_1")
# MPlus Output gespeichert in: '1_Analysen/input_nfc_k_1.out'

compute_fit("1_Analysen/input_nfc_k_2")
# MPlus Output gespeichert in: '1_Analysen/input_nfc_k_2.out'

compute_fit("1_Analysen/input_nfc_k_3")
# MPlus Output gespeichert in: '1_Analysen/input_nfc_k_3.out'




# Korrelationen aller NFC-Skalen mit verwandten Konstrukten (Tabelle 2)
################################################################################
# siehe auch SPSS-Syntax: '0_Daten/Syntax Datenaufbereitung.sps'

vars <- c("NFC", "Offenheit", "Gewissenhaftigkeit", "Deliberation", "TIE", "Lernziel_Gesamt", "Vermeid_gesamt", "SozErwuenscht")
scales <- c("NFC_K_1", "NFC_K_2", "NFC_K_3", "NFC")


corrs <- matrix(NA, length(vars), 4)
rownames(corrs) <- vars
colnames(corrs) <- scales
for(i in 1:nrow(corrs)){
  corrs[i, 1] <- cor(data[,c(rownames(corrs)[i], colnames(corrs)[1])], use="pairwise.complete.obs")[2,1]
  corrs[i, 2] <- cor(data[,c(rownames(corrs)[i], colnames(corrs)[2])], use="pairwise.complete.obs")[2,1]
  corrs[i, 3] <- cor(data[,c(rownames(corrs)[i], colnames(corrs)[3])], use="pairwise.complete.obs")[2,1]
  corrs[i, 4] <- cor(data[,c(rownames(corrs)[i], colnames(corrs)[4])], use="pairwise.complete.obs")[2,1]
}
round(corrs, 2)



###### Korrelation mit der Langskala
round(corrs["NFC", c("NFC_K_1", "NFC_K_2", "NFC_K_3")], 2)

##### Mittlere Korrelation mit verwandten Konstrukten
(mean_cors <- round(apply(corrs[c("Offenheit", "Deliberation", "TIE", "Lernziel_Gesamt"), 
                  c("NFC_K_1", "NFC_K_2", "NFC_K_3")], 2, mean), 2))
(n_nfc_k <- table(data$Variante_NFC_K))


# Unterschiede in den Korrelationen
print(r.test(n_nfc_k[1], mean_cors[1], mean_cors[2]), digits=3)
print(r.test(n_nfc_k[2], mean_cors[2], mean_cors[3]), digits=3)
print(r.test(n_nfc_k[1], mean_cors[1], mean_cors[3]), digits=3)


##### Korrelation mit sozialer Erwünschtheit
round(corrs["SozErwuenscht", c("NFC_K_1", "NFC_K_2", "NFC_K_3")], 2)















