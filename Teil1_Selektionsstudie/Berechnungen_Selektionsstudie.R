################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# Teil 1: Selektionsstudie
################################################################################


# Einstellungen und Pakete
################################################################################
#!# Anpassen des aktuellen Arbeitsverzeichnisses
path <- "Teil1_Selektionsstudie/"
setwd(path)


#!# Anpassen des Pfades zum MPLus-Programm, z.B.
# Windows: "mplus.exe"
# Linux: "/opt/mplusdemo/mpdemo"
# Mac: "/Applications/Mplus/mplus"
mplus_path <<- "/opt/mplusdemo/mpdemo"



# Deskriptive Ergebnisse zur Stichprobe via SPSS
################################################################################
# siehe "0_Daten/Syntax Datenaufbereitung.sps"



# Klassische Itemstatistiken via SPSS (Anhang 1 in ESM 1)
################################################################################
# siehe "1_Klassische_Itemstatistiken/Berechnung_Itemsstatistiken.sps"



# Berechnung des Full Information Approachs
################################################################################
# Aufbereitung der Daten und Berechnung aller Subskalen
# Skript kann automatisch laufen via "source" oder händisch durchgegangen werden
source("2_Full_Information_Approach/Berechnung_full_information.R")



# Selektion der besten Skalen im Full Information Approach
################################################################################
# Selektion der besten 21 Skalen nach definierten Kriterien

source("2_Full_Information_Approach/Selektion_full_information.R")
##### Zunächst Selektion anhand harter Kriterien:
# - 4 oder 5 Items
# - 75% negativ formulierter items
# - alpha und GLB > 0.7
# - CFI > 0.95
# - Ausschluss von Skalen mit Item v_96
##### Ergebnis: 104 beste Skalen --> "/2_Full_Information_Approach/Zwischenergebnisse/Full_Information_Skalenauswahl.RData"


#### Weitere Selektion relativ über sequentiell beste 2/3
# - RMSEA minimal
# - mittlere Korrelation zu verwandten Konstrukten maximal
# - Korrelation mit sozialer Erwünschtheit minimal


##### Finale Selektion durch interaktive Analyse in Excel basierend auf Full_Information_beste_Skalen.csv
# All selektierten Skalen hochgradig ähnlich
# Kriterien:
# - RMSEA_korr minimal
# - Korrelation mit sozialer Erwünschtheit minimal
# - mittlere Korrelation zu verwandten Konstrukten maximal
# - zusätzlich: 
#   - zufriedenstellender Reliabilität (Cronbachs alpha, GLB)
#   - ausgewogene Korrelation zu verwandten Konstrukten, siehe Differenz der Korrelationen (nicht nur mittlere Korrelation)

final_scales <<- c(423, 5226)



##### Ablegen der finalen Skalen (Kennwerte für Manuskript)
best_scales <- read.csv("2_Full_Information_Approach/Full_Information_beste_Skalen.csv")
write.csv(best_scales[best_scales$id %in% final_scales, ], file="Full_Information_selektierte_Skalen.csv", 
          row.names = FALSE)



# Sensitivitätsanalyse: Messinvarianz im Full Information Approach
################################################################################
# Basierend auf den 104 besten Skalen

source("3_Sensitivitätsanalyse/Berechnung_Messinvarianz.R")
# Sensitivitäts-Ergebnisse siehe 'Auswertung der Ergebnisse' am Ende des Skripts
# Invarianzkennwerte der selektierten Skalen (Anhang 5 in ESM 1) in: 'Measurement_Invariance_selektierte_Skalen.csv 

source("3_Sensitivitätsanalyse/Berechnung_MLR_Schaetzer.R")
# Sensitivitäts-Ergebnisse siehe 'Vergleich zwischen ML und MLR' am Ende des Skripts


# Mplus-Berechnungen der selektierten Skalen zur Inspektion der Mplus Outfiles (Anhang 3 in ESM 1)
################################################################################
population.matrix		<- read.table("2_Full_Information_Approach/Zwischenergebnisse/population.matrix.txt")
variable.names	<- c("v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r","v10r","v11r","v12r","v13","v14","v15r","v16r")
population.matrix <- population.matrix[rownames(population.matrix) %in% final_scales, ]


##### CFA mit und ohne korrelierte Fehlerterme
# Ergebnisse liegen in "2_Full_Information_Approach/Zwischenergebnisse/...

compute_fi(population.matrix[1, ], 
           file_in = "2_Full_Information_Approach/Zwischenergebnisse/input_uncorr", 
           correlated_errors = FALSE,
           extract_output = FALSE,
           out_name = "_NFCK2")
compute_fi(population.matrix[2, ], 
           file_in = "2_Full_Information_Approach/Zwischenergebnisse/input_uncorr", 
           correlated_errors = FALSE,
           extract_output = FALSE,
           out_name = "_NFCK3")

compute_fi(population.matrix[1, ], 
           file_in = "2_Full_Information_Approach/Zwischenergebnisse/input_corr", 
           correlated_errors = TRUE,
           extract_output = FALSE,
           out_name = "_NFCK2")
compute_fi(population.matrix[2, ], 
           file_in = "2_Full_Information_Approach/Zwischenergebnisse/input_corr", 
           correlated_errors = TRUE,
           extract_output = FALSE,
           out_name = "_NFCK3")

###### Messinvarianzmodelle
# Ergebnisse liegen in "3_Sensitivitätsanalyse/Zwischenergebnisse/...

### bezüglich Geschlecht
compute_mi(population.matrix[1, ], 
           file_in = "3_Sensitivitätsanalyse/Zwischenergebnisse/input_mi_gender", 
           correlated_errors = TRUE, 
           extract_output = FALSE,
           out_name = "_NFCK2")
compute_mi(population.matrix[2, ], 
           file_in = "3_Sensitivitätsanalyse/Zwischenergebnisse/input_mi_gender", 
           correlated_errors = TRUE, 
           extract_output = FALSE,
           out_name = "_NFCK3")

### bezüglich Bildung
compute_mi(population.matrix[1, ], 
           file_in = "3_Sensitivitätsanalyse/Zwischenergebnisse/input_mi_edu", 
           correlated_errors = TRUE, 
           extract_output = FALSE,
           out_name = "_NFCK2")
compute_mi(population.matrix[2, ], 
           file_in = "3_Sensitivitätsanalyse/Zwischenergebnisse/input_mi_edu", 
           correlated_errors = TRUE, 
           extract_output = FALSE,
           out_name = "_NFCK3")



