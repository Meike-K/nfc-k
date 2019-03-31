################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# I. Selektionsstudie
################################################################################



#!# Set your current working directory
path <- "Teil1_Selektionsstudie/"
setwd(path)

#!# Set Path of the MPlus executable, e.g.
# Windows: "mplus.exe"
# Linux: "/opt/mplusdemo/mpdemo"
# Mac: "/Applications/Mplus/mplus"
mplus_path <<- "/opt/mplusdemo/mpdemo"



# Klassische Itemstatistiken via SPSS
################################################################################
# siehe "1_Klassische_Itemstatistiken/Verechnung Itemsstatistiken.sps"



# Berechnung des Full Information Approachs
################################################################################
# Aufbereitung der Daten und Berechnung aller Subskalen
source("2_Full_Information_Approach/Berechnung_full_information.R")


s# Selektion der besten Skalen im Full Information Approach
################################################################################
# Selektion der besten x Skalen nach definierten Kriterien
source("2_Full_Information_Approach/Selektion_full_information.R")
##### Zunächst Selektion anhand harter Kriterien:
# - 4 oder 5 Items
# - 75% negatively worded items
# - alpha und GLB > 0.7
# - CFI > 0.95
# - Ausschluss von Skalen mit Item v_96
##### Ergebnis: 104 beste Skalen --> "/2_Full_Information_Approach/Zwischenergebnisse/Full_Information_Skalenauswahl.RData"


#### Weitere Selektion relativ über sequentiell beste 2/3
# - RMSEA minimal
# - mittlere Korrelation zu verwandten Konstrukten maximal
# - Korrelation mit sozialer Erwünschtheit minimal


##### Finale Selektion durch interaktive Analyse in Excel siehe selection_fi_results.xlsx
# All selektierten Skalen hochgradig ähnlich
# Kriterien:
# - RMSEA_korr minimal
# - Korrelation mit sozialer Erwünschtheit minimal
# - mittlere Korrelation zu verwandten Konstrukten maximal
# - zusätzlich: 
#   - zufriedenstellender Reliabilität (Cronbachs alpha, GLB)
#   - ausgewogene Korrelation zu verwandten Konstrukten, siehe Differenz der Korrelationen (nicht nur mittlere Korrelation)
final_scales <<- c(423, 5226)


# Sensitivitätsanalyse: Messinvarianz im Full Information Approach
################################################################################
source("3_Sensitivitätsanalyse/Berechnung_Messinvarianz.R")

source("3_Sensitivitätsanalyse/Berechnung_MLR_Schaetzer.R")


# Berechnungen an selektierten Skalen zur Inspektion der Mplus Outfiles
################################################################################
population.matrix		<- read.table("2_Full_Information_Approach/Zwischenergebnisse/population.matrix.txt")
variable.names	<- c("v1","v2","v3","v4r","v5","v6r","v7r","v8r","v9r","v10r","v11r","v12r","v13","v14","v15r","v16r")
population.matrix <- population.matrix[rownames(population.matrix) %in% final_scales, ]


##### Modell ohne korrelierte Fehlerterme
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

###### Messinvarianz bezüglich Geschlecht
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

###### Messinvarianz bezüglich Bildung
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



