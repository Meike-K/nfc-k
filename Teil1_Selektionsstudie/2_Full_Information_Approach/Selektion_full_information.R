################################################################################
# Code Anhang für das Manuskript:
# Ein Vergleich traditioneller und computergestützter Methoden zur Erstellung 
# einer deutschsprachigen Need for Cognition Kurzskala
################################################################################
# I. Selektionsstudie
################################################################################
# I.3 Seĺektion von Skalen basierend auf dem Full Information Approach
################################################################################

setwd("2_Full_Information_Approach/")
load("Zwischenergebnisse/Full_Information_Statistiken.RData")


###### Check ob zufriedenstellende Reliabilität bei wenigen Items
plot(results$no.items, results$alpha,
     xlab = "Anzahl Items der Skala", ylab = "Cronbachs alpha")
# Ergebnis: keine zufriedenstellende Reliabilität bei < 4 Items


###### Selektionsschritte aus Paper
# Skalen mit 4 oder 5 Items
fi_results <- results[(results$no.items > 3)&(results$no.items < 6),]


# Nur Skalen mit mindestens 25% und maximal 75% verneinter Items
fi_results$ratio <- fi_results$negativ.worded/fi_results$no.items
fi_results <- fi_results[(fi_results$ratio >= (1/4))&(fi_results$ratio <= (3/4)),]


# Skalen mit Cronbachs alpha und GLB >= 0.7
fi_results <- subset(fi_results, alpha >= 0.70)
fi_results <- subset(fi_results, glb >= 0.70)


# Skalen mit Mindestfit CFI >= 0.95
fi_results <- subset(fi_results, CFI_korr >= 0.95)
fi_results <- subset(fi_results, CFI_unkorr >= 0.95)


# Skalen ohne Item v_96 (Item 2), da zu komplex formuliert
fi_results <- fi_results[fi_results$v_96==0,]


##### Ergebnis: 111 Subskalen
nrow(fi_results)
save(fi_results, file = "Zwischenergebnisse/Full_Information_Skalenauswahl.RData")


##### Weitere Reduktion zur einfacheren interaktiven Analyse: Unteres/Oberes Drittel der Optimierungskriterien
rmsea_korr_cutoff <- quantile(fi_results$RMSEA_korr, probs = 2/3)
fi_results <- subset(fi_results, RMSEA_korr <= rmsea_korr_cutoff)

rmsea_unkorr_cutoff <- quantile(fi_results$RMSEA_unkorr, probs = 2/3)
fi_results <- subset(fi_results, RMSEA_unkorr <= rmsea_unkorr_cutoff)

mean_cor_cutoff <- quantile(fi_results$mean_cor, probs = 1/3)
fi_results <- subset(fi_results, mean_cor >= mean_cor_cutoff)

socdes_cor_cutoff <- quantile(fi_results$corSOCDES, probs = 2/3)
fi_results <- subset(fi_results, corSOCDES <= socdes_cor_cutoff)


##### Umstellen der Spaltenreihenfolge für schnellere interaktive Analyse
fi_results <- fi_results[, c("id", "glb", "alpha",  "mean_cor", "corSOCDES",
                             "RMSEA_korr", "RMSEA_unkorr",
                             "omega", "CFI_korr", "TLI_korr", "AIC_korr", "BIC_korr", 
                             "CFI_unkorr", "TLI_unkorr", "AIC_unkorr", "BIC_unkorr",
                             "diff_tie", "diff_delib", "diff_open", "diff_learn",
                             "no.items", "negativ.worded", "v_95",  "v_96", "v_97",
                             "v_98r", "v_25", "v_26r", "v_27r", "v_28r", "v_253r",
                             "v_254r", "v_255r", "v_256r", "v_257", "v_258", "v_259r",            
                             "v_260r")]

write.csv(round(fi_results, 2), file="Full_Information_beste_Skalen.csv", row.names = FALSE)
nrow(fi_results)

##### Überlappung von Items
overlapping <- round(apply(fi_results[, c("v_95",  "v_96", "v_97", "v_98r", "v_25", "v_26r", "v_27r", 
                                          "v_28r", "v_253r", "v_254r", "v_255r", "v_256r", "v_257", 
                                          "v_258", "v_259r", "v_260r")], 2, sum)/nrow(fi_results), 2)
sort(overlapping, decreasing = TRUE)
# Die 3 am häufigsten vorkommenden Items v_97, v_27r, v_28r sind auch Überschneidung der beiden selektierten Subskalen

setwd("..")
