* Encoding: UTF-8.

******************************************************************************************************.
************ Analysen für die Itemselektion nach der klassischen Testtheorie ****************.
******************************************************************************************************.

*** Itemschwierigkeit, Varianz der Items und Trennschärfe ***.

RELIABILITY
  /VARIABLES=v_96 v_97 v_98r v_25 v_26r v_27r v_28r v_253r v_254r v_255r v_256r v_257 v_258 v_259r v_260r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.


***Explorative Faktorenanalyse ***.
FACTOR
/VARIABLES v_96 v_97 v_98r v_25 v_26r v_27r v_28r v_253r v_254r v_255r v_256r v_257 v_258 v_259r v_260r
/MISSING LISTWISE
/ANALYSIS v_96 v_97 v_98r v_25 v_26r v_27r v_28r v_253r v_254r v_255r v_256r v_257 v_258 v_259r v_260r
/CRITERIA MINEIGEN(1) ITERATE(25)
/EXTRACTION PC
/CRITERIA ITERATE(25)
/ROTATION VARIMAX
/SAVE REG(ALL)
/METHOD=CORRELATION.

*** Korrelations-Analyse aller Skalen ***.
CORRELATIONS
  /VARIABLES=NFC_K NFC NFC_ohne Offenheit Deliberation TIE Lernziel_Schule 
Lernziel_Studium Lernziel_Beruf Selbsttaeuschung Fremdtaeuschung SozErwuenscht 
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.




