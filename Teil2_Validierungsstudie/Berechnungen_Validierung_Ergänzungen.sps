* Encoding: UTF-8.

******************************************************************************************************.
************************************ Reliabilitäten **************************************************.
******************************************************************************************************.

*** Offenheit für neue Erfahrungen (aus dem NEO-FFI) ***. 

*Reliabilität Offenheit.
RELIABILITY
  /VARIABLES=neo_o1_r neo_o2_r neo_o3 neo_o4_r neo_o5_r neo_o6 neo_o7_r neo_o8_r neo_o9 neo_o10_r neo_o11 neo_o12
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


*** Gewissenhaftigkeit (aus dem NEO-FFI)  ***. 

*Reliabilität Gewissenhaftigkeit.
RELIABILITY
  /VARIABLES=neo_g1 neo_g2 neo_g3_r neo_g4 neo_g5 neo_g6_r neo_g7 neo_g8 neo_g9_r neo_g10 neo_g11_r neo_g12
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


*** NFC-K-1 ***. 

*Reliabilität NFC-K-1.
RELIABILITY
  /VARIABLES=nfc_k_k1, nfc_k_k2_r, nfc_k_k3_r, nfc_k_k4, nfc_k_k5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



*** NFC-K-2 ***. 

*Reliabilität NFC-K-2.
RELIABILITY
  /VARIABLES=nfc_k423_1, nfc_k423_2, nfc_k423_3_r, nfc_k423_4_r, nfc_k423_5_r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



*** NFC-K-3 ***. 

*Reliabilität NFC-K-3.
RELIABILITY
  /VARIABLES=nfc_k5226_1, nfc_k5226_2_r, nfc_k5226_3_r, nfc_k5226_4_r, nfc_k5226_5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



*** Typisches intellektuelles Interesse (TIE) ***.

*Reliabilität TIE.
RELIABILITY
  /VARIABLES=TIE_1, TIE_2, TIE_3, TIE_4, TIE_5, TIE_6_r, TIE_7, TIE_8_r, TIE_9_r, TIE_10_r, 
TIE_11_r, TIE_12_r, TIE_13, TIE_14, TIE_15, TIE_16, TIE_17, TIE_18 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



*** NFC 16-Item-Langform ***.

*Reliabilität und Itemstatistiken NFC 16-Item-Langform*.
RELIABILITY
  /VARIABLES=NFC_1, NFC_2, NFC_3,NFC_4_r, NFC_5, NFC_6_r, NFC_7_r, 
NFC_8_r, NFC_9_r, NFC_10_r,  NFC_11_r, NFC_12_r, NFC_13, NFC_14, NFC_15_r, NFC_16_r 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.




*Exploratorische Faktorenanalyse.
FACTOR
/VARIABLESNFC_1, NFC_2, NFC_3,NFC_4_r, NFC_5, NFC_6_r, NFC_7_r, 
NFC_8_r, NFC_9_r, NFC_10_r,  NFC_11_r, NFC_12_r, NFC_13, NFC_14, NFC_15_r, NFC_16_r 
/MISSING LISTWISE
/ANALYSIS NFC_1, NFC_2, NFC_3,NFC_4_r, NFC_5, NFC_6_r, NFC_7_r, 
NFC_8_r, NFC_9_r, NFC_10_r,  NFC_11_r, NFC_12_r, NFC_13, NFC_14, NFC_15_r, NFC_16_r 
/CRITERIA MINEIGEN(1) ITERATE(25)
/EXTRACTION PC
/CRITERIA ITERATE(25)
/ROTATION VARIMAX
/SAVE REG(ALL)
/METHOD=CORRELATION.



*** Soziale Erwünschtheit ***. 

*Reliabilität Selbsttäuschung.
RELIABILITY
  /VARIABLES=SE_selbst1, SE_selbst2_r, SE_selbst3, SE_selbst4_r, SE_selbst5_r, SE_selbst6, 
SE_selbst7_r, SE_selbst8, SE_selbst9_r, SE_selbst10_r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



** Fremdtäuschung **.

*Reliabilität Fremdtäuschung.
RELIABILITY
  /VARIABLES=SE_fremd1_r, SE_fremd2_r, SE_fremd3, SE_fremd4_r, SE_fremd5_r, SE_fremd6, SE_fremd7_r, SE_fremd8_r, SE_fremd9, SE_fremd10_r 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



*** Präferenz für Deliberation ***. 

*Reliabilität Präferenz für Deliberation .
RELIABILITY
  /VARIABLES=PI_D_D1, PI_D_D2, PI_D_D3, PI_D_D4, PI_D_D5, PI_D_D6, PI_D_D7, PI_D_D8, PI_D_D9
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.




*** Lernzielorientierung ***. 

* Reliabilität Lernzielorientierung Version Schüler/innen.
RELIABILITY
  /VARIABLES=v_90, v_92, v_94, v_100, v_102, v_104, v_106, v_108 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


*  Reliabilität Lernzielorientierung Version Studierende.
RELIABILITY
  /VARIABLES=v_72, v_74, v_76, v_78, v_80, v_82, v_84, v_86 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


* Reliabilität Lernzielorientierung Version Berufstätige.
RELIABILITY
  /VARIABLES=v_56, v_58, v_60, v_62, v_64, v_66, v_68, v_70 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



*** Vermeidungs-Leistungszielorientierung ***. 

* Reliabilität Vermeidungs-Leistungszielorientierung Version Schüler/innen.
RELIABILITY
  /VARIABLES=v_91, v_93, v_99, v_101, v_103, v_105, v_107, v_109
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



* Reliabilität Vermeidungs-Leistungszielorientierung Version Studierende.
RELIABILITY
  /VARIABLES=v_73, v_75, v_77, v_79, v_81, v_83, v_85, v_87 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


* Reliabilität Vermeidungs-Leistungszielorientierung Version Berufstätige.
RELIABILITY
  /VARIABLES=v_57, v_59, v_61, v_63, v_65, v_67, v_69, v_71 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.




******************************************************************************************************.
*********************************** Analysen zur Validierung *****************************************.
******************************************************************************************************.


*** Korrelations-Analyse aller Skalen ***.
CORRELATIONS
  /VARIABLES=NFC NFC_K_1 NFC_K_2 NFC_K_3 Offenheit Gewissenhaftigkeit Deliberation TIE 
    Lernziel_Gesamt Vermeid_gesamt SozErwuenscht
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.


*** T-Test







******************************************************************************************************.
***************************** Sensitivitätsanalysen (nicht im Manuskript) ****************************.
******************************************************************************************************.
*** Gewichtungen ***. 

* Gewicht Bildung.
*Quelle: Statistisches Bundesamt, Verteilung der Bildung in Deutschland 2017*.
*(Verteilung in Deutschland: 57,1% niedrig, 31,9% mittel, hoch 17,7% (angepasst an unsere Bildungskategorien, deswegen >100%), in unserer Stichprobe: 15,7% niedrig, 38,2% mittel, 45,2% hoch)*.

IF Bildungkategorie=1 gewichtung=3.64 .
IF Bildungkategorie=2 gewichtung=0.84 .
IF Bildungkategorie=3 gewichtung=0.39.
EXECUTE .

WEIGHT BY gewichtung.


*Gewicht Geschlecht.
*Quelle: Statistisches Bundesamt, Verteilung der Bildung in Deutschland 2018*.
*(Verteilung in Deutschland: 49,4% männlich, 50,6% weiblich, in unserer Stichprobe: 41,9% weiblich, 58,1% männlich )*.

IF Geschlecht=1 gewichtung=1.21 .
IF Geschlecht=2 gewichtung=0.85 .
EXECUTE .

WEIGHT BY gewichtung.

