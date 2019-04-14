* Encoding: UTF-8.

******************************************************************************************************.
*********************** Rekodierungen, Skalenbildung und Reliabilität ********************************.
******************************************************************************************************.


*** Bildung ***.

* Rekodierung des Bildungsabschlusses in niedrigere, mitllere und hoehere Bildung.
RECODE Abschluss (1=1) (2=1) (3=1) (4=1) (5=2) (6=2) (7=3) (8=3) (9=3) (10=3) INTO Bildung.
VARIABLE LABELS Bildung 'Bildung'. 
VALUE LABELS Bildung 
1 'niedrige Bildung' 2 'mittlere Bildung' 3 'höhere Bildung'.
EXECUTE.



*** Offenheit fuer neue Erfahrungen (aus dem NEO-FFI) ***. 

*Rekodierung der invertierten Items.
RECODE neo_o1 neo_o2 neo_o4 neo_o5 neo_o7 neo_o8 neo_o10 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO neo_o1_r neo_o2_r neo_o4_r neo_o5_r neo_o7_r neo_o8_r neo_o10_r.
EXECUTE.

*Reliabilitaet Offenheit.
RELIABILITY
  /VARIABLES=neo_o1_r neo_o2_r neo_o3 neo_o4_r neo_o5_r neo_o6 neo_o7_r neo_o8_r neo_o9 neo_o10_r neo_o11 neo_o12
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala Offenheit.
Compute Offenheit = MEAN
(neo_o1_r, neo_o2_r, neo_o3, neo_o4_r, neo_o5_r, neo_o6, neo_o7_r, neo_o8_r, neo_o9, neo_o10_r, neo_o11, neo_o12).
des Offenheit.



*** Gewissenhaftigkeit (aus dem NEO-FFI)  ***. 

*Rekodierung der invertierten Items.
RECODE neo_g3 neo_g6 neo_g9 neo_g11   (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO neo_g3_r neo_g6_r neo_g9_r neo_g11_r.
EXECUTE.

*Reliabilitaet Gewissenhaftigkeit.
RELIABILITY
  /VARIABLES=neo_g1 neo_g2 neo_g3_r neo_g4 neo_g5 neo_g6_r neo_g7 neo_g8 neo_g9_r neo_g10 neo_g11_r neo_g12
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala Gewissenhaftigkeit..
Compute Gewissenhaftigkeit = MEAN
(neo_g1, neo_g2, neo_g3_r, neo_g4, neo_g5, neo_g6_r, neo_g7, neo_g8, neo_g9_r, neo_g10, neo_g11_r, neo_g12).
des Gewissenhaftigkeit.


*** NFC-K-1 ***. 

*Rekodierung der invertierten Items.
RECODE nfc_k_k2 nfc_k_k3   (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO nfc_k_k2_r nfc_k_k3_r .
EXECUTE.


*Reliabilitaet NFC-K-1.
RELIABILITY
  /VARIABLES=nfc_k_k1, nfc_k_k2_r, nfc_k_k3_r, nfc_k_k4, nfc_k_k5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala NFC-K-1.
Compute NFC_K_1 = MEAN (nfc_k_k1, nfc_k_k2_r, nfc_k_k3_r, nfc_k_k4, nfc_k_k5).
des NFC_K_1.


*** NFC-K-2 ***. 

*Rekodierung der invertierten Items.
RECODE nfc_k423_3 nfc_k423_4 nfc_k423_5   (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO nfc_k423_3_r nfc_k423_4_r nfc_k423_5_r.
EXECUTE. 

*Reliabilitaet NFC-K-2.
RELIABILITY
  /VARIABLES=nfc_k423_1, nfc_k423_2, nfc_k423_3_r, nfc_k423_4_r, nfc_k423_5_r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala NFC-K-2.
Compute NFC_K_2 = MEAN (nfc_k423_1, nfc_k423_2, nfc_k423_3_r, nfc_k423_4_r, nfc_k423_5_r).
des NFC_K_2.


*** NFC-K-3 ***. 

*Rekodierung der invertierten Items.
RECODE nfc_k5226_2 nfc_k5226_3 nfc_k5226_4  (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO nfc_k5226_2_r nfc_k5226_3_r nfc_k5226_4_r.
EXECUTE. 

*Reliabilitaet NFC-K-3.
RELIABILITY
  /VARIABLES=nfc_k5226_1, nfc_k5226_2_r, nfc_k5226_3_r, nfc_k5226_4_r, nfc_k5226_5
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala NFC-K-3.
Compute NFC_K_3 = MEAN (nfc_k5226_1, nfc_k5226_2_r, nfc_k5226_3_r, nfc_k5226_4_r, nfc_k5226_5).
des NFC_K_3.



*** Typisches intellektuelles Interesse (TIE) ***.

*Rekodierung der invertierten Items.
RECODE TIE_6 TIE_8 TIE_9 TIE_10 TIE_11 TIE_12  (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO TIE_6_r TIE_8_r TIE_9_r TIE_10_r TIE_11_r TIE_12_r.
EXECUTE. 


*Reliabilitaet TIE.
RELIABILITY
  /VARIABLES=TIE_1, TIE_2, TIE_3, TIE_4, TIE_5, TIE_6_r, TIE_7, TIE_8_r, TIE_9_r, TIE_10_r, 
TIE_11_r, TIE_12_r, TIE_13, TIE_14, TIE_15, TIE_16, TIE_17, TIE_18 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala TIE.
Compute TIE = MEAN (TIE_1, TIE_2, TIE_3, TIE_4, TIE_5, TIE_6_r, TIE_7, TIE_8_r, TIE_9_r, TIE_10_r, TIE_11_r, TIE_12_r, TIE_13, TIE_14, TIE_15, TIE_16, TIE_17, TIE_18).



*** NFC 16-Item-Langform ***.

*Rekodierung der invertierten Items.
RECODE NFC_4 NFC_6 NFC_7 NFC_8 NFC_9 NFC_10 NFC_11 NFC_12 NFC_15 NFC_16  (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO NFC_4_r  NFC_6_r  NFC_7_r  NFC_8_r  NFC_9_r  NFC_10_r  NFC_11_r  NFC_12_r  NFC_15_r  NFC_16_r.
EXECUTE. 

*Reliabilitaet und Itemstatistiken NFC 16-Item-Langform*.
RELIABILITY
  /VARIABLES=NFC_1, NFC_2, NFC_3,NFC_4_r, NFC_5, NFC_6_r, NFC_7_r, 
NFC_8_r, NFC_9_r, NFC_10_r,  NFC_11_r, NFC_12_r, NFC_13, NFC_14, NFC_15_r, NFC_16_r 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


*Bidlung der Skala NFC 16-Item-Langform.
Compute NFC = MEAN (NFC_1, NFC_2, NFC_3,NFC_4_r, NFC_5, NFC_6_r, NFC_7_r, 
NFC_8_r, NFC_9_r, NFC_10_r,  NFC_11_r, NFC_12_r, NFC_13, NFC_14, NFC_15_r, NFC_16_r).
des NFC.

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

*** Soziale Erwuenschtheit ***. 

** Selbsttaeuschung **.
*Rekodierung der invertierten Items.
RECODE SE_selbst2 SE_selbst4 SE_selbst5 SE_selbst7 SE_selbst9 SE_selbst10  (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO SE_selbst2_r SE_selbst4_r SE_selbst5_r SE_selbst7_r SE_selbst9_r SE_selbst10_r .
EXECUTE. 

*Reliabilitaet Selbsttaeuschung.
RELIABILITY
  /VARIABLES=SE_selbst1, SE_selbst2_r, SE_selbst3, SE_selbst4_r, SE_selbst5_r, SE_selbst6, 
SE_selbst7_r, SE_selbst8, SE_selbst9_r, SE_selbst10_r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala Selbsttaeuschung.
Compute Selbsttaeuschung = MEAN (SE_selbst1, SE_selbst2_r, SE_selbst3, SE_selbst4_r ,SE_selbst5_r, SE_selbst6, SE_selbst7_r, SE_selbst8, SE_selbst9_r, SE_selbst10_r).
des Selbsttaeuschung.


** Fremdtaeuschung **.
*Rekodierung der invertierten Items.
RECODE SE_fremd1 SE_fremd2 SE_fremd4 SE_fremd5 SE_fremd7 SE_fremd8 SE_fremd10 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
INTO SE_fremd1_r SE_fremd2_r SE_fremd4_r SE_fremd5_r SE_fremd7_r SE_fremd8_r SE_fremd10_r.
EXECUTE. 

*Reliabilitaet Fremdtaeuschung.
RELIABILITY
  /VARIABLES=SE_fremd1_r, SE_fremd2_r, SE_fremd3, SE_fremd4_r, SE_fremd5_r, SE_fremd6, SE_fremd7_r, SE_fremd8_r, SE_fremd9, SE_fremd10_r 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala Fremdtaeuschung.
Compute Fremdtaeuschung = MEAN (SE_fremd1_r, SE_fremd2_r, SE_fremd3, SE_fremd4_r, SE_fremd5_r, SE_fremd6, SE_fremd7_r, SE_fremd8_r, SE_fremd9, SE_fremd10_r).
des Fremdtaeuschung.


* Bildung einer Gesamtskala Soziale Erwuenschtheit.

Compute SozErwuenscht = MEAN (SE_selbst1, SE_selbst2_r, SE_selbst3, SE_selbst4_r ,SE_selbst5_r, SE_selbst6, SE_selbst7_r, SE_selbst8, SE_selbst9_r, SE_selbst10_r, 
SE_fremd1_r, SE_fremd2_r, SE_fremd3, SE_fremd4_r, SE_fremd5_r, SE_fremd6, SE_fremd7_r, SE_fremd8_r, SE_fremd9, SE_fremd10_r). 
des SozErwuenscht.

*Reliabilitaet der Gesamtskala Soziale Erwuenschtheit.
RELIABILITY
  /VARIABLES=SE_selbst1, SE_selbst2_r, SE_selbst3, SE_selbst4_r ,SE_selbst5_r, SE_selbst6, SE_selbst7_r, SE_selbst8, SE_selbst9_r, SE_selbst10_r, 
SE_fremd1_r, SE_fremd2_r, SE_fremd3, SE_fremd4_r, SE_fremd5_r, SE_fremd6, SE_fremd7_r, SE_fremd8_r, SE_fremd9, SE_fremd10_r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*** Praeferenz fuer Deliberation ***. 

*keine Rekodieren noetig, nur positive Items.

*Reliabilitaet Praeferenz fuer Deliberation .
RELIABILITY
  /VARIABLES=PI_D_D1, PI_D_D2, PI_D_D3, PI_D_D4, PI_D_D5, PI_D_D6, PI_D_D7, PI_D_D8, PI_D_D9
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.


*Bidlung der Skala PrÃ¤ferenz fuer Deliberation .
Compute  Deliberation= MEAN (PI_D_D1, PI_D_D2, PI_D_D3, PI_D_D4, PI_D_D5, PI_D_D6, PI_D_D7, PI_D_D8, PI_D_D9).
des Deliberation.


*** Lernzielorientierung ***. 

*keine Rekodieren noetig, nur positive Items.

* Reliabilitaet Lernzielorientierung Version Schueler/innen.
RELIABILITY
  /VARIABLES=v_90, v_92, v_94, v_100, v_102, v_104, v_106, v_108 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

* Bildung der Skala Lernzielorientierung bei Schueler/innnen.
Compute Lernziel_Schule = MEAN (v_90, v_92, v_94, v_100, v_102, v_104, v_106, v_108).
des Lernziel_Schule.

*  Reliabilitaet Lernzielorientierung Version Studierende.
RELIABILITY
  /VARIABLES=v_72, v_74, v_76, v_78, v_80, v_82, v_84, v_86 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala Lernzielorientierung Studierenden.
Compute Lernziel_Studium= MEAN (v_72, v_74, v_76, v_78, v_80, v_82, v_84, v_86).
des Lernziel_Studium.

* Reliabilitaet Lernzielorientierung Version Berufstaetige.
RELIABILITY
  /VARIABLES=v_56, v_58, v_60, v_62, v_64, v_66, v_68, v_70 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala Lernzielorientierung bei Berufstaetigen.
Compute Lernziel_Beruf = MEAN (v_56, v_58, v_60, v_62, v_64, v_66, v_68, v_70).
des Lernziel_Beruf.

*Bildung einer Gesamt-Skala Lernzielorientierung Ã¼ber alle Subgruppen hinweg.
COMPUTE Lernziel_Gesamt= sum.1 (Lernziel_Schule, Lernziel_Studium, Lernziel_Beruf).


*** Vermeidungs-Leistungszielorientierung ***. 

*keine Rekodieren noetig, nur positive Items.

* Reliabilitaet Vermeidungs-Leistungszielorientierung Version Schueler/innen.
RELIABILITY
  /VARIABLES=v_91, v_93, v_99, v_101, v_103, v_105, v_107, v_109
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala Vermeidungs-Leistungszielorientierung SchÃ¼ler/innen.
Compute Vermeid_Schule = MEAN (v_91, v_93, v_99, v_101, v_103, v_105, v_107, v_109).
des Vermeid_Schule.

* Reliabilitaet Vermeidungs-Leistungszielorientierung Version Studierende.
RELIABILITY
  /VARIABLES=v_73, v_75, v_77, v_79, v_81, v_83, v_85, v_87 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala Vermeidungs-Leistungszielorientierung bei Studierenden.
Compute Vermeid_Studium = MEAN (v_73, v_75, v_77, v_79, v_81, v_83, v_85, v_87).
des Vermeid_Studium.


* Reliabilitaet Vermeidungs-Leistungszielorientierung Version Berufstaetige.
RELIABILITY
  /VARIABLES=v_57, v_59, v_61, v_63, v_65, v_67, v_69, v_71 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala Vermeidungs-Leistungszielorientierung bei Berufstaetigen.
Compute Vermeid_Beruf = MEAN (v_57, v_59, v_61, v_63, v_65, v_67, v_69, v_71).
des Vermeid_Beruf.

*Bildung einer Gesamt-Skala Vermeidungs-Leistungszielorientierung ueber alle Subgruppen hinweg.
COMPUTE Vermeid_gesamt= sum.1 (Vermeid_Schule, Vermeid_Studium, Vermeid_Beruf).





*** Erstellen einer Variable, die anzeigt, welche der drei Kurzskale die Person bearbeitet hat*.
Compute Variante_NFC_K = 0.
if (nfc_k_k1 > 0) Variante_NFC_K = 1.
if (nfc_k423_1 > 0) Variante_NFC_K = 2.
if (nfc_k5226_1 > 0) Variante_NFC_K = 3.
EXECUTE.



******************************************************************************************************.
************************************** Demographie ***************************************************.
******************************************************************************************************.

*Deskriptive Statistiken für Geschlecht, Alter und höchster Bildungsabschluss. 

FREQUENCIES VARIABLES=Geschlecht Alter Abschluss
  /STATISTICS=STDDEV MEAN
  /ORDER=ANALYSIS.



******************************************************************************************************.
*********************************** Analysen zur Validierung *****************************************.
******************************************************************************************************.


*** Korrelations-Analyse aller Skalen ***.
CORRELATIONS
  /VARIABLES=NFC NFC_K_1 NFC_K_2 NFC_K_3 Offenheit Gewissenhaftigkeit Deliberation TIE 
    Lernziel_Gesamt Vermeid_gesamt SozErwuenscht
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.
