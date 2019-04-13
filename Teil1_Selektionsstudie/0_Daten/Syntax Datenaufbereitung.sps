* Encoding: UTF-8.

******************************************************************************************************.
****************** Rekodierungen, Skalenbildung und Reliabilität *************************************.
******************************************************************************************************.

*** Bildung ***.

* Rekodierung des Bildungsabschlusses in niedrigere, mitllere und höhere Bildung.
RECODE Abschluss (1=1) (2=1) (3=1) (4=1) (5=2) (6=2) (7=3) (8=3) (9=3) (10=3) INTO Bildung.
VARIABLE LABELS Bildung 'Bildung'. 
VALUE LABELS Bildung
1 'niedrige Bildung' 2 'mittlere Bildung' 3 'höhere Bildung'.
EXECUTE.



*** Offenheit für neue Erfahrungen (NEO-FFI) ***. 

*Rekodierung der invertierten Items.
Compute v_8r = 8 - v_8. 
Compute v_9r = 8 - v_9. 
Compute v_11r = 8 - v_11. 
Compute v_12r = 8 - v_12. 
Compute v_13r = 8 - v_13. 
Compute v_15r = 8 - v_15. 
Compute v_17r = 8 - v_17.

*Reliabilität Offenheit.
RELIABILITY
  /VARIABLES=v_8r v_9r v_10 v_11r v_12r v_13r v_14 v_15r v_16 v_17r v_18 v_19
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala Offenheit.
Compute Offenheit= MEAN(v_8r, v_9r, v_10, v_11r, v_12r, v_13r, v_14, v_15r, v_16, v_17r, v_18, v_19).
des Offenheit.


*** Präferenz für Deliberation ***. 

*keine Rekodieren nötig, nur positive Items.

*Reliabilität Präferenz für Deliberation .
RELIABILITY
  /VARIABLES=v_236 v_237 v_238 v_239 v_240 v_241 v_242 v_243 v_244
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bidlung der Skala Präferenz für Deliberation .
Compute Deliberation= MEAN(v_236, v_237, v_238, v_239, v_240, v_241, v_242, v_243, v_244).
des Deliberation.


*** Typisches intellektuelles Interesse (TIE) ***.

*Rekodierung der invertierten Items.
Compute v_168r = 8 - v_168. 
Compute v_170r = 8 - v_170. 
Compute v_171r = 8 - v_171. 
Compute v_172r = 8 - v_172. 
Compute v_173r = 8 - v_173. 
Compute v_174r = 8 - v_174. 

*Reliabilität TIE.
RELIABILITY
  /VARIABLES=v_163 v_164 v_165 v_166 v_167 v_168 v_169 v_170 v_171 v_172 v_173 v_174 v_175 v_176 v_177 v_178 v_179 v_180
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.



Compute TIE= MEAN(v_163, v_164, v_165, v_166, v_167, v_168r, v_169, v_170r, v_171r, v_172r, v_173r, v_174r, v_175, v_176, v_177, v_178, v_179, v_180).
des TIE.


*** Lernzielorientierung ***.

*keine Rekodieren nötig, nur positive Items.

* Reliabilität Lernzielorientierung Version Schüler/innen.
RELIABILITY
  /VARIABLES=v_90 v_92 v_94 v_100 v_102 v_104 v_106 v_108
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL MEANS VARIANCE CORR.

* Bildeung der Skala Lernzielorientierung bei Schüler/innnen.
Compute Lernziel_Schule= MEAN(v_90, v_92, v_94, v_100, v_102, v_104, v_106, v_108). 
des Lernziel_Schule.

*  Reliabilität Lernzielorientierung Version Studierende.
RELIABILITY
  /VARIABLES=v_72 v_74 v_76 v_78 v_80 v_82 v_84 v_86
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL MEANS VARIANCE CORR.

*Bildung der Skala Lernzielorientierung Studierenden.
Compute Lernziel_Studium= MEAN(v_72, v_74, v_76, v_78, v_80, v_82, v_84, v_86). 
des Lernziel_Studium.


* Reliabilität Lernzielorientierung Version Berufstätige.
RELIABILITY
  /VARIABLES=v_56 v_58 v_60 v_62 v_64 v_66 v_68 v_70
  /SCALE('ALL VARIABLES') ALL 
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR
  /SUMMARY=TOTAL MEANS VARIANCE CORR.

*Bildung der Skala Lernzielorientierung bei Berufstätigen.
Compute Lernziel_Beruf= MEAN(v_56, v_58,  v_60, v_62, v_64, v_66, v_68, v_70). 
des Lernziel_Beruf.


*Bildung einer Gesamt-Skala Lernzielorientierung über alle Subgruppen hinweg.
COMPUTE Lernziel_Gesamt= sum.1 (Lernziel_Schule, Lernziel_Studium, Lernziel_Beruf).




*** Soziale Erwünschtheit ***. 

** Selbsttäuschung **.

*Rekodierung der invertierten Items. 
Compute v_35r = 8 - v_35.
Compute v_37r = 8 - v_37.
Compute v_38r = 8 - v_38.
Compute v_40r = 8 - v_40.
Compute v_42r = 8 - v_42.
Compute v_43r = 8 - v_43.

* Reliabilität Selbststäuschung. 
RELIABILITY
  /VARIABLES=v_34 v_35r v_36 v_37r v_38r v_39 v_40r v_41 v_42r v_43r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

* Bildung der Skala Selbsttäuschung.
Compute Selbsttaeuschung= MEAN(v_34, v_35r, v_36, v_37r, v_38r, v_39, v_40r, v_41, v_42r, v_43r). 
des Selbsttaeuschung.


** Fremdtäuschung **.

*Rekodierung der invertierten Items.  
Compute v_110r = 8 - v_110.
Compute v_111r = 8 - v_111.
Compute v_113r = 8 - v_113.
Compute v_114r = 8 - v_114.
Compute v_116r = 8 - v_116.
Compute v_117r = 8 - v_117.
Compute v_119r = 8 - v_119.

*Reliabilität Fremdtäuschung.
RELIABILITY
  /VARIABLES=v_110r v_111r v_112 v_113r v_114r v_115 v_116r v_117r v_118 v_119r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

*Bildung der Skala Fremdtäuschung.
Compute Fremdtaeuschung= MEAN(v_110r, v_111r, v_112, v_113r, v_114r, v_115, v_116r, v_117r, v_118, v_119r). 
des Fremdtaeuschung.


*Bildung einer Gesamtskala Soziale Erwünschtheit.
Compute SozErwuenscht = MEAN(v_34, v_35r, v_36, v_37r, v_38r, v_39, v_40r, v_41, v_42r, v_43r, v_110r, v_111r, v_112, v_113r, v_114r, v_115, v_116r, v_117r, v_118, v_119r). 
des SozErwuenscht.

*Reliabilität der Gesamtskala Soziale Erwünschtheit..
RELIABILITY
  /VARIABLES=v_34 v_35r v_36 v_37r v_38r v_39 v_40r v_41 v_42r v_43r v_110r v_111r v_112 v_113r v_114r v_115 v_116r v_117r v_118 v_119r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.


*** NFC 16-Item-Langform ***.

*Rekodierung der invertierten Items.  
Compute v_98r = 8 - v_98.
Compute v_26r = 8 - v_26.
Compute v_27r = 8 - v_27.
Compute v_28r = 8 - v_28.
Compute v_253r = 8 - v_253.
Compute v_254r = 8 - v_254.
Compute v_255r = 8 - v_255.
Compute v_256r = 8 - v_256.
Compute v_259r = 8 - v_259.
Compute v_260r = 8 - v_260.

*Reliabilität und Itemstatistiken NFC 16-Item-Langform*.
RELIABILITY
  /VARIABLES= v_95 v_96 v_97 v_98r v_25 v_26r v_27r v_28r v_253r v_254r v_255r v_256r v_257 v_258 v_259r v_260r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

* Bildung der Skala NFC 16-Item-Langform.
Compute NFC= MEAN(v_95, v_96, v_97, v_98r, v_25, v_26r, v_27r, v_28r, v_253r, v_254r, v_255r, v_256r, v_257, v_258, v_259r, v_260r). 
des NFC.

* Bildung einer NFC Skala ohne die Items, die in der NFC_K_1 vorkommen*.
Compute NFC_ohne= MEAN(v_96,  v_98r, v_25, v_26r, v_253r, v_254r, v_255r, v_256r, v_258, v_259r, v_260r).
des NFC_ohne.


* Voranalyse: T-Test zwischen weiblichen und männlichen Teilnehmenden in Bezug auf Geschlecht.
T-TEST GROUPS=Geschlecht(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=NFC
  /CRITERIA=CI(.95).



*** Kurzskala NFC_K_1 (aus klassischer Itemanalyse) ***.

*Reliabilität NFC_K_1.
RELIABILITY
  /VARIABLES= v_95 v_97 v_27r v_28r v_257
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala NFC_K_1.
Compute NFC_K_1 = MEAN(v_95, v_97, v_27r, v_28r, v_257). 
des NFC_K_1. 


*** Kurzskala NFC_K_2 (aus full information approach) ***.

*Reliabilität NFC_K_2.
RELIABILITY
  /VARIABLES=  v_97 v_25 v_27r v_28r v_253r
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala NFC_K_2.
Compute NFC_K_2 = MEAN(v_97, v_25, v_27r, v_28r, v_253r). 
des NFC_K_2. 


*** Kurzskala NFC_K_3 (aus full information approach) ***.

*Reliabilität NFC_K_3.
RELIABILITY
  /VARIABLES=  v_97 v_27r v_28r v_255r v_257
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE CORR.

*Bildung der Skala NFC_K_3.
Compute NFC_K_3 = MEAN(v_97, v_27r, v_28r, v_255r, v_257). 
des NFC_K_3. 




******************************************************************************************************.
**************************************** Demographie *************************************************.
******************************************************************************************************.
