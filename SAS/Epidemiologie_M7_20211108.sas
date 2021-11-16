/************************************************************************************
Programme name: 	Epidemiologie_M7_D02_0_0.sas
Programme language: SAS 9.4
Initial date: 		08/NOV/2021
Author(s): 		Yimeng Zheng
*************************************************************************************
Short description: 	Modul 7: Fortgeschrittene epidemiologische und statistische Methoden
Requirements: 		---
*************************************************************************************/




/************************************************************************************/ 
/************************        General settings	           ************************/ 
/************************************************************************************/ 

*** delete all datasets in the work library;
proc datasets library=work kill nolist; run; quit;

*** Change the output destination to SAS and empty the log as well as the output window;
ods listing;

*** Clear the log and the output window;
dm log 'clear';
dm output 'clear';

*** Clear old titles and footnotes;
title;
footnote;


/************************************************************************************/ 
/************************          Start programm            ************************/ 
/************************************************************************************/ 

*** Load the data;
data geburt1_analysis_D01;set "C:\Users\zbai\Documents\GitHub\R-Projects\SAS\geburt1.sas7bdat";run;

/************************************************************************************
Fragestellung: Beeinflusst die Gewichtszunahme in der Schwangerschaft (kg) Probleme nach der Geburt?
E014z:        	Gewichtszunahme in Schwangerschaft (kg)
e018:    		Probleme nach Geburt

e018m1:		Nach Geburt: Atmungsschwierigkeiten, Anpassungsstörungen
e018m2:		Nach Geburt: Infektion
e018m3:		Nach Geburt: Gelbsucht
e018m4:		Nach Geburt: Untergewicht, Frühgeburt
e018m5:		Nach Geburt: Sonstige
e018m6:		Nach Geburt: Kinderklinik
e018m6z:		Nach Geburt: Nächte in Kinderklinik

Covariate adjustment:

sex:			Geschlecht
e089m:		Mutter: Schulabschluss
e012mz:		Mutter: Alter (Jahre)
e093:		Monatl. Haushaltsnettoeinkommen
Mbmi:          Mutter: Body-Mass-Index (kg/qm)
OW:			Ost/West geografisch
************************************************************************************/ 

*** Choose the relevants variables and format;
proc format;
	value  sex_FMT 	1 = "Männlich"
					2 = "Weiblich";
	value  e018_FMT 	1 = "Ja"
					2 = "Nein";
	value  e089m_FMT    1 = "Haupt-/Volksschulabschluss"
					2 = "Realschulabschluss (Mittl. Reife)"
					3 = "Abschluss Polytechn. Oberschule"
					4 = "Fachhochschulreife"
					5 = "Abitur (Gymnasium bzw. EOS)"
					6 = "Anderer Schulabschluss"
					7 = "Ohne Schulabschluss"
					8 = "Noch keinen Schulabschluss";
	value e093_FMT		1 = "<500 €"
					2 = "500 - < 750 €"
					3 = "750 - < 1.000 €"
					4 = "1.000 - < 1.250 €"
					5 = "1.250 - < 1.500 €"
					6 = "1.500 - < 1.750 €"
					7 = "1.750 - < 2.000 €"
					8 = "2.000 - < 2.250 €"
					9 = "2.250 - < 2.500 €"
					10 = "2.500 - < 3.000 €"
					11 = "3.000 - < 4.000 €"
					12 = "4.000 - < 5.000 €"
					13 = ">= 5.000 €";
	value OW_FMT        1  = "Ost"
					2  = "West";
run;

data geburt1_analysis_D02;
	set geburt1_analysis_D01;
	keep E014z e018 e018m: sex e089m e012mz e093 OW Mbmi;
	format sex sex_FMT. e018 e018_FMT. e089m e089m_FMT. e093 e093_FMT. OW OW_FMT.;
run;
 


/************************************************************************************
1. Descriptive statistics of variables
    	* for continious and categorical variable by Probleme nach Geburt
	* for Geburt illness
2. Logistic regression (Full model)
3. Logistic regression (selected model)
************************************************************************************/


/*** Descriptive statistics of variables ***/
proc sort data = geburt1_analysis_D02; by e018; run;
proc means data=geburt1_analysis_D02 n nmiss min max mean median std q1 q3 maxdec=3 completetypes missing order=data; 
	var E014z e012mz Mbmi;	
	class e018;
	ods output summary = geburt1_analysis_D03_conti;
run; 
proc print data = geburt1_analysis_D03_conti; run;

proc freq data = geburt1_analysis_D02 ; 
	tables (sex e089m e093 OW)*e018/Missing nopercent nocum; 
run;

*** Summary;
proc freq data = geburt1_analysis_D02 noprint; 
	tables Sex*e018/Missing nopercent nocum out = Geburt1_analysis_d03_cate1; 
run;
proc freq data = geburt1_analysis_D02 noprint; 
	tables e089m*e018/Missing nopercent nocum out = Geburt1_analysis_d03_cate2; 
run;
proc freq data = geburt1_analysis_D02 noprint; 
	tables e093*e018/Missing nopercent nocum out = Geburt1_analysis_d03_cate3; 
run;
proc freq data = geburt1_analysis_D02 noprint; 
	tables OW*e018/Missing nopercent nocum out = Geburt1_analysis_d03_cate4; 
run;
data Geburt1_analysis_d03_cate;
	length Parameter Variable $50.;
	set Geburt1_analysis_d03_cate1 (in=a )
	    Geburt1_analysis_d03_cate2 (in=b )
	    Geburt1_analysis_d03_cate3 (in=c )
	    Geburt1_analysis_d03_cate4 (in=d );
	if a then Parameter = "Geschlecht";
	if b then Parameter = "Mutter: Schulabschluss";
	if c then Parameter = "Haushaltsnettoeinkommen";
	if d then Parameter = "Ost/West geografisch";

	if Sex ne . then Variable = vvalue(Sex);
	if e089m ne . then Variable = vvalue(e089m);
	if e093 ne . then Variable = vvalue(e093);
	if OW ne . then Variable = vvalue(OW);

	if Variable = "" then Variable = "Missing";

	drop Sex e089m e093 OW;
run;
proc print data = Geburt1_analysis_d03_cate; run;


/*** Calculate for Illness after Geburt ***/
data geburt1_analysis_D04;
	set geburt1_analysis_D02;
	if e018 = 1;
run;
proc freq data=geburt1_analysis_D04; 
	tables e018m:/missing;	
run; 

 proc freq data=geburt1_analysis_D04; 
	tables e018/missing;	
run; 







/*** Full Model ***/
proc logistic data = geburt1_analysis_D02  plots(MAXPOINTS=NONE)=ODDSRATIO  ;
	class sex e089m e093 OW/ PARAM=REF;;
	model e018 (EVENT="Nein") = E014z sex e089m e012mz Mbmi e093 OW;
run;


/*** New Model ***/
ods output OddsRatios = geburt1_analysis_D03_OddsRatios
		 ParameterEstimates = geburt1_analysis_D03_ParEst;
proc logistic data = geburt1_analysis_D02  plots(MAXPOINTS=NONE)=ODDSRATIO  ;
	class sex(REF='Weiblich') e089m(REF='Noch keinen Schulabschluss') e093(REF='<500 €')/ PARAM=REF;;
	model e018 (EVENT="Nein") = sex e089m  e093 Mbmi;
run;









 
