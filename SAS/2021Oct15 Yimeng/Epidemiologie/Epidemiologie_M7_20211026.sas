/************************************************************************************
Programme name: 	Epidemiologie_M7_D01_0_0.sas
Programme language: SAS 9.4
Initial date: 		26/OCT/2021
Author(s): 		ZBA
*************************************************************************************
Short description: 	Modul 7: Fortgeschrittene epidemiologische und statistische Methoden
Requirements: 		---
*************************************************************************************
Input: 			
Output: 			Table in RTF format
Required programs:  
*************************************************************************************
Document history :
Version 	     Date 			Author 		Purpose
D01_0_0		26/OCT/2021		ZBA			First initiation
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

*** Change the output destination to SAS;
ods exclude all;

*** Clear old titles and footnotes;
title;
footnote;

*** Get the actual date in form of a global macro variable;
%global today;
%let today=%sysfunc(today(),date9.); /* DDMMMYYYY */
%put &today.;

%let Input_folder     = C:\Users\zbai\Documents\GitHub\R-Projects\SAS;
%let Output_folder    = C:\Users\zbai\Documents\GitHub\R-Projects\SAS;
%let This_prog        = Epidemiologie_M7_D01_0_0; 


/************************************************************************************/ 
/************************          Start programm            ************************/ 
/************************************************************************************/ 


*** Load the data;
data geburt1_analysis_D01;set "&Input_folder.\geburt1.sas7bdat";run;

*** Check the variable names and description;
proc contents data = geburt1_analysis_D01 out=geburt1_contents; run;
/*******************************************
Mbmi Mutter: Body-Mass-Index (Selbstangabe) (kg/qm)
e019 Subjektiver Gesundheitszustand (EFB)
*******************************************/

*** Sort the data by mother's BMI;
proc sort data=geburt1_analysis_D01 out = geburt1_analysis_D02; by Mbmi; run;

*** Count for empirical distribution and remove the missing values;
data geburt1_analysis_D02;
	set geburt1_analysis_D02 nobs=totalobs;
	ecdf_pcs = _n_ / totalobs;
	label ecdf_pcs = "Probability";
	if Mbmi ne .;
run;

 

*** Empirical distribution plot for maternal BMI of the mother;
ods tagsets.rtf file="&Output_folder.\&This_prog._empirical distribution.rtf";

ods proclabel="Figure 1.1: Empirical distribution plot";
ods escapechar = "^";
options nobyline;

ods exclude none;
ods html close; 

title1 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "Figure 1.1: Empirical distribution plot for maternal BMI of the mother";
footnote1 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program &this_prog.";

 
proc gplot data=geburt1_analysis_D02;
   plot ecdf_pcs * Mbmi;
run;
quit;
 
footnote; 
title;

ods graphics off;
ods exclude all;

ods tagsets.rtf close;
ods _all_ close;








*** Calculate the deciles and create macro variables for categories;
proc univariate data=geburt1_analysis_D02 noprint;
	var Mbmi;
	output out=geburt1_analysis_D03 pctlpre=P_
	pctlpts=10 to 100 by 10;
run;
proc transpose data=geburt1_analysis_D03 out=geburt1_analysis_D03_t; run;

data _NULL_;
	set geburt1_analysis_D03_t;
	call symput('Cat'||LEFT(_N_), COL1);
run;
%put &Cat1. &Cat2. &Cat3. &Cat4.;

*** Categorize;
data geburt1_analysis_D04;
	set geburt1_analysis_D02;
	if 		Mbmi le &Cat1.                     then  Mbmi_CAT = 1;
	else if   Mbmi gt &Cat1. and Mbmi le &Cat2.  then  Mbmi_CAT = 2;
	else if   Mbmi gt &Cat2. and Mbmi le &Cat3.  then  Mbmi_CAT = 3;
	else if   Mbmi gt &Cat3. and Mbmi le &Cat4.  then  Mbmi_CAT = 4;
	else if   Mbmi gt &Cat4. and Mbmi le &Cat5.  then  Mbmi_CAT = 5;
	else if   Mbmi gt &Cat5. and Mbmi le &Cat6.  then  Mbmi_CAT = 6;
	else if   Mbmi gt &Cat6. and Mbmi le &Cat7.  then  Mbmi_CAT = 7;
	else if   Mbmi gt &Cat7. and Mbmi le &Cat8.  then  Mbmi_CAT = 8;
	else if   Mbmi gt &Cat8. and Mbmi le &Cat9.  then  Mbmi_CAT = 9;
	else if   Mbmi gt &Cat9. and Mbmi le &Cat10. then  Mbmi_CAT = 10;
run;
*** Check the categories;
proc freq data = geburt1_analysis_D04 noprint; tables Mbmi_CAT/out = geburt1_analysis_D04_check; run;

*** cumulative logit model;
*** Confidence Intervals Wald/Profile likelihood CI;
ods select none;
ods output CLparmWald = geburt1_analysis_D05_WaldCI
		 OddsRatios = geburt1_analysis_D05_OddsRatios
		 ParameterEstimates = geburt1_analysis_D05_ParEst;
proc logistic data = geburt1_analysis_D04  plots(MAXPOINTS=NONE)=ODDSRATIO  ;
	class Mbmi_CAT (REF='1') / PARAM=REF;;
	model e019 (EVENT="1") = Mbmi_CAT/ CLPARM=BOTH;
run;



ods tagsets.rtf file="&Output_folder.\&This_prog._LG Results.rtf";

ods proclabel="Table 1.1: Results from cumulative logit model";
ods escapechar = "^";
options nobyline;

ods exclude none;
ods html close;
 
title1 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "Table 1.1:^R/RTF'\tab' Results from cumulative logit model";
title2 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "^R/RTF'\tab\tab' Odds Ratios and Wald CI"; 

footnote1 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program &this_prog.";

proc print data = geburt1_analysis_D05_OddsRatios; run;

footnote; 
title;



title1 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "Table 1.2:^R/RTF'\tab' Empirischen Verteilung in 10 gleichbesetzte Kategorien (Dezile)";
title2 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "^R/RTF'\tab\tab' Odds Ratios and Wald CI"; 

footnote1 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program &this_prog.";

proc print data = Geburt1_analysis_d03_t; run;

footnote; 
title;



title1 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "Table 1.3:^R/RTF'\tab' Results from cumulative logit model";
title2 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "^R/RTF'\tab\tab' Parameter estimates"; 

footnote1 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program &this_prog.";

proc print data = geburt1_analysis_D05_ParEst; run;

footnote; 
title;


title1 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "Table 1.4:^R/RTF'\tab' Results from cumulative logit model";
title2 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "^R/RTF'\tab\tab' Wald CI for parameter estimates"; 

footnote1 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program &this_prog.";

proc print data = geburt1_analysis_D05_WaldCI; run;

footnote; 
title;

ods exclude all;

ods tagsets.rtf close;
ods _all_ close;





/***********************************************************************
*** Not recommand to use this unordered multinomial model;
proc logistic data = geburt1_analysis_D04;
	class Mbmi_CAT ;
	model e019 (EVENT="1") = Mbmi_CAT/ CLPARM=BOTH  LINK=GLOGIT;
run;
************************************************************************/



ods tagsets.rtf file="&Output_folder.\&This_prog._odds ratio plot.rtf";

ods proclabel="Figure 1.2: Odds ratio plot";
ods escapechar = "^";
options nobyline;

ods exclude none;
ods html close;
ods graphics on;
ods graphics / height=14cm width=26cm noborder;
ods select ORPlot;

title1 j=l  height=10pt font=courier bold bcolor="#e6f3ff" "Figure 1.2:^R/RTF'\tab' Odds ratio plot";

footnote1 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program &this_prog.";

proc logistic data = geburt1_analysis_D04  plots(MAXPOINTS=NONE)=ODDSRATIO;
	class Mbmi_CAT (REF='10') / PARAM=REF;;
	model e019 (EVENT="1") = Mbmi_CAT/ CLPARM=BOTH;
run;

footnote; 
title;

ods graphics off;
ods exclude all;

ods tagsets.rtf close;
ods _all_ close;

/************************************************************************************/ 
/************************          End programm            ************************/ 
/************************************************************************************/ 



/**********************************
Tidy up
**********************************/
*** Macro for deleting all macro variables;
%macro deleteALL;
   	options nonotes;
  	%local vars;
  	proc sql noprint;
      	     select name into: vars separated by ' '
         	  from dictionary.macros
            	      where scope='GLOBAL' 
			   and not name contains 'SYS_SQL_IP_' and not name contains ('STUDY') 
			   and not name contains ('REPORT') and not name contains ('SPONSOR')
			   and not name contains ('SYSRANDOM')
			   ;
   	quit;
 	%put &vars.;
   	%symdel &vars;
 
   	options notes;
   	%put NOTE: Macro variables deleted.;
 
%mend deleteALL;
%deleteALL

proc datasets nolist;
	delete Geburt1_:;
run;
