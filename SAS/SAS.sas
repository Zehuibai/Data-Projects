
/************************************************************************************
Programme name: 	InboTNA_T14_3_6_FASAQ_D01_0_0.sas
Programme language: SAS 9.4
Initial date: 		04/OCT/2021
Sponsor | study: 	Prof. Dr. Martina Kerscher, Universität Hamburg
Author(s): 		ZBA
*************************************************************************************
Short description: 	Table 14.3.6:	Secondary Endpoints,
                                  	Patient satisfaction by FASAQ,
 							Response at patient satisfaction by FASAQ at days 30 60 90 120 and 150,
Requirements: 		---
Risk assessment: 	Moderate
*************************************************************************************
Input: 			
Output: 			Table in RTF format
Required programs:  Current version InBoTNA_MasterProg_TFLs (InBoTNA_MasterProg_TFLs_D01_0_0.sas)
*************************************************************************************
Document history :
Version 	     Date 			Author 		Purpose
D01_0_0		04/OCT/2021		ZBA			First initiation
*************************************************************************************/ 

**********************************************************************************************************;
***                                        General settings;                                           ***;
**********************************************************************************************************;

*** delete all datasets in the work library;
	proc datasets library=work kill nolist; run; quit;

*** Change the output destination to SAS and empty the log as well as the output window;
	ods listing;

*** Clear the log and the output window;
	dm log 'clear';
	dm output 'clear';

*** Kind of analysis;
	%let data=01_Final; 

*** Declare the study path;
	%let study_folder=C:\Users\zbai\Desktop\SAS;		

*** Get the actual date in form of a global macro variable;
	%global today;
	%let today=%sysfunc(today(),date9.); /* DDMMMYYYY */

*** Define helpful macro variables;
	%let studyacronym=InBoNTA;
	%let sponsor=B. Braun Medical AG;
	%let report_version=draft_v01;
	%let report_author=Andreas Beust;
	%let studyname=InBoNTA;
	%let outputtype = Analysis; /* <- WARNING: WIP!*/
	%let version = 1.0 /* <- WARNING: WIP!*/;

*** Options;
	option nocenter orientation=landscape; 
	option linesize = 111 nodate nonumber;
	option pagesize = 42;

**********************************************************************************************************;
*** Overall includes; 
**********************************************************************************************************;
	
*** Load the data and the formats of the data; 
	libname ana_dat  "&study_folder.\SASFILES"; 
	options fmtsearch=(formats.m_&STUDYACRONYM._format_library);

*** Load macro library;
	%include "&study_folder.\SASMacro\Macro_IncludeProgrammes_V02.sas";
	%IncludeProgrammes(folderpath=&study_folder.\SASMacro\);

*** Load the formatted datasets (with event-specific prefix and without event-specific prefix;
	%include "&study_folder.\SASMacro\Macro_Import_V01.sas";
	%Import (folder_path=&study_folder.\SASFILES,
			 delete_all=1);





**********************************************************************************************************;
***                                        Data preparation;                                           ***;
**********************************************************************************************************;


/*********************************************
Define analysis dataset FAS and PPS
*********************************************/
data Inbonta_analysis_set;
  length SUBJECT_ID $5.;
  do Analysis_set = "FAS","PPS";
	do ID = 1 to 40;
	  SUBJECT_ID = "p"||put(ID,z2.);
       do visit = 2 to 6;
	    output;
	  end;
	end;
  end;
  drop ID;
run;
data Inbonta_analysis_set_FAS;set Inbonta_analysis_set;if Analysis_set = "FAS"; run;
data Inbonta_analysis_set_PPS;
  set Inbonta_analysis_set;
  if Analysis_set = "PPS";
  if SUBJECT_ID in ("P05","p09","p10","p17","p19","p39","p40") then delete;
run;
*** Sort the datasets;
proc sort data = Inbonta_analysis_set_FAS; by SUBJECT_ID VISIT; run;
proc sort data = Inbonta_analysis_set_PPS; by SUBJECT_ID VISIT; run;
 



/*********************************************
For SELS, the surface will be selected
*********************************************/
proc sort data = Inbonta_07_sels; by SUBJECT_ID VISIT; run;
data Inbonta_analysis_sels;
    merge Inbonta_07_sels (where=(SELS_SIDE="s")) Inbonta_analysis_set_FAS(in=a);
    by SUBJECT_ID VISIT;
    if a;
    SER  = input(SELS_SER, BEST12.);
    SESC = input(SELS_SESC,BEST12.);
    SEM  = input(SELS_SEM, BEST12.);
    SEW  = input(SELS_SEW, BEST12.);
    keep SUBJECT_ID VISIT SER SESC SEM SEW;
run;



/*********************************************
Roughness: Average Len R1 R2 R3 R4 R5 are used
*********************************************/
proc sort data = Inbonta_06_roughness; by SUBJECT_ID VISIT; run;
data Inbonta_analysis_roughness;
    merge Inbonta_06_roughness (where=(ROUGH_SIDE="s")) Inbonta_analysis_set_FAS(in=a);
    by SUBJECT_ID VISIT;
    if a;
    array Char   {6} $10.  AVG_LEN AVG_R1 AVG_R2 AVG_R3 AVG_R4 AVG_R5;
    array Num    {6}       LEN R1 R2 R3 R4 R5;
	do i=1 to 6;
	     Num{i}=input(Char{i},??comma32.);  
	end;
    keep SUBJECT_ID VISIT LEN R1 R2 R3 R4 R5;
run;



/*********************************************
For corneometry, the average of 3 measurements 
each on the left and right side will be used

The mean of the measurements of the left and 
right side will be calculated per subject. 
These values will be further classified according 
to the following categories:

•	Very dry (< 30)
•	Dry (30 - 45)
•	Sufficiently moist (> 45)
*********************************************/
proc format;
    value CORNEO_CAT
        4 = "Missing"
        1 = "Very dry"
        2 = "Dry"
	   3 = "Sufficiently moist";
run;
proc sort data = Inbonta_01_corneometry; by SUBJECT_ID VISIT CORNEO_SIDE; run;
data Inbonta_analysis_corneometry;
	set Inbonta_01_corneometry(rename=(visit=visit_c));
	by SUBJECT_ID visit_c CORNEO_SIDE;
	retain Sum_CORNEO;
	visit=input(visit_c,??comma32.);

	if first.CORNEO_SIDE then Sum_CORNEO = CORNEO_VAL;
	else Sum_CORNEO = Sum_CORNEO + CORNEO_VAL;
	if CORNEO_ASS_NO = "3" then CORNEO = Sum_CORNEO/3;

	if CORNEO_ASS_NO = "3";
      
	*** Categories;
	if CORNEO ne . then do;
	   if       CORNEO lt 30                  then CAT = 1;
	   else if  CORNEO ge 30 and CORNEO le 45 then CAT = 2;
	   else if  CORNEO gt 45                  then CAT = 3;
	end;
	if CORNEO eq . then CAT = 4;
	rename CORNEO_SIDE =SIDE;
	format CAT CORNEO_CAT.;
	keep SUBJECT_ID VISIT CORNEO_SIDE CORNEO visit CAT;
run;




/*********************************************
Cutometry: convert SIDE;
*********************************************/
proc sort data=Inbonta_02_cutometry; by SUBJECT_ID VISIT CUTO_RESULT; run;
proc transpose data=Inbonta_02_cutometry out=Inbonta_02_cutometry_temp1(rename=(_name_=var));
   by SUBJECT_ID VISIT CUTO_RESULT;
   var CUTO_LEFT CUTO_RIGHT;
run;
proc sort data=Inbonta_02_cutometry_temp1; by SUBJECT_ID VISIT var;run;
proc transpose data=Inbonta_02_cutometry_temp1 out=Inbonta_02_cutometry_temp2(drop=_name_);
   by SUBJECT_ID VISIT var;
   var col1;
   id CUTO_RESULT;
run;
*** For cutometry, the measurement of R0 to R9 by left and right side will be used;
data Inbonta_analysis_cutometry;
    length SIDE $7.;
    set Inbonta_02_cutometry_temp2;
    if var="CUTO_LEFT" then SIDE="left";
    if var="CUTO_RIGHT" then SIDE="right";
    keep SUBJECT_ID VISIT SIDE R:;
run;




/*********************************************
For PH value, the average of 3 measurements each 
on the left and right side will be used;
*********************************************/
proc sort data = Inbonta_03_ph; by SUBJECT_ID VISIT PH_SIDE; run;
data Inbonta_analysis_ph;
	set Inbonta_03_ph(rename=(visit=visit_c));
	by SUBJECT_ID visit_c PH_SIDE;
	retain Sum_PH;
	visit=input(visit_c,??comma32.);

	if first.PH_SIDE then Sum_PH = PH_VAL;
	else Sum_PH = Sum_PH + PH_VAL;
	if PH_ASS_NO = "3" then PH = Sum_PH/3;

	if PH_ASS_NO = "3";
	rename PH_SIDE =SIDE;
	keep SUBJECT_ID VISIT PH_SIDE PH;
run;



/*********************************************
Sebumetry: convert visit and delete missing ID

Sebumetry values will be classified according to 
the following values:

•	Dry, low oiliness (< 100) 
•	Normal (100 - 220)
•	Oily (> 220)
*********************************************/
proc format;
    value Sebumetry_CAT
        4 = "Missing"
        1 = "Dry, low oiliness"
        2 = "Normal"
	   3 = "Oily";
run;
proc sort data=Inbonta_04_sebumetry; by SUBJECT_ID; run;
proc transpose data=Inbonta_04_sebumetry out=Inbonta_analysis_sebumetry(rename=(_name_=var));
   by SUBJECT_ID;
   var V2_SEBU_VAL V3_SEBU_VAL V4_SEBU_VAL V5_SEBU_VAL V6_SEBU_VAL;
run;
data Inbonta_analysis_sebumetry;
    set Inbonta_analysis_sebumetry;
    length visit_c $3.;
    visit_c = substr(var, 2, 1);
    visit=input(visit_c,??comma32.);
    if SUBJECT_ID ne "entfällt";

    *** Categories;
	if COL1 ne . then do;
	   if       COL1 lt 100                 then CAT = 1;
	   else if  COL1 ge 100 and COL1 le 220 then CAT = 2;
	   else if  COL1 gt 220                 then CAT = 3;
	end;
	if COL1 eq . then CAT = 4;

    rename COL1 = SEBU;
    format CAT Sebumetry_CAT.;
    keep SUBJECT_ID VISIT COL1 CAT;
run;




/*********************************************
TEWL values will be classified according to the following categories:
•	Very healthy skin condition (0 - <10)
•	Healthy skin condition (10 - <15)
•	Normal skin condition (15 - <25)
•	Stressed skin (25 - <30)
•	Critical skin condition (= 30)
*********************************************/
proc format;
    value TEWL_CAT
        6 = "Missing"
        1 = "Very healthy"
        2 = "Healthy"
	   3 = "Normal"
	   4 = "Stressed"
	   5 = "Critical";
run;

proc sort data=Inbonta_05_tewl; by SUBJECT_ID VISIT TEWL_SIDE; run;
data Inbonta_analysis_tewl;
    set Inbonta_05_tewl(rename=(visit=visit_c));
    visit=input(visit_c,??comma32.);

    *** Categories;
	if TEWL_VAL ne . then do;
	   if       TEWL_VAL ge 0  and TEWL_VAL lt 10 then CAT = 1;
	   else if  TEWL_VAL ge 10 and TEWL_VAL lt 15 then CAT = 2;
	   else if  TEWL_VAL ge 15 and TEWL_VAL lt 25 then CAT = 3;
	   else if  TEWL_VAL ge 25 and TEWL_VAL lt 30 then CAT = 4;
	   else if  TEWL_VAL ge 30                    then CAT = 5;
	end;
	if TEWL_VAL eq . then CAT = 6;

    rename TEWL_SIDE=SIDE TEWL_VAL=TEWL;
    drop VARIABLE visit_c;
    format CAT TEWL_CAT.;
run;



/*********************************************
Skin Wrinkles (Cluster 1-4 of SASSQ)
*********************************************/
proc format;
    value Cluster_CAT
        5 = "Missing"
	   0 = "0: none"
        1 = "1: mild"
	   2 = "2: moderate"
	   3 = "3: severe"
        4 = "4: very severe";
run;
proc sort data=Inbonta_10_sassq_wrinkles nodupkey; by SASSQ_RATER SUBJECT_ID VISIT SASSQ_WRIN_CLUSTER; run;
data Inbonta_10_sassq_wrinkles_temp;
    set Inbonta_10_sassq_wrinkles;
    if SASSQ_WRIN_CLUSTER in ("1","2","3","4");
    format SASSQ_WRIN_VAL Cluster_CAT.;
run;
proc transpose data=Inbonta_10_sassq_wrinkles_temp out = Inbonta_analysis_wrinkles (drop=_NAME_) prefix=CLUSTER;
    by SASSQ_RATER SUBJECT_ID VISIT;
    var SASSQ_WRIN_VAL;
    ID SASSQ_WRIN_CLUSTER;
run;
data Inbonta_analysis_wrinkles;
    set Inbonta_analysis_wrinkles(rename=(visit=visit_c));
    visit=input(visit_c,??comma32.);
    rename CLUSTER1 = Forehead
    		 CLUSTER2 = Glabella 
	      CLUSTER3 = L_perior
           CLUSTER4 = R_perior;
    drop visit_c;
run; 



/*********************************************
GICS: categories
*********************************************/
proc format;
    value GICS_CAT
        8 = "Missing"
        1 = "+3   Very much improved"
	   2 = "+2   Much improved"
	   3 = "+1   Minimally improved"
        4 = " 0   No change" 
	   5 = "-1   Minimally worse"
	   6 = "-2   Much worse"
	   7 = "-3   Very much worse";
	value responder_CAT
	   2 = "Missing"
	   0 = "Non-Responder"
	   1 = "Responder";
run;
proc sort data=Inbonta_09_gics; by GICS_RATER SUBJECT_ID VISIT; run;
data Inbonta_analysis_gics;
    set Inbonta_09_gics(rename=(visit=visit_c));
    visit=input(visit_c,??comma32.);

         *** Format the catogories;
    if GICS_VAL ne "" then do;
	    if      GICS_VAL = "plus 3"  then GICS_CAT = 1;
	    else if GICS_VAL = "plus 2"  then GICS_CAT = 2;
	    else if GICS_VAL = "plus 1"  then GICS_CAT = 3;
	    else if GICS_VAL = "0"       then GICS_CAT = 4;
	    else if GICS_VAL = "minus 1" then GICS_CAT = 5;
	    else if GICS_VAL = "minus 2" then GICS_CAT = 6;
	    else if GICS_VAL = "minus 3" then GICS_CAT = 7;
    end;
    if GICS_VAL eq "" then GICS_CAT = 8;

    if GICS_CAT in (1,2) then  responder=1;
    else if GICS_CAT ne . then responder=0;
    else if GICS_CAT eq . then responder=2;

    format GICS_CAT GICS_CAT.   responder responder_CAT.;
    keep GICS_RATER SUBJECT_ID VISIT GICS_CAT responder;
run;



/*********************************************
FASAQ: FASAQ_Q8 as numeric
*********************************************/
proc format;
    value Agree_CAT
        999 = "Missing"
          0 = "  0%"
	    25 = " 25%"
	    50 = " 50%"
         75 = " 75%" 
	   100 = "100%";
run;
proc sort data=Inbonta_08_fasaq; by SUBJECT_ID VISIT; run;
data Inbonta_analysis_fasaq;
	set Inbonta_08_fasaq (rename=(visit=visit_c));
	if FASAQ_Q4 = 245 then FASAQ_Q4 = 25;
	if FASAQ_Q8 = "neutral" then Q_8 = 0;
	else Q_8 = input(FASAQ_Q8,??comma32.);

	visit=input(visit_c,??comma32.);

	array FASAQ   {7}       FASAQ_Q1-FASAQ_Q7;
	array Q       {7}       Q_1-Q_7;
	do i=1 to 7;
	     if FASAQ{i} eq . then Q{i}=999;	
          if FASAQ{i} ne . then Q{i}=FASAQ{i};		
	end;
	format Q_1-Q_7 Agree_CAT.;
	drop FASAQ_Q: i visit_c;
run;





**********************************************************************************************************;
***                              Data analysis marco for Conti/Cate variables                          ***;
**********************************************************************************************************;

/***************************************************************
%let data_in     = Inbonta_analysis_sels;
%let by_var      = SUBJECT_ID VISIT;
%let class_var   = VISIT;
%let Conti_var   = SER SESC SEM SEW;
%let Categ_var   =  ;
%let data_out    = Report_sels; 
 

%let data_in     = Inbonta_analysis_corneometry;
%let by_var      = SUBJECT_ID VISIT SIDE;
%let class_var   = VISIT SIDE;
%let Conti_var   = CORNEO;
%let Categ_var   = CAT;
%let data_out    = Report_Corneometry; 
 
%let data_in     = Inbonta_analysis_fasaq;
%let by_var      = SUBJECT_ID VISIT;
%let class_var   = VISIT;
%let Conti_var   = Q_8;
%let Categ_var   = Q_1 Q_2 Q_3 Q_4 Q_5 Q_6 Q_7;
%let data_out    = Report_fasaq; 
***************************************************************/

 
/*********** VAR: Calculating CFB and get the quantiative analysis prepare for report **************/
%macro VAR(data_in, by_var, class_var, Conti_var, Categ_var, data_out);
%MACRO HL;*** Enable editor code highlighting, delete when finalising; %MEND HL;

%put %sysfunc(countw(&Conti_var, %str( ), q));
%put %sysfunc(countw(&Categ_var, %str( ), q));

/*************************************************************/
/*** Start: Descriptove statistic for continious variables ***/
/*************************************************************/
%if %sysfunc(countw(&Conti_var, %str( ), q)) ne 0 %then %do;
	%do i=1 %to %sysfunc(countw(&Conti_var, %str( ), q));
		%let var=%scan(&Conti_var,&i);
		%put &var.;
               
		     *** Calculate the CFB;
			proc sort data=&data_in.;by &by_var.;run;
			data Temporary_&var._D01;																	
				set &data_in.;
				by &by_var.;	
				retain baseline ;
				if first.SUBJECT_ID then do;
				if Visit = 2 then baseline = &var.;
				end;
				if Visit > 2 and baseline ne .  then CFB = &var. - baseline;
			run;

			*** quantiative analysis ;
			ods select None;
			proc means data=Temporary_&var._D01 stackodsoutput 
                          n nmiss min max mean median std q1 q3 maxdec=3 
                          completetypes missing order=data; 
				var &var. CFB;	
				class &class_var.;
				ods output summary = Temporary_&var._D02;
			run;
			ods select all;

			*** calculate p values;
			proc sort data = Temporary_&var._D01; by &class_var.;run;
			ods select none;
			ods output Statistics = Temporary_&var._D03_CL TTests = Temporary_&var._D03_tp;
			proc ttest data=Temporary_&var._D01 (where=(visit ne 2)) /***sides=u***/ alpha=0.05 CL=equal;
			    by &class_var.;
	              var CFB; 
			run;

			ods output TestsForLocation = Temporary_&var._D03_wp;
			proc univariate data=Temporary_&var._D01 (where=(visit ne 2)) normal;
			    by &class_var.;
	              var CFB; 
			run;
			ods select all;

			*** Data manage with CL and p, merge with surmmary statistics;
			data Temporary_&var._D03_CL;
			    set Temporary_&var._D03_CL;
			    keep VISIT Variable LowerCLMean UpperCLMean;
			run;
			data Temporary_&var._D03_tp;
			    set Temporary_&var._D03_tp;
			    keep VISIT Variable Probt;
			    rename Probt=t_test_p;
			run;
			data Temporary_&var._D03_wp;
			    set Temporary_&var._D03_wp;
			    if Test = "Signed Rank";
			    keep VISIT VarName pValue;
			    rename pValue=wilcoxon_test_p
                          VarName = Variable;
			run;
			proc sort data = Temporary_&var._D02;    by VISIT Variable; run;
			proc sort data = Temporary_&var._D03_CL; by VISIT Variable; run;
			proc sort data = Temporary_&var._D03_tp; by VISIT Variable; run;
			proc sort data = Temporary_&var._D03_wp; by VISIT Variable; run;
			data Temporary_&var._D04;
			    merge Temporary_&var._D02 Temporary_&var._D03_CL
			          Temporary_&var._D03_tp Temporary_&var._D03_wp;
			    by VISIT Variable; 
			    if Variable = "CFB" then order2 = 2;
			    else order2 = 1;
			    order1 = &i.;
			    if visit = 2 and Variable = "CFB" then delete;
			run;
			
			*** Format for report;
			proc sort data = Temporary_&var._D04; by order1 VISIT order2; run;
			data &data_out._&var.;
			    length VISIT2 t_test_p_c wilcoxon_test_p_c $20;
			    format   N		     COMMA2.0
					   NMiss	     COMMA2.0
					   Min		COMMA6.2
					   Max		COMMA6.2
					   Mean		COMMA7.3
					   LowerCLMean COMMA7.3
					   UpperCLMean COMMA7.3
					   StdDev		COMMA6.2
					   Q1		COMMA6.2	
					   Median		COMMA7.3
					   Q3		COMMA6.2
					   t_test_p              PVALUE5.3
					   wilcoxon_test_p       PVALUE5.3
					   ;	
				set Temporary_&var._D04;
				array character {11} $20. N_c NMiss_c Min_c Max_c Mean_c LCL_c       UCL_c         StdDev_c Q1_c Median_c Q3_c ;
				array num       {11}      N   NMiss   Min   Max   Mean   LowerCLMean UpperCLMean   StdDev   Q1   Median   Q3   ;
				do i=1 to 11;
				  if num[i] =  . then character[i]  = "";
				  if num[i] ne . then do;
				  	if      num[i] ge  100 then character[i]  = strip(vvalue(num[i]));
					else if num[i] ge   10 then character[i]  = "^_"||strip(vvalue(num[i]));
					else if num[i] ge    0 then character[i]  = "^_^_"||strip(vvalue(num[i]));
					else if num[i] gt  -10 then character[i]  = "^_"||strip(vvalue(num[i]));
					else if num[i] gt -100 then character[i]  = strip(vvalue(num[i]));
				  end;				
				end;

                    *** format p values;
				if t_test_p ne . and t_test_p le 0.05 then t_test_p_c = "^_^_"||"^R/RTF'\b' "||strip(vvalue(t_test_p));
				if t_test_p ne . and t_test_p gt 0.05 then t_test_p_c = "^_^_"||strip(vvalue(t_test_p));

				if wilcoxon_test_p ne . and wilcoxon_test_p le 0.05 then wilcoxon_test_p_c = "^_^_"||"^R/RTF'\b' "||strip(vvalue(wilcoxon_test_p));
				if wilcoxon_test_p ne . and wilcoxon_test_p gt 0.05 then wilcoxon_test_p_c = "^_^_"||strip(vvalue(wilcoxon_test_p));

				if Variable = "CFB" and t_test_p_c = ""        then t_test_p_c = "^_^_^_^_N/F";
				if Variable = "CFB" and wilcoxon_test_p_c = "" then wilcoxon_test_p_c = "^_^_^_^_N/F";

				filler="";

				if visit = 2 then VISIT2="Baseline";
				if visit = 3 then VISIT2="Days 30";
				if visit = 4 then VISIT2="Days 60";
				if visit = 5 then VISIT2="Days 90";
				if visit = 6 then VISIT2="Days 120";
			run;

			*** clean the datasets;
			proc datasets lib=work nolist; delete Temporary_&var._:;quit;

	%end;
%end;
/*************************************************************/
/***  End: Descriptove statistic for continious variables  ***/
/*************************************************************/

 
/*************************************************************/
/***Start: Descriptove statistic for categorical variables ***/
/*************************************************************/

%if %sysfunc(countw(&Categ_var, %str( ), q)) ne 0 %then %do;
	%do i=1 %to %sysfunc(countw(&Categ_var, %str( ), q));
		%let var=%scan(&Categ_var,&i);
		%put &var;
          
		data Temporary_&var._D01; set &data_in.;run;
		data Temporary_&var._D01_nm; set &data_in.;if strip(vvalue(&var.)) ne "Missing"; run;
		proc sort data= Temporary_&var._D01;by &class_var.;run;	
		proc sort data= Temporary_&var._D01_nm;by &class_var.;run;		
		*** calculate the count and percentage;
		ods select None;
		proc tabulate data =  Temporary_&var._D01 out = Temporary_&var._D02a;
			by &class_var.;
			class &var.;
			table &var.,colpctn all;
		run;
		proc tabulate data =  Temporary_&var._D01_nm out = Temporary_&var._D02b;
			by &class_var.;
			class &var.;
			table &var.,colpctn all;
		run;
          ods select all;
          
		proc sort data=Temporary_&var._D02a; by &class_var. &var.; run;
		proc sort data=Temporary_&var._D02b; by &class_var. &var.; run;
		data Temporary_&var._D02;
		    merge Temporary_&var._D02a(drop=PctN_0) Temporary_&var._D02b;
		    by &class_var. &var.;
		run;

		*** Calculate the p-value based on Chi-square test;
		*** Get the baseline value; 
		proc sort data= Temporary_&var._D01;by &by_var.;run;		
		data Temporary_&var._D03;																	
			set Temporary_&var._D01;
			by &by_var.;	
			retain baseline ;
			if first.SUBJECT_ID then do;
				if Visit = 2 then baseline = &var.;
			end;
		run;
          
          *** Check the condition of proc freq;
		ods select none;
          ods output OneWayFreqs = Temporary_&var._D03_check;
          proc freq data=Temporary_&var._D03; tables baseline; run;
          proc sql noprint; select Percent into :check from Temporary_&var._D03_check (obs = 1); quit;
		%put &check;
		ods select all;
          
		%if &check. ne 100.00 %then %do;
          proc sort data=Temporary_&var._D03;by &class_var.;run;	
		ods select none;
		ods  output ChiSq = Temporary_&var._D04;
		**ods select None;
		proc freq data=Temporary_&var._D03;	
		     by &class_var.;
          	tables &var.*baseline / chisq;
		run;
		ods select all;

		***merge the p-values with the summary statistics;
		data Temporary_&var._D04;
		    set Temporary_&var._D04;
		    if Statistic="Chi-Square";
		    if Visit = 2 then delete;
		    &var. = .;
		    keep Visit &class_var. &var. Prob;
		run;
		data Temporary_&var._D02;
		    set Temporary_&var._D02;
		    keep N PctN_0 &var. &class_var.;
		run;
		proc sort data = Temporary_&var._D02;by &class_var. &var.;run;
		proc sort data = Temporary_&var._D04;by &class_var. &var.;run;

		data &data_out._&var.;
		    length VISIT2 $20;
		    format   N		     COMMA2.0
				   PctN_0	     COMMA6.1
				   Prob        PVALUE5.3
				   ;	
			merge Temporary_&var._D02 Temporary_&var._D04;
			by &class_var. &var.;

			filler="";
			if visit = 2 then VISIT2="Baseline";
			if visit = 3 then VISIT2="Days 30";
			if visit = 4 then VISIT2="Days 60";
			if visit = 5 then VISIT2="Days 90";
			if visit = 6 then VISIT2="Days 120";
		run;
		%end;

		%if &check. eq 100.00 %then %do;
		data &data_out._&var.;
		    length VISIT2 $20;
		    format   N		     COMMA2.0
				   PctN_0	     COMMA6.1
				   Prob        PVALUE5.3
				   ;	
			set Temporary_&var._D02 ;
			by &class_var. &var.;

			filler="";
			if visit = 2 then VISIT2="Baseline";
			if visit = 3 then VISIT2="Days 30";
			if visit = 4 then VISIT2="Days 60";
			if visit = 5 then VISIT2="Days 90";
			if visit = 6 then VISIT2="Days 120";
		run;
		%end;

		*** clean the datasets;
		proc datasets lib=work nolist; delete Temporary_&var._:;quit;
	%end;
%end;
/**************************************************************/
/***  End: Descriptove statistic for categorical variables  ***/
/**************************************************************/
%mend;
%VAR;




/************************************/
/**************  SELS  **************/
/************************************/
%VAR(data_in     = Inbonta_analysis_sels,
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = SER SESC SEM SEW,
	Categ_var   =  ,
	data_out    = Report_sels); 
 
/************************************/
/***********  Roughness  ************/
/************************************/
%VAR(data_in     = Inbonta_analysis_roughness,
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = LEN R1 R2 R3 R4 R5,
	Categ_var   =  ,
	data_out    = Report_roughness); 

/************************************/
/***** Physiology: Corneometry  *****/
/************************************/
%VAR(data_in     = Inbonta_analysis_corneometry,
	by_var      = SUBJECT_ID VISIT SIDE,
	class_var   = VISIT SIDE,
	Conti_var   = CORNEO,
	Categ_var   = CAT,
	data_out    = Report_Corneometry);
 
/************************************/
/****** Physiology: Cutometry  ******/
/************************************/
%VAR(data_in     = Inbonta_analysis_cutometry,
	by_var      = SUBJECT_ID VISIT SIDE,
	class_var   = VISIT SIDE,
	Conti_var   = R0 R1 R2 R3 R4 R5 R6 R7 R8 R9,
	Categ_var   =  ,
	data_out    = Report_cutometry);
 
/************************************/
/*********  Physiology: PH  *********/
/************************************/
%VAR(data_in     = Inbonta_analysis_ph,
	by_var      = SUBJECT_ID VISIT SIDE,
	class_var   = VISIT SIDE,
	Conti_var   = PH,
	Categ_var   =  ,
	data_out    = Report_PH); 


/************************************/
/********  Physiology: TEWL  ********/
/************************************/
%VAR(data_in     = Inbonta_analysis_tewl,
	by_var      = SUBJECT_ID VISIT SIDE,
	class_var   = VISIT SIDE,
	Conti_var   = TEWL,
	Categ_var   = CAT,
	data_out    = Report_tewl); 

 
/************************************/
/******  Physiology: Sebumetry  *****/
/************************************/
%VAR(data_in     = Inbonta_analysis_sebumetry,
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = SEBU,
	Categ_var   = CAT,
	data_out    = Report_sebumetry); 

/************************************/
/*********  Skin Wrinkles  **********/
/************************************/
%VAR(data_in     = Inbonta_analysis_wrinkles(where=(SASSQ_RATER="Expert 1")),
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = ,
	Categ_var   = Forehead Glabella L_perior R_perior,
	data_out    = Report_Wrinkles_ep); 

%VAR(data_in     = Inbonta_analysis_wrinkles(where=(SASSQ_RATER="Subject")),
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = ,
	Categ_var   = Forehead Glabella L_perior R_perior,
	data_out    = Report_Wrinkles_sj); 
 

/************************************/
/*************  GICS  ***************/
/************************************/
%VAR(data_in     = Inbonta_analysis_gics(where=(GICS_RATER="Expert")),
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = ,
	Categ_var   = GICS_CAT responder,
	data_out    = Report_GICS_ep); 

%VAR(data_in     = Inbonta_analysis_gics(where=(GICS_RATER="Subject")),
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = ,
	Categ_var   = GICS_CAT responder,
	data_out    = Report_GICS_sj); 
 
 

/************************************/
/********  Physiology: FASAQ  *******/
/************************************/
%VAR(data_in     = Inbonta_analysis_fasaq,
	by_var      = SUBJECT_ID VISIT,
	class_var   = VISIT,
	Conti_var   = Q_8,
	Categ_var   = Q_1 Q_2 Q_3 Q_4 Q_5 Q_6 Q_7,
	data_out    = Report_fasaq); 


/********************************************************************************************************/
/***************************************** End: Data analysis *******************************************/
/********************************************************************************************************/

/*********************************************
Tidy up
*********************************************/
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


proc datasets lib=work nolist;
    delete Inbonta_0: Inbonta_10:;
quit;
















/************************************************************************************/ 
/*******************************  Start: Report   ***********************************/
/************************************************************************************/ 


 
*** Change the output destination to SAS and empty the log as well as the output window;
ods listing;
dm log 'clear';
dm output 'clear';
ods exclude all;

*** Clear old titles and footnotes;
title;
footnote;

%let Output_folder  = C:\Users\zbai\Desktop\SAS\Output;
%let This_prog      = report;





/*****************************************************/
/***       Evaluation of Secondary Endpoints       ***/
/***                    FASAQ                      ***/
/*****************************************************/

*** Merge catogories with full glossary datasets;
proc format library=work cntlout=format_dataset; run;

data format_dataset_FASAQ_CAT; 
	set format_dataset(where=(FMTNAME="AGREE_CAT"));
	CAT=input(START,??comma32.); 
	do visit = 2 to 6; 
       output;
	end;
	keep CAT visit Label;
run;
proc sort data = format_dataset_FASAQ_CAT;             by  visit CAT; run;



/****************************************
%let vardata = Report_fasaq_q_1;
%let i=1;
****************************************/
%Macro CateMerge (var_string,);
%Macro HL; %Mend HL;

%do i = 1 %to 7;
%let vardata = Report_fasaq_q_&i.; 

data &vardata.a; set &vardata.; run;
proc sort data = &vardata.a (rename=(Q_&i.=CAT));     by  visit CAT; run;

data &vardata.b;
	length N_c P_c $20.;
	merge &vardata.a(drop=Prob) format_dataset_FASAQ_CAT (in=a);
	by visit CAT;
	if a;

	if      N eq .  then N_c = "^_0";
	else if N lt 10 then N_c = "^_"||strip(vvalue(N));
	else if N ge 10 then N_c = strip(vvalue(N));

	if      PctN_0 eq .   then P_c = "^_^_0.0%";
	else if PctN_0 lt  10 then P_c = "^_^_"||strip(vvalue(PctN_0))||"%";
	else if PctN_0 ge  10 then P_c = "^_"||strip(vvalue(PctN_0))||"%";
	else if PctN_0 eq 100 then P_c = strip(vvalue(PctN_0))||"%";
	if  LABEL = "Missing" then P_c = "";

	if visit = 2 then VISIT2="Baseline";
	if visit = 3 then VISIT2="Days 30";
	if visit = 4 then VISIT2="Days 60";
	if visit = 5 then VISIT2="Days 90";
	if visit = 6 then VISIT2="Days 120";

	order2 = CAT;
run;

*** p values;
data &vardata.b;
    length Prob_c $20.;
    set &vardata.b &vardata.a(where=(CAT=.));

    if Prob ne . and Prob le 0.05 then Prob_c = "^_^_"||"^R/RTF'\b' "||strip(vvalue(Prob));
    if Prob ne . and Prob gt 0.05 then Prob_c = "^_^_"||strip(vvalue(Prob));  
run;
proc sort data =  &vardata.b; by  visit CAT; run;

%end;
%Mend CateMerge;
%CateMerge;


 
*** merge together;
data T14_3_6_1_FASAQ;
    length Variable parameter $30.;
    set Report_fasaq_q_1b (in=a)
	   Report_fasaq_q_2b (in=b)
	   Report_fasaq_q_3b (in=c)
	   Report_fasaq_q_4b (in=d)
	   Report_fasaq_q_5b (in=e)
	   Report_fasaq_q_6b (in=f)
	   Report_fasaq_q_7b (in=g)
        ;
    if a then do; parameter = "Q1";  order1=1; end;
    if b then do; parameter = "Q2";  order1=2; end;
    if c then do; parameter = "Q3";  order1=3; end;
    if d then do; parameter = "Q4";  order1=4; end;
    if e then do; parameter = "Q5";  order1=5; end;
    if f then do; parameter = "Q6";  order1=6; end;
    if g then do; parameter = "Q7";  order1=7; end;

    Variable=LABEL;
run;
proc sort data = T14_3_6_1_FASAQ; by order1 visit order2; run;


data T14_3_6_2_FASAQ;
    length Variable parameter $30.;
    set Report_fasaq_q_8;
    parameter = "Q8";  
    if Variable ne "CFB" then Variable = "raw";
run;
proc sort data = T14_3_6_2_FASAQ; by order1 visit order2; run;




%macro report1 (Report_in, Table_Nr, Lable1, Lable2, Lable3, Analysisset, Footnote, This_prog);
%macro HL; %mend HL;
ods tagsets.rtf file="&Output_folder.\&This_prog..rtf" tablerows=25 style=gcpservice_style;
ods escapechar="^";	
options nodate nonumber nocenter;

ods exclude none;
ods proclabel="&Table_Nr.: &Lable3.";

ods listing close;
ods exclude none;
option byline;

proc report data=&report_in. headline headskip nowd 
					    split="|" missing contents=''
					    style(header)=[fontweight=bold];

	title1 j=l height=10pt font=courier "&Table_Nr.: ^R/RTF'\tab' &Lable1.";
	title2 j=l height=10pt font=courier "^R/RTF'\tab\tab\tab' &Lable2.";
	title3 j=l height=10pt font=courier "^R/RTF'\tab\tab\tab' &Lable3.";
	title5 j=l height=10pt font=courier "Analysis set: &Analysisset.";

	footnote1 j=l height=8pt font=courier "n: Number of non-missing observations; %: Percentage based on non-missing observations; p-value: p-value was calculated based on the chi square test to compare the post treatment and baseline;";
     footnote2 j=l height=8pt font=courier &Footnote.; 
	footnote4 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program '&this_prog.'";

	columns (Parameter filler Visit2 filler Variable  N_c P_c Prob_c);	
	define Parameter 	     / "^R/RTF'\ql' Parameter^n" order 	order=data  style(column)=[cellwidth=4.5 cm just=l];
	define Visit2 	          / "^R/RTF'\ql' Visit^n"     order 	order=data  style(column)=[cellwidth=2.7 cm just=l];
	define Variable 	     / "^R/RTF'\ql' Category^n"  display            style(column)=[background=lightyellow cellwidth=4.8 cm just=l];
	define N_c		     / "^R/RTF'\ql' ^_n^n"	   display            style(column)=[background=lightyellow cellwidth=2.0 cm just=l];
	define P_c		     / "^R/RTF'\ql' ^_^_^_%^n"   display            style(column)=[background=lightyellow cellwidth=3.5 cm just=l];
	define Prob_c	          / "p-value^n"	             display            style(column)=[background=lightyellow cellwidth=3.8 cm just=l];
	define filler 	      	/ ""					   display            style(column)=[cellwidth=0.1 cm];

	compute before ;									
	line ' ';
	endcomp;

	compute after Visit2;
	line ' ';
	endcomp;
run;
quit;

ods tagsets.rtf close;
ods _all_ close; 

%mend report1;

%report1(Report_in     = T14_3_6_1_FASAQ,
         Table_Nr  	   = Table 14.3.6.1,
	    Lable1    	   = Secondary Endpoints,
	    Lable2    	   = Patient satisfaction by FASAQ (Q1-Q7),
	    Lable3    	   = Response at patient satisfaction by FASAQ at days 30 60 90 120 and 150,
	    Analysisset   = PPS,
	    Footnote      = "Other footnotes will be added here;",
         This_prog	   = InboTNA_T14_3_6_1_FASAQ_D01_0_0);














%macro words(string,root=);
%*--------------------------------------------------;
%* Return number of words in string. If root ^' ',  ;
%* then create global variables starting with root. ;
%*--------------------------------------------------;
   %local count word;
   %let count=1;
   %let word = %scan(&string,&count,%str( ));
   %do %while(%quote(&word)^= );
	%*put WORDS: word=&word;
       %if &root^=  %then %do;
          %global &root&count;
          %let &root&count=&word;
       %end;
       %let count = %eval(&count+1);
       %let word = %scan(&string,&count,%str( ));
   %end;
   %eval(&count-1)
%mend words;
/******************************************************
%put words() = %words(hhh ggg);
%put words = %words(A B C,root=W);
******************************************************/

%macro multisummary(
	data=_last_,    /* name of input data set */
	var=_numeric_,  /* names of analysis variables                      */
	class=,         /* name(s) of  0 or more class variables            */
	stats=,         /* names of output statistics to calculate          */
	options=,       /* options for proc summary, e.g., nway, missing... */
	out=
	);

%let nstat=%words(&stats);

options nonotes;
proc datasets nolist nowarn;
	delete &out;

%do i=1 %to &nstat;
	%let stat=%scan(&stats, &i);
	proc summary data=&data &options;
	%if %length(&class) %then %do;
		class &class;
		%end;
	var &var;
	output out=_out  &stat= ;

	data _out;
		length _statistic_ $8;
		set _out;
		_statistic_ = "&stat";
		label _statistic_ = 'Name of statistic';
	
	proc append base=&out new=_out;
	run;
	%end;

%if %length(&class) %then %do;
proc sort data=&out;
	by &class;
	%end;

proc datasets nolist nowarn;
	delete _out;
options notes;

%mend multisummary;

%multisummary(data=Inbonta_analysis_fasaq, var=Q_8, class = visit, stats=n mean std MEAN MEDIAN Q1 Q3, options=missing, OUT=T14_3_6_1_fasaqQ8);	
 









%macro report2 (Report_in, Table_Nr, Lable1, Lable2, Lable3, Analysisset, Footnote, This_prog);
%macro HL; %mend HL;
ods tagsets.rtf file="&Output_folder.\&This_prog..rtf" tablerows=25 style=gcpservice_style;
ods escapechar="^";	
options nodate nonumber nocenter;

ods exclude none;
ods proclabel="&Table_Nr.: &Lable3.";

ods listing close;
ods exclude none;
option byline;

proc report data=&report_in. headline headskip nowd split="|" missing contents='';

	title1 j=l height=10pt font=courier "&Table_Nr.: ^R/RTF'\tab' &Lable1.";
	title2 j=l height=10pt font=courier "^R/RTF'\tab\tab\tab' &Lable2.";
	title3 j=l height=10pt font=courier "^R/RTF'\tab\tab\tab' &Lable3.";
	title5 j=l height=10pt font=courier "Analysis set: &Analysisset.";

	footnote1 j=l height=8pt font=courier "n: Number of non-missing observations; miss: Number of missing observations; SD: Standard deviation; LCI: lower 95% confidence limit of the mean difference; UCI: upper 95% confidence limit of the mean difference; Min: minimum; Q1: lower quartile; Q3: upper quartile; Max: maximum; p-value^{super 1}: p-value based on the paired t-test between the post treatment and baseline; p-value^{super 2}: p-value based on the wilcoxon signed rank test between the post treatment and baseline;";
	footnote2 j=l height=8pt font=courier &Footnote.;
	footnote4 j=l height=8pt font=courier "^R/RTF'\ql' Output generated by program '&this_prog.'";

	columns (Parameter filler Visit2 filler Variable  N_c NMiss_c Mean_c LCL_c UCL_c StdDev_c Min_c Q1_c Median_c Q3_c Max_c wilcoxon_test_p_c t_test_p_c);	
	define Parameter 	     / "^R/RTF'\ql' Parameter^n" order 	order=data  style(column)=[cellwidth=1.8 cm just=l];
	define Visit2 	          / "^R/RTF'\ql' Visit^n"     order 	order=data  style(column)=[cellwidth=1.7 cm just=l];
	define Variable 	     / ""                        order 	order=data  style(column)=[cellwidth=1.2 cm just=l];
	define N_c		     / "^R/RTF'\qc' n^n"		   display            style(column)=[cellwidth=0.9 cm just=l];
	define NMiss_c		     / "^R/RTF'\qc' miss^n"	   display            style(column)=[cellwidth=0.8 cm just=l];
	define Mean_c	          / "^R/RTF'\qc' Mean^n"	   display            style(column)=[cellwidth=1.5 cm just=l];
	define LCL_c	          / "^R/RTF'\qc' LCL^n"	   display            style(column)=[cellwidth=1.5 cm just=l];
	define UCL_c	          / "^R/RTF'\qc' UCL^n"	   display            style(column)=[cellwidth=1.5 cm just=l];
	define StdDev_c	     / "^R/RTF'\qc' SD^n"	   display            style(column)=[cellwidth=1.5 cm just=l];
	define Min_c		     / "^R/RTF'\qc' Min^n"	   display            style(column)=[cellwidth=1.6 cm just=l];
	define Q1_c		     / "^R/RTF'\qc' Q1^n"	   display            style(column)=[cellwidth=1.6 cm just=l];
	define Median_c	     / "^R/RTF'\qc' Median^n"	   display            style(column)=[cellwidth=1.6 cm just=l];
	define Q3_c		     / "^R/RTF'\qc' Q3^n"	   display            style(column)=[cellwidth=1.6 cm just=l];
	define Max_c		     / "^R/RTF'\qc' Max^n"	   display            style(column)=[cellwidth=1.6 cm just=l];
	define t_test_p_c	     / "p-value^{super 1}^n"	   display            style(column)=[cellwidth=1.8 cm just=l];
	define wilcoxon_test_p_c	/ "p-value^{super 2}^n"	   display            style(column)=[cellwidth=1.8 cm just=l];
	define filler 	      	/ ""					   display            style(column)=[cellwidth=0.1 cm];

	compute before ;									
	line ' ';
	endcomp;

	compute after Visit2;
	line ' ';
	endcomp;
run;
quit;

ods tagsets.rtf close;
ods _all_ close; 

%mend report2;


%report2(Report_in    = T14_3_6_2_FASAQ,
         Table_Nr  	   = Table 14.3.6.2,
	    Lable1    	   = Secondary Endpoints,
	    Lable2    	   = Patient satisfaction by FASAQ (Q8),
	    Lable3    	   = Response at patient satisfaction by FASAQ at days 30 60 90 120 and 150,
	    Analysisset   = PPS,
	    Footnote      = "Other footnotes will be added here;",
         This_prog	   = InboTNA_T14_3_6_2_FASAQ_D01_0_0);


/************************************************************************************/ 
/*******************************   End: Report    ***********************************/
/************************************************************************************/ 






/************************************************************************************
                                      Tidy up
*************************************************************************************/ 

*** Delete temporary datasets;
proc datasets lib=work nolist; 
	delete T14_3_6_1_FASAQ T14_3_6_2_FASAQ;
quit;

proc catalog cat=work.sasmacr;
	delete CateMerge report/ et=macro;
quit;

*** delete all temporary macro variables that could lead to errors in later programs;
%deleteALL;


 
