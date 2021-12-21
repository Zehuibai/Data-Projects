/************************************************************************************
Programme name: 	Gebaermutter_D01_0_0.sas
Programme language: SAS 9.4
Initial date: 		01/DEC/2021
Author(s): 		ZBA
*************************************************************************************
Short description: 	SAS Consulting
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

 
%*--------------------------------------------------;
%*               Load the data                      ;
%*--------------------------------------------------;

data gebaermutter_analysis_D01;
	set "C:\Users\zbai\Documents\GitHub\R-Projects\SAS\Faith\gebaermutter.sas7bdat";
	/***** 
	array Nums[*] _numeric_;
	******/

	array Chars[*] _character_;
     /*************
	do i = 1 to dim(Nums);
		if Nums[i] = 999 then Nums[i] = .;
	end;
	*************/

	do j = 1 to dim(Chars);
		if Chars[j] = "" then Chars[j] = "Miss";
		if Chars[j] = "N/A" then Chars[j] = "Miss";
	end;
	drop j;
run;


%*--------------------------------------------------;
%*               Overview the data                  ;
%*--------------------------------------------------;
proc contents data = gebaermutter_analysis_D01 out = gebaermutter_analysis_D02;run;
proc sort data = gebaermutter_analysis_D02; by VARNUM; run;

proc sql;
     * create macro list of the numeric variables' names;
     select name
     into :numerics
          separated by ' '
     from gebaermutter_analysis_D02
     where type = 1;

     * create macro list of the numeric variables' names with the suffix "C";
     /* select trim(name) || 'C'*/
	select trim(name)
     into :characters
          separated by ' '
     from gebaermutter_analysis_D02
     where type = 2;

     * create macro list of the conversion from the original name to the new name with the suffix "C";
     select cats(name, ' = ' , name, '_c')
     into :conversions
          separated by ' '
     from gebaermutter_analysis_D02
     where type = 2;
quit;

%put &numerics.;
%put &characters.;
%put &conversions.;



proc sql noprint;
	select distinct cats(name,'=',name,'_c') into :renames separated by ' ' from gebaermutter_analysis_D02 where type=2;
quit;
%put &=renames;


/**** gebaermutter_analysis_D03 with all renamed vars ***/
data gebaermutter_analysis_D03; set gebaermutter_analysis_D01; run;
proc datasets library=work;
	modify gebaermutter_analysis_D03;
	rename &renames;
run;
quit;



 
 
 
%*--------------------------------------------------;
%*                 Format the data                  ;
%*--------------------------------------------------;

%let Charvar = age;

proc freq data = gebaermutter_analysis_D01 noprint;
	table &Charvar./missing out=Freq_&Charvar.;
run;
 


/********************************
        Example for SEX
********************************/

proc format;
	value sex_FMT 1   = "Divers"
		         2   = "Männlich"
		         3   = "Weiblich"
		         999 = "Miss";
run;


/***********************
%let Chr_Var = wi1_t1_c;
%let Num_Var = wi1_t1;
***********************/

%Macro JN_Fmt (Chr_Var, Num_Var);
	if       &Chr_Var. = "Ja"   then &Num_Var. = 1;
	else if  &Chr_Var. = "Nein" then &Num_Var. = 2;
	else if  &Chr_Var. = "Miss" then &Num_Var. = .;
%Mend JN_Fmt;


/*** Gebaermutter_analysis_d04 is the final clean dataset which needed to be exported ***/
data Gebaermutter_analysis_d04;
	/*** Rename the char variabes to char naem ***/
	set gebaermutter_analysis_D03;

	/*** Coding the categories ***/

	if      sex_c = "Weiblich"  then sex = 1;
	else if sex_c = "Männlich"  then sex = 2;
	else if sex_c = "Divers"    then sex = 3;
	else if sex_c = "Miss"      then sex = .;

	%JN_Fmt(wi1_t1_c, wi1_t1);
	%JN_Fmt(wi1_t2_c, wi1_t2);

	** format sex sex_FMT.;
	drop  sex_c wi1_t1_c;
run;




/*******************************
Export SAS

1. export sas dataset sas7bdat:
	* Proc export
	* Specify libname Faith; data Faith.__;set __;
     * Contents colomn
2. export csv
     * Proc export (.csv)
     * Contents colomn

Copy data
1. Select
2. Copy cltr+C
3. Paste cltr+V
********************************/

 



 





%*--------------------------------------------------;
%*              Impute the data  (example)          ;
%*--------------------------------------------------;

*** Datasets;
proc format;
  value female 0 = "male"
               1= "female";
  value prog 1 = "general"
             2 = "academic"
             3 = "vocation" ;
  value race 1 = "hispanic"
             2 = "asian"
             3 = "african-amer"
             4 = "white";
  value schtyp 1 = "public"
               2 = "private";
  value ses  1 =  "low"
             2 = "middle"
             3 = "high";
run;
options fmtsearch=(work);

data new;
    set "C:\Users\zbai\Documents\GitHub\R-Projects\SAS\Faith\hsb_mar.sas7bdat";
    if prog ^=. then do;
    if prog =1 then progcat1=1;
    else progcat1=0;
    if prog =2 then progcat2=1;
    else progcat2=0;
    end;
run;


data hsb_flag;
    set new;
    if female =.  then female_flag =1; else female_flag =0;
    if write  = . then write_flag  =1; else write_flag  =0;
    if read   = . then read_flag   =1; else read_flag   =0;
    if math   = . then math_flag   =1; else math_flag   =0;
    if prog   = . then prog_flag   =1; else prog_flag   =0;
run;



/*** misspattern ***/
proc mi data=HSB_flag nimpute=0 ;
    var socst write read female math prog;
    ods select misspattern;
run;







/***  MI using fully conditional specification ***/
proc mi data= new nimpute=20 out=mi_fcs ;
	class female prog; 
	var socst write read female math science prog;
	fcs logistic(female prog /link=glogit) regpmm(math read write); 
run;
 
proc genmod data=mi_fcs;
	class female prog;
	model read= write female math prog /dist=normal ;
	by _imputation_;
	ods output ParameterEstimates=gm_fcs;
run;


title " multiple imputation linear regression - fcs";
proc mianalyze parms(classvar=level)=gm_fcs;
	class female prog;
	modeleffects intercept write female math prog;
run;
