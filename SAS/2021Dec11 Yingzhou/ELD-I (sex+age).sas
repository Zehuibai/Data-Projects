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





* ELD-I nach Alter und Geschlecht;

libname yz 'E:\Masterarbeit\SAS';
run;

data Database;
set yz.ana_data;
run;



/*data Database;set "C:\Users\zbai\Desktop\ana_data.sas7bdat";run;*/



data database;
	set database;
	ufa_in=pufa_in + mufa_in;
	drop pufa_in mufa_in;
run;

 



proc means data=Database min mean max median std;
var age energy fish_in grain_in veggie_in chicken_in fruit_in dairy_in legumes_in meat_in potat_in egg_in nuts_in ufa_in sfa_in sugar_in ;
run;

data a;
set database;
if bmi_score_cole_12 <= -2 then bmi_score_kat = 1; * Thinness;
if -2 < bmi_score_cole_12 and bmi_score_cole_12 <= 1 then bmi_score_kat = 2; * Normal;
if 1 < bmi_score_cole_12 and bmi_score_cole_12 <= 2 then bmi_score_kat = 3; * Overweight;
if 2 < bmi_score_cole_12 then bmi_score_kat = 4; * Obesity;
run;

data a;
set a;
if sex=1 and 3 <= age and age < 4 then age_grp = 1; 
if sex=1 and 4 <= age and age < 5 then age_grp = 2; 
if sex=1 and 5 <= age and age < 6 then age_grp = 3; 
if sex=1 and 6 <= age and age < 7 then age_grp = 4; 
if sex=1 and 7 <= age and age < 8 then age_grp = 5; 
if sex=1 and 8 <= age and age < 9 then age_grp = 6; 
if sex=1 and 9 <= age and age < 10 then age_grp = 7; 
if sex=1 and 10 <= age and age < 11 then age_grp = 8; 
if sex=1 and 11 <= age and age < 12 then age_grp = 9; 
if sex=1 and 12 <= age and age < 13 then age_grp = 10; 
if sex=1 and 13 <= age and age < 14 then age_grp = 11; 
if sex=1 and 14 <= age and age <= 15 then age_grp = 12; * 1-12 sind fuer Jungen;
if sex=2 and 3 <= age and age < 4 then age_grp = 13; 
if sex=2 and 4 <= age and age < 5 then age_grp = 14; 
if sex=2 and 5 <= age and age < 6 then age_grp = 15; 
if sex=2 and 6 <= age and age < 7 then age_grp = 16; 
if sex=2 and 7 <= age and age < 8 then age_grp = 17; 
if sex=2 and 8 <= age and age < 9 then age_grp = 18; 
if sex=2 and 9 <= age and age < 10 then age_grp = 19; 
if sex=2 and 10 <= age and age < 11 then age_grp = 20; 
if sex=2 and 11 <= age and age < 12 then age_grp = 21; 
if sex=2 and 12 <= age and age < 13 then age_grp = 22; 
if sex=2 and 13 <= age and age < 14 then age_grp = 23; 
if sex=2 and 14 <= age and age <= 15 then age_grp = 24; *13-24 sind fuer Maedchen;
run;

proc freq data=a;
tables sex isced bmi_score_kat age_grp;
run;




/*Begin; 11/12/2021 by ZBA*/
 
/*load cat off dataset*/
*** Define the local path;
%let cutoff_path = C:\Users\zbai\Documents\GitHub\R-Projects\SAS\Yingzhou;
proc import  datafile="&cutoff_path.\cut-off adjustment.csv"
	out=cutoff_D01
	dbms=csv
	replace;
	delimiter=",";
	getnames=yes;
	guessingrows=max;
run;

***format the dataset: change name and add index;
data cutoff_D02;
	set cutoff_D01(firstobs=2);
	age_grp = _N_;
	drop VAR1;
run;

*** rename;
proc contents data = cutoff_D02 out = cutoff_D03;run;
proc sort data = cutoff_D03; by VARNUM; run;
proc sql;
     * create macro list of the conversion from the original name to the new name with the suffix "C";
     select cats(name, ' = ' , name, '_cutoff')
     into :conversions
          separated by ' '
     from cutoff_D03
     where type = 1;
quit;
%put &conversions.;

data  cutoff_D04;
	set cutoff_D02(rename=(&conversions.));
	rename age_grp_cutoff=age_grp;
run;

/*merge with all cut off values*/
proc sort data = cutoff_D04; by age_grp; run;
proc sort data = a; 	    by age_grp; run;

data Database_cutoff;
	merge a cutoff_D04;
	by age_grp;
run;
proc sort data = Database_cutoff; by IFAMILY_ID; run;


/*delete datasets tidy up*/
proc datasets library=work nolist; delete cutoff_:;run; quit;


/*End; 11/12/2021 by ZBA*/

























* Score;

proc transpose data= a out= b(rename=(col1=intake));
var fish_in grain_in veggie_in chicken_in fruit_in dairy_in legumes_in meat_in potat_in egg_in nuts_in ufa_in sfa_in sugar_in; 
by IFAMILY_ID energy age_grp sex;
run;

data c;
set b;
if _NAME_="veggie_in" or _NAME_="fruit_in" or _NAME_="grain_in" or _NAME_="legumes_in" or _NAME_="nuts_in" then a=-1;
else a=1;
run;

data c;
set c;
if _NAME_="potat_in" then cut=100;
if _NAME_="fish_in" then cut=100;
if _NAME_="grain_in" then cut=464;
if _NAME_="veggie_in" then cut=200;
if _NAME_="chicken_in" then cut=58;
if _NAME_="fruit_in" then cut=100;
if _NAME_="dairy_in" then cut=500;
if _NAME_="legumes_in" then cut=100;
if _NAME_="egg_in" then cut=25;
if _NAME_="nuts_in" then cut=25;
if _NAME_="ufa_in" then cut=80;
if _NAME_="sfa_in" then cut=11.8;
if _NAME_="sugar_in" then cut=31;
if _NAME_="meat_in" then cut=28;
run;

data d;
set c;
if sex=1 and age_grp=1 then intake_1=a*(cut-(intake*1252/energy))/cut;
if sex=1 and age_grp=2 then intake_1=a*(cut-(intake*1360/energy))/cut;
if sex=1 and age_grp=3 then intake_1=a*(cut-(intake*1467/energy))/cut;
if sex=1 and age_grp=4 then intake_1=a*(cut-(intake*1573/energy))/cut;
if sex=1 and age_grp=5 then intake_1=a*(cut-(intake*1692/energy))/cut;
if sex=1 and age_grp=6 then intake_1=a*(cut-(intake*1830/energy))/cut;
if sex=1 and age_grp=7 then intake_1=a*(cut-(intake*1978/energy))/cut;
if sex=1 and age_grp=8 then intake_1=a*(cut-(intake*2150/energy))/cut;
if sex=1 and age_grp=9 then intake_1=a*(cut-(intake*2341/energy))/cut;
if sex=1 and age_grp=10 then intake_1=a*(cut-(intake*2548/energy))/cut;
if sex=1 and age_grp=11 then intake_1=a*(cut-(intake*2770/energy))/cut;
if sex=1 and age_grp=12 then intake_1=a*(cut-(intake*2990/energy))/cut;
if sex=2 and age_grp=1 then intake_1=a*(cut-(intake*1156/energy))/cut;
if sex=2 and age_grp=2 then intake_1=a*(cut-(intake*1241/energy))/cut;
if sex=2 and age_grp=3 then intake_1=a*(cut-(intake*1330/energy))/cut;
if sex=2 and age_grp=4 then intake_1=a*(cut-(intake*1428/energy))/cut;
if sex=2 and age_grp=5 then intake_1=a*(cut-(intake*1554/energy))/cut;
if sex=2 and age_grp=6 then intake_1=a*(cut-(intake*1698/energy))/cut;
if sex=2 and age_grp=7 then intake_1=a*(cut-(intake*1854/energy))/cut;
if sex=2 and age_grp=8 then intake_1=a*(cut-(intake*2006/energy))/cut;
if sex=2 and age_grp=9 then intake_1=a*(cut-(intake*2149/energy))/cut;
if sex=2 and age_grp=10 then intake_1=a*(cut-(intake*2276/energy))/cut;
if sex=2 and age_grp=11 then intake_1=a*(cut-(intake*2379/energy))/cut;
if sex=2 and age_grp=12 then intake_1=a*(cut-(intake*2449/energy))/cut;
run;

proc sort data=d;
by IFAMILY_ID;
run; 

data d;
set d;
by IFAMILY_ID;
retain intake_1 0;
if first.IFAMILY_ID then eld = 0;
eld + intake_1;
run;

data e;
set d;
by IFAMILY_ID;
eld_i=100*eld/14;
if last.IFAMILY_ID;
run;

proc sort data=a;
by IFAMILY_ID;
run;

proc sort data=e;
by IFAMILY_ID;
run;

data database2;
merge a e;
by IFAMILY_ID;
drop _NAME_ a cut intake_1 eld intake;
run;
data database2;
set database2
 (rename=( IFAMILY_ID=id_no));
run;



proc import datafile="yz.xlsx" 
			out=yz dbms=xlsx replace;
			getnames=yes;
run;
proc sort data=yz;
 by id_no;
run;

data database3;
merge database2 yz;
 by id_no;
run;

data database3;
set database3;
if sex=. or age=. then delete;
run;

proc contents data=database3;
run;


* Var: sex age sacana_in_T3 isced bmi_score_cole_12 energy fish_in grain_in veggie_in chicken_in fruit_in dairy_in legumes_in
meat_in potat_in egg_in nuts_in sfa_in sugar_in ufa_in bmi_score_kat age_grp eld_i sex_T3 age_T3 wb_score_T3 BF_2_T3
bedrmedia_number_T3 isced_cat2011_T3 pub_T3 bmi_cat_cole_12_T3 bmi_score_cole_12_T3 birth_w_T3 club_mbr_T3 country maternal_bmi;







* Deskriptive Analyse;
proc univariate data=database3 normal plot;              
var eld_i; 
histogram  eld_i; 
probplot eld_i;       
run;

* Stratifiezieren nach Geschlecht;
proc sort data=database3 out=database_sex;
by sex;
run;

proc means data=database_sex mean median var std n;
by sex;
var age_T3  energy  fish_in  grain_in  veggie_in  chicken_in  fruit_in  dairy_in  legumes_in  meat_in  potat_in  
egg_in  nuts_in  ufa_in  sfa_in  sugar_in  eld_i  birth_w_T3  age  BMI_score_cole_12  wb_score_T3  maternal_bmi;
run; 

proc freq data=database_sex;
tables sex  sacana_in_T3  isced  bmi_score_kat  country  sex_T3  BF_2_T3  bedrmedia_number_T3  isced_cat2011_T3  pub_T3  
club_mbr_T3;
run;
 
* Stratifiezieren nach Altersgruppen;
data database_age;
 set database3;
 if 3<= age <=6 then age_kat=1;
 if 6< age <=10 then age_kat=2;
 if 10< age <=15 then age_kat=3;
run;

proc sort data=database_age;
by age_kat;
run;

proc means data=database_age mean median var std n;
by age_kat;
var  energy  fish_in  grain_in  veggie_in  chicken_in  fruit_in  dairy_in  legumes_in  meat_in  potat_in  egg_in 
nuts_in  ufa_in  sfa_in  sugar_in  eld_i  birth_w_T3  age  bmi_score_cole_12  wb_score_T3  maternal_bmi;
run; 

proc freq data=database_age;
tables sex  sacana_in_T3  isced  bmi_score_kat  country  sex_T3  BF_2_T3  bedrmedia_number_T3  isced_cat2011_T3  pub_T3  
club_mbr_T3;
run;

* Stratifizieren nach eld_i;
proc sort data=database3 out=database_eld;
by descending eld_i ;
run;

proc rank data=database_eld out=database_eld DESCENDING ties=low groups=4;
   var eld_i;
   ranks eld_i_kat;
   run;

proc boxplot data=database_eld;
plot eld_i*eld_i_kat;
insetgroup min mean max std;
run;

proc freq data=database_eld;
tables isced*eld_i_kat sex*eld_i_kat sacana_in_T3*eld_i_kat  bmi_score_kat*eld_i_kat  age_grp*eld_i_kat  country*eld_i_kat  sex_T3*eld_i_kat
BF_2_T3*eld_i_kat  bedrmedia_number_T3*eld_i_kat; 
run;   * n + %;

proc means data=database_eld mean median var std;
by eld_i_kat;
var age  energy  fish_in  grain_in  veggie_in  chicken_in  fruit_in  dairy_in  legumes_in  meat_in  potat_in  egg_in 
nuts_in  ufa_in  sfa_in  sugar_in  eld_i  birth_w_T3  age  bmi_score_cole_12  wb_score_T3  maternal_bmi;
run; * mean + std;


* Regression Analyse;
proc corr data=database_eld spearman; 
	var eld_i eld_i_kat age sex sacana_in_T3  isced  bmi_score_kat  age_grp  country  sex_T3  BF_2_T3  bedrmedia_number_T3  
isced_cat2011_T3  pub_T3  club_mbr_T3  energy  fish_in  grain_in  veggie_in  chicken_in  fruit_in  dairy_in  legumes_in  meat_in  potat_in  egg_in 
nuts_in  ufa_in  sfa_in  sugar_in  birth_w_T3  bmi_score_cole_12  wb_score_T3  maternal_bmi;
run; * Korrelation aller Variablen, wenn >0.05 , dann ausgeschlossen?;

proc reg data=database_eld;
	model eld_i= age sex/ clb; 
run; * Baseline;

proc reg data=database_eld;
	model eld_i= age sex  bedrmedia_number_T3 country  wb_score_T3   pub_T3/ clb; 
run; * Baseline + lifestyle characteristics;

proc reg data=database_eld;
	model eld_i= age sex fish_in  veggie_in  chicken_in  fruit_in  dairy_in  legumes_in  meat_in  potat_in  
egg_in  nuts_in   sfa_in  sugar_in / clb; 
run; * Baseline + dietary characteristics;

proc reg data=database_eld;
	model eld_i= age sex  bedrmedia_number_T3 country  wb_score_T3   pub_T3  fish_in  veggie_in  chicken_in  fruit_in  
dairy_in  legumes_in  meat_in  potat_in  egg_in  nuts_in   sfa_in  sugar_in/ clb; 
run; * Alle Variablen;


proc logistic data=database_eld;
	model eld_i_kat= age sex / cl; 
run;

proc logistic data=database_eld;
	model eld_i_kat= age sex  bedrmedia_number_T3 country  wb_score_T3   pub_T3 / cl; 
run;  * Baseline + lifestyle characteristics;

proc logistic data=database_eld;
	model eld_i_kat= age sex  fish_in   veggie_in  chicken_in  fruit_in  dairy_in  legumes_in  meat_in  potat_in  
egg_in  nuts_in  sfa_in  sugar_in / cl; 
run; * Baseline + dietary characteristics;

proc logistic data=database_eld;
	model eld_i_kat= age sex  bedrmedia_number_T3 country  wb_score_T3   pub_T3 fish_in   veggie_in  chicken_in  fruit_in  dairy_in  
legumes_in  meat_in  potat_in   egg_in  nuts_in  sfa_in  sugar_in/ cl; 
run; * Alle Variablen;



