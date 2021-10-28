/************************************************************************************
Programme name: 	SAS_20211015.sas
Programme language: SAS 9.4
Initial date: 		15/OCT/2021
Author(s): 		ZBA
*************************************************************************************
Short description: 	
Requirements: 		---
*************************************************************************************
Input: 			
Output: 			Table in RTF format
Required programs:  
*************************************************************************************
Document history :
Version 	     Date 			Author 		Purpose
D01_0_0		15/OCT/2021		ZBA			First initiation
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

%let Input_folder     = C:\Users\zbai\Desktop\SAS;
%let Output_folder    = C:\Users\zbai\Desktop\MG-III-001 (NeraCare)\Output;
%let This_prog        = MG_III_001_T_2_1_MSS_Des_D01_0_0;

%let studyacronym	= Study-CodeName;
%let sponsor		= Sponsor Name;
%let report_version	= draft_v01;
%let report_author	= Zehui Bai; 
%let outputtype 	= Analysis; 
%let version		= 1.0;


*** Options;
option nocenter orientation=landscape; 
option linesize = 111 nodate nonumber;
option pagesize = 42;


*** Load macro library if exist;
%include "&Input_folder.\SASMacro\Macro_IncludeProgrammes_V02.sas";
%IncludeProgrammes(folderpath=&Input_folder.\SASMacro);




ods tagsets.rtf file="&Output_folder.\Table\&This_prog..rtf" style=gcpservice_style;

ods escapechar="^";	
option NOQUOTELENMAX ;
options nodate nonumber nocenter;
ods exclude none;

 
goptions reset=all xpixels=900 ypixels=750 device=jpeg ftext="Arial/bo" cback=vpag;
goptions border iback='&Input_folder.\SASBackgroud\gpc-service-logo.png' imagestyle=fit htitle=2.75 htext=1.3;
ods listing close;
ods html path="&Output_folder.\Table" body='tryit.htm' ;
Title h=2 font=Arial bold italic 'Linear Regression';

proc sgplot data=sashelp.class noautolegend;
reg y=weight x=height;
run;

quit;
ods html close;
ods listing;
