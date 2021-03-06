/************************************************************************************
 Programme name:   		UVEA839_T8_5_1_1_CDVA_D01_0_0.sas
 Programme language: 	SAS 9.4
 Initial date: 			28/APR/2022
 Sponsor | study: 		Carl Zeiss Meditec AG | UVEA839
 Author(s): 		 	RPA
 *************************************************************************************
 Short description: 	Table 8.5.1.1	  - Secondary endpoints
 	 	 	 	 	 	 	 	 	 	 	Visual acuity
 	 	 	 	 	 	 	 	 	 	 	Postoperative CDVA
 Requirements: 	 		---
 Risk assessment: 		Low
 *************************************************************************************
 Input:	 				<TBD> dataset
 Output: 				Table in RTF document
 Required programmes: 	current version of master programme 'UVEA839_MasterProg_TFLs'
 *************************************************************************************
 Document history :
 Version 	Date 		Author 		Purpose
 D01_0_0 	28/APR/2022 RPA	 		First initiation
 *************************************************************************************/

 *************************************************************************************;
 **************************        General settings	    ******************************;
 *************************************************************************************;
 
 *** Change the output destination to SAS and empty the log as well as the output window;
 ODS LISTING;
 DM LOG 'CLEAR';
 DM OUTPUT 'CLEAR';
 ODS EXCLUDE ALL;
 
 *** Clear old titles and footnotes;
 TITLE;
 FOOTNOTE;
 
 *** Name of this program;
 %LET THIS_PROG= UVEA839_T8_5_1_1_CDVA_D01_0_0;
 %LET THIS_TOC_LABEL = Table 8.5.1.1;
 %LET DS_NAMES = UVEA839_T8_5_1_1;
 %LET CAPTION_LVL1 = Secondary endpoints;
 %LET CAPTION_LVL2 = Visual acuity;
 %LET CAPTION_LVL3 = Postoperative CDVA;

 *************************************************************************************;
 **************************        Programme start	    ******************************;
 *************************************************************************************;


 *************************************************************************************;
 **************************             Report  	      ******************************;
 *************************************************************************************;


 %IF &OUTPUT. EQ FALSE %THEN %DO;
 ODS TAGSETS.RTF FILE="C:/Users/zbai/Documents/GitHub/R-Projects/SAS_Mock_Templete_Generator/07_STATISTIC/04_Statistical_Analysis/01_Final/02_Output/01_Tables/&THIS_PROG..rtf"
 STYLE=gcpservice_style;
 %END;
 
 ODS ESCAPECHAR="^";
 OPTIONS NODATE NONUMBER NOCENTER;
 ODS EXCLUDE NONE;
 
 ODS PROCLABEL="&THIS_TOC_LABEL.: &CAPTION_LVL3.";
 TITLE1 j=l height=10pt font=courier "&THIS_TOC_LABEL:^R/RTF'\tab' &CAPTION_LVL1.";
 TITLE2 j=l height=10pt font=courier "^R/RTF'\tab\tab\tab' &CAPTION_LVL2.";
 TITLE3 j=l height=10pt font=courier "^R/RTF'\tab\tab\tab' &CAPTION_LVL3.";
 
 footnote1 j=l height=8pt font=courier "n: Number of non-missing observations; SD: Standard deviation; ";
 footnote2 j=l height=8pt font=courier "CFB: Change from baseline, calculated as (postoperative value ? preoperative value);";
 footnote3 j=l height=8pt font=courier "mITT: Modified Intent-to-treat population, following the modified intention-to-treat (mITT) principle, the mITT population will include all subjects who have received an investigational device (UVEA 839:AT LISA tri 839MP (UVE) or ELISAT: AT LISA tri 839MP (UV)) and at least one of the three co-primary endpoints is measured postoperatively; The subset of subjects from the UVEA 839 study of the mITT will be denoted as mITT839;";
 footnote5 j=l height=8pt font=courier "Output generated by program '&this_prog.';";
 
 ods listing close;
 
 PROC REPORT DATA=UVEA839_T8_5_1_1_D99 HEADLINE HEADSKIP NOWD SPLIT='|' MISSING CONTENTS='';
 COLUMNS (DUMMY (
 	EMPTY_COLUMN
 ));
 DEFINE DUMMY				/ 		ORDER 	NOPRINT;
 define empty_column		/ "" 		  	style(column)=[cellwidth=0.2 cm];
 
 
 compute before;
 line ' ';
 endcomp;
 
 compute after dummy;
 line ' ';
 endcomp;
 
 break before dummy / contents="" page;
 run; 
 
 footnote; title;
 
 %IF &OUTPUT. EQ FALSE %THEN %DO;
 ods tagsets.rtf close;
 ods _all_ close;

 %END;
 

*** Cleaning up;
PROC DATASETS NOLIST;
DELETE UVEA839_T8_5_1_1_D:;
QUIT;

*************************************************************************************;
**************************        Programme End	      ******************************;
*************************************************************************************;


