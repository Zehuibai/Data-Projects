
/*****************************************************************************************
Program Name:    Macro_CountObs_DN.sas
Initial Date:    27-Jan-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  ARGOS-02
SAS-Version:     9.4
******************************************************************************************
Short Description: Count number of observations in a dataset. Macro variable Condition
				   allows to count observations for a certain subset
Risk Assessment:   High
******************************************************************************************
Source of data:    Closed database by our DM
Input datasets:	   Dataset for which the number of observation has to be counted
Output format:     -
Output type:       Macro variable (the name of the macro variable always contains the
				   indicator "m_" for variables that are obtained by a macro.
Templates /macros: - 
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			28-Jan-2015  DN      First Release
02			26-Feb-2015	 DN		 Macro now SOP conform
03			02-Apr-2015  DN		 Changes due to validation;
04			18-May-2015	 DN		 Problem with "condition" solved;

*/


*****************************************************************; 
***						Macro start;
*****************************************************************;

*** New macro variable "input". Input dataset;
*** New macro variable "name". Name ;
*** New macro variable "counts". Number of observations;
*** New macro variable "condition". WHERE-statement to analyse only a subpopulation;
%macro CountObs(input, name, no_rep, condition);
	data _null_;
		if compress("&condition") ne "" then do;
			condition="where " || "&condition";
			call symput ("condition", strip(condition));
		end;
	run;
	
	data input;
		set &input;
	run;

	%if &no_rep ne %then %do;
		proc sort data=input nodupkey;
			by &no_rep;
		run;
	%end;;

	*** Count number of observations;
	proc sql noprint;
		select count(*)
		into :counts
		from input
		&condition;
	quit;

	data _null_;
		if upcase(substr("&name", 1, 2)) eq "M_" then do;
			name = substr("&name", 3, length("&name")-2);
			call symput ( "name", compress(name));
		end;
	run;

	proc sql noprint;
	quit;
		
*** Declare global macro variable with the number of observations;
	%global m_&name;

	data _null_;
		counts=&counts;
		count_c = put(counts, 6.);
		call symput("m_&name", compress(count_c));		
	run;

	
*** Delete all datasets that are not needed anymore;
	proc datasets library=work nolist;
	 	delete  input;
	run;
	quit;
%mend CountObs;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%CountObs (input="Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\Q_01_screening_ungrouped",
		   name=m_num_subj, 
		   no_rep,
		   condition=pre_sex2 eq 1);
