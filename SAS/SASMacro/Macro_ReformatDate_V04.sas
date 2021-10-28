*** Caution: format of date in raw data may have to be adapted in case of partial dates, if the
			 date format differs from usual date formats. Unfortunately, it is not possible to
			 take care of all possible date formats.

/*****************************************************************************************
Program Name:    Macro_ReformatDate_V04.sas
Initial Date:    09-Mar-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: This macro changes the format of date variables into the standard format
				   for dates at the statistical department of GCP-Service. The standard 
				   format is "DDMMMYYYY". 
				   Furthermore, partial dates are imputed as specified in the Statistical 
				   Analysis Plan and saved as m_ITEMNAME_imputed. 
				   That means the first day of the month is imputed if the day
				   is missing and the first month of the year if the month is missing.
Risk Assessment:   High
*******************************************************************************************
Source of data:   work-library
Input datasets:	  all datasets imported by macro %Import
Output format:    .sas7bdat
Output type:       SAS datasets with reformatted and imputed dates
Templates /macros: -
Required programs: %Import or other macros importing datasets such as %AssignFormats (to 
				   create dataset "m_dataset_list" with a list of all datasets with crf-data)
*******************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			11-Mar-2015 DN     	 First Release
02			26-Jun-2015	DN		 Error handling
03			30-Jul-2015	DN		 Macro does not do any imputation anymore, as this is not
								 necessary at the time point of analysis at which this 
								 macro is intended. Imputation will be a separat macro.
								 Macro parameter "date_format" has been added and allows 
								 a user defined format of the dates.
*/


*****************************************************************; 
***						Macro start;
*****************************************************************;
*** New macro parameter "inp_path". File path for the CRF itemlist;
*** New macro parameter "input". Name of the CRF itemlist;
*** New macro parameter "out_path". Output path for the SAS-datasets with the reformatted dates.
									If no output path is specified, the datasets remain in the work library;
*** New macro parameter "spec_char". Special character that indicates the beginning of the version number of the item (version number will be cut of)
									Please do not put the character in quotation marks.
*** New macro parameter "date_format". The intended format of the date variables.


*** New macro variable "fname&i". Macro variable that contains the dataset name;
*** New macro variable "check_m_&i". Flag indicating whether the dataset name starts wit m_;
*** New macro variable "total". Number of all datasets within a folder;
*** New macro variable "obs". Number of variables with partial dates in a dataset;
*** New macro variable "date_names". Variable with a list of all date variables (not partial dates!) within a dataset;

*** New variable "spec_char_pos". Position of the special char (spec_char) within the item_name;

*option spool;
%macro ReformatDate (inp_path, input, out_path, spec_char, date_format);
	*** import excel file with item list;
	libname inp_path "&inp_path";
	data _null_;
		length item_name $32.;
		infile "&inp_path.&input" firstobs=3;
		file "&inp_path.itemlist.dat";
		input;
		put _infile_;
	run;

	proc import datafile="&inp_path.itemlist.dat"
		replace 
		dbms=csv
		out=work.itemlist_excel;
		guessingrows=10000;
		getnames=yes;
		delimiter=';';
	run;

	data itemlist_excel;
		set itemlist_excel;
		spec_char=compress("&spec_char", "'");
		if compress(spec_char) ne "" then do;
			spec_char_pos = find(item_name,spec_char);
			item_name = substr(item_name, 1, spec_char_pos-1);
		end;
	run;
	proc sort data=itemlist_excel nodupkey;   
		by item_name;
	run;

	*** Declare macro variables containing the name of a dataset in the specified folder and their overall number.
		The list is automatically created by the macro %import.
		Check whether the dataset name starts with "m_" (see description of marco variable check_m_);
	data _null_;
		set m_dataset_list end=eof;
		/* edit length as needed */
		length fname $40;
		/* the parsing (splitting) of FNAME may need to be changed depending on your OS*/
		call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
		if eof then call symput('total',trim(left(put(i,8.))));
		if substr(fname, 1, 2) eq "m_" then check_m_ = "1";
		else if substr(fname, 1, 2) ne "m_" then check_m_ = "0";
		call symput('check_m_'||trim(left(put(i,8.))), trim(put(check_m_,2.))); 
	run;

	libname out_path "&out_path";
	*** create dataset with a list of all variables in the sas dataset that currently has to be modified (indicated by &i); 	
	%do i=1 %to &total;
		proc contents data=work.&&fname&i out=contents_&i noprint;
		run;

		proc sort data=contents_&i;
			by name;
		run;
	
	*** create list of all variables in the datasets that are dates or at least partial dates (list is created out of the excel file with the item list);
		data date_var;
			length item_name $32.;
			set itemlist_excel;
			if find(upcase(strip(data_type)),"DATE", 1) ne 0;	
		run;

		proc sort data=date_var;
			by item_name;
		run;

	*** create a macro variable that contains all variables with complete dates in the sas dataset that is currently processed (indicated by &i);
		%let date_names=;
		data _null_;
			length date_names $32767.;
			merge date_var (in=a) contents_&i(rename=(name=item_name) in=b) end=eof;
			retain date_names;
			by item_name;
			if a and b;
			if substr(upcase(strip(data_type)),1,4) eq "DATE";
			date_names = strip(date_names) || " " || strip(item_name);
			call symput("date_names", date_names);
		run;		
	
		%put &date_names;

		*** modify dataset name if the name of the dataset does not start with "m_" and 
			reformat dates partial dates are reformatted depending on the original format. Furthermore, the variable "m_&&partial_date_var&k.._imputed" contains 
			the imputed partial dates as observations;
		%if 	  &&check_m_&i eq 1 and &out_path eq %then data work.&&fname&i;
		%else %if &&check_m_&i eq 0 and &out_path eq %then data work.m_&&fname&i;
		%else %if &&check_m_&i eq 1 and &out_path ne %then data out_path.&&fname&i;
		%else %if &&check_m_&i eq 0 and &out_path ne %then data out_path.m_&&fname&i;;
			set work.&&fname&i;
		%if &date_names ne  %then format &date_names &date_format;;
		run; 
	%end;

	*** delete all datasets that are not needed anymore;
	 proc datasets library=work nolist;
	 	delete itemlist_excel date_var contents_1-contents_&total ;
	 run;
	 quit;
%mend ReformatDate;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%include "Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\03_Macros\Macro_Import_V01.sas";
*%Import (folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\,
		  delete_all=);
*%ReformatDate (inp_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\04_Excelfiles\,
			    input=ITEMLIST_data_20150130.csv,
			    out_path=,
				spec_char=,
				date_format=date9.);
