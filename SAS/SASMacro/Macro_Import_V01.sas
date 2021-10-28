/*****************************************************************************************
Program Name:    Macro_Import_V01.sas
Initial Date:    09-Mar-2015
Author:          Joanna Wengrzik/Dennis Neuschwander
Shorthand Symbol:JW/DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Import all SAS datasets available in a specific folder
Risk Assessment:   High
******************************************************************************************
Source of data:   -
Input datasets:	  -
Output format:     .sas7bdat
Output type:       SAS datasets (in the work-library)
Templates /macros: -
Required programs: -
*****************************************************************************************
Document History:
Version 	Date 	    Author	 Purpose
01			09-Mar-2015 DN     	 First Release
*/


*****************************************************************; 
***						Macro start;
*****************************************************************;

*** new macro variable "folder_path". File path with the data that are to be imported;
*** new macro variable "delete_all". If delete_all is unequal 1 then all datasets  in the work-library
									 are deleted, else if it is 1 no datasets are deleted;
*** new macro variable "fname&i". Macro variable that contains the dataset name;
*** new macro variable "total". Number of all datasets within a folder;
%macro Import (folder_path, delete_all);
	*** delete all datasets if delete_all is 1;
	%if &delete_all ne 1 %then %do;
		proc datasets library=work kill nolist; run; quit;
		dm log 'clear';
	%end;

	*** definition of folder with the datasets;
	filename entry2 pipe "dir &folder_path /b";

	*** declare macro variables containing the name of a dataset in the specified folder and their overall number; 
	data m_dataset_list;
		infile entry2 truncover end=last;
		/* edit length as needed */
		length fname $40;
		input fname;
		i+1;
		if upcase(substr(fname, 1, 2)) eq "M_" then check_m_ = 1;
		else check_m_ = 0;
		/* the parsing (splitting) of FNAME may need to be changed depending on your OS*/
		call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
		if last then call symput('total',trim(left(put(i,8.))));
	run;

	*** import by means of the macro variables defined above ; 
	libname libdata "&folder_path";
	libname library (libdata); 	
	%do i=1 %to &total;
		data work.&&fname&i; set libdata.&&fname&i; 
		run;
	%end;
%mend Import;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%Import (folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\,
		 delete_all=);
