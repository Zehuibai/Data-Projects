/*****************************************************************************************
Program Name:    Macro_ImportCsvTxt_V01.sas
Initial Date:    09-Mar-2015
Author:          Joanna Wengrzik/Dennis Neuschwander
Shorthand Symbol:JW/DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Import all datasets available in a specific folder, if they are 
				   consistent in  format
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
%macro ImportCsvTxt (folder_path, delete_all, delimiter, type);
	*** delete all datasets if delete_all is 1;
	%if &delete_all ne 1 %then %do;
		proc datasets library=work kill nolist; run; quit;
		dm log 'clear';
	%end;

	data _null_;
		length type dbms dlm $10.;
		if upcase(strip("&type")) eq "CSV" then do;
			type="&type";
			dbms="csv";
			dlm="&delimiter";
		end;	
		else if upcase(strip("&type")) eq "TXT" then do;
			type="&type";
			dbms="dlm";
			dlm="&delimiter";
		end;	
		else do;
			type="&type";
			dbms="&type";
			dlm="&delimiter";
		end;		
		call symput ("type", compress(type));
		call symput ("dbms", compress(dbms));
		call symput ("dlm", compress(dlm));
	run;

	*** definition of folder with the datasets;
	filename entry2 pipe "dir &folder_path /b";

	*** declare macro variables containing the name of a dataset in the specified folder and their overall number; 
	data m_dataset_list;
		infile entry2 truncover end=last;
		/* edit length as needed */
		length fname sas_conform_name $40;
		input fname;
		i+1;
		sas_conform_name = tranwrd(strip(fname), "-", "_");
		/* the parsing (splitting) of FNAME may need to be changed depending on your OS*/
		call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
		call symput('sas_conform_name'||trim(left(put(i,8.))),scan(trim(sas_conform_name),1,'.'));
		if last then call symput('total',trim(left(put(i,8.))));
	run;

	*** import by means of the macro variables defined above ; 
	libname libdata "&folder_path";
	libname library (libdata); 	
	%do i=1 %to &total;
		proc import datafile="&folder_path.&&fname&i...csv"
			replace 
			dbms=&dbms
			out=work.i_&&sas_conform_name&i;;
			guessingrows=10000;
			getnames=yes;
			delimiter="&dlm";
		run;
	%end;
%mend ImportCsvTxt;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%Import (folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\,
		 delete_all=);
