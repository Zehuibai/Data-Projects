/*
******************************************************************************************
Program Name:    Macro_DataLock_V02.sas
Initial Date:    24-Feb-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Saves all datasets within a folder as write-protected datasets with 
				   an user-defined password saved in an external txt.file
Risk Assessment:   High
******************************************************************************************
Source of data:    Closed database by our DM
Input datasets:	   SAS datasets (all datasets within a folder)
Output format:     .sas7bdat
Output type:       SAS datasets (write-protected)
Templates /macros: - 
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			24-Feb-2015 DN		 First initiation
02			26-Feb-2015 DN		 Macro now SOP conform
*/

*****************************************************************; 
***						Macro start;
*****************************************************************;

*** Count number of observations with a specific characteristic;  
*** New macro variable "folder_path". File path with all datasets that are to be write protected;
*** New macro variable "out_path". File path for the write protected datasets;
*** New macro variable "pass_path". File path for the  txt-file with the password;
*** New macro variable "password". Password.;
*** New macro variable "fname&i". Macro variable that contains the dataset name;
*** New macro variable "check_m_&i". Flag indicating whether the dataset name starts wit m_;
*** New macro variable "total". Number of all datasets within a folder;

%macro DataLock(folder_path, out_path, pass_path, password_write, password_alter);
*** Create macro variables, where each macro variable contains the name of a dataset in the folder.
	Check whether the datasets has been already modified by a macro. Indicator for a modified dataset 
	is "m_" at the beginning of the dataset name;
	filename entry2 pipe "dir &folder_path /b";
	data _null_;
		infile entry2 truncover end=last;
		/* Edit length as needed */
		length fname $40;
		input fname;
		i+1;
		if substr(fname, 1, 2) eq "m_" then check_m_ = 1;
		else if substr(fname, 1, 2) ne "m_" then check_m_ = 0;
		/* the parsing (splitting) of FNAME may need to be changed depending on your OS*/
		call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
		call symput('check_m_'||trim(left(put(i,8.))), trim(put(check_m_,2.)));
		if last then call symput('total',trim(left(put(i,8.))));
	run;
	
	libname libdata "&folder_path";
	libname library (libdata); 
	libname out_path "&out_path";
*** write protect each dataset;
	%do i=1 %to &total;
		%if 	  &&check_m_&i eq 1 %then data out_path.&&fname&i;
		%else %if &&check_m_&i eq 0 %then data out_path.m_&&fname&i;
			(write=&password_write alter=&password_alter); 
			 set libdata.&&fname&i; 
		run;
	%end;
*** save password;
	data _null_;
		file  "&pass_path.passwords.txt";
		put   "write: &password_write";
		put   "alter: &password_alter";
	run;
%mend DataLock;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%let lib_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\05_output\Passwords;
*%DataLock(folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\, 	
		   out_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\05_output\Locked_data\, 
		   pass_path=&lib_path,
		   password_write=test,
		   password_alter=test);
