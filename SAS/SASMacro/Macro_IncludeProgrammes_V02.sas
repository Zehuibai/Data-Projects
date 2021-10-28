/*****************************************************************************************
Program Name:    Macro_IncludeProgrammes_V01.sas
Initial Date:    24_Feb-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Import all programmes available in a specific folder
Risk Assessment:   high
******************************************************************************************
Source of Data:    -
Input datasets:	   All programmes and macros in the specified folder
Output format:     - 
Output type:       -
Templates /macros: - 
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			24-Feb-2015 DN     	 First initiation
*/


*****************************************************************; 
***						Macro start;
*****************************************************************;
*** New macro variables:
	* "filename&i". Macro variable that contains the dataset name;
	* "check_m_&i". Flag indicating whether the dataset name starts wit m_;
	* "includetotal". Number of all datasets within a folder;

%macro IncludeProgrammes(folderpath);
	filename entry2 pipe "dir &folderpath /b";
	*** get a list of all programmes in the specified folder
		and declare a macro variable with the programme name for each programme ;
	data prog_list;
		infile entry2 truncover;
		length filename $40;
		input filename;
	run;

	proc sort data=prog_list;
		by filename;
	run;

	data prog_list;
		set prog_list end=eof;
		j+1;
		call symput('filename'||trim(left(put(j,8.))),scan(trim(filename),1,'.'));
		if eof then do;
			if substr("&folderpath", 1, length("&folderpath")) ne "\" then folderpath = "&folderpath.\";
			call symput("folderpath", strip(folderpath));
			call symput('includetotal',trim(left(put(j,8.))));
		end; 
	run;
	%put &folderpath;
	%put &includetotal;

	*** include all programmes in the specified folder;
	libname libdata "&folderpath";
	%do j=1 %to &includetotal;
	%put &j;
		%include "&folderpath.&&filename&j...sas";
	%end;
%mend IncludeProgrammes;


*****************************************************************; 
***						Example of use;
*****************************************************************;
*%IncludeProgrammes(folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\03_Macros\);
