***Attention: if the domain name is longer than 5 characters, it is not recognized by 
			  the programme. In case of domain names longer 5 the limit has to be augmented.;

/*****************************************************************************************
Program name:    Macro_StandardiseVarNameFolder_V02.sas
Initial date:    25-Feb-2015
Author:          Dennis Neuschwander
Shorthand symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short fescription: Standardisation of variables names across different crfs. All datasets
				   within a folder are standardised (Removal of domain name and version number
				   of item). Special character has to be specified.
Risk Assessment:   High
******************************************************************************************
Source of data:    -
Input datasets:	   -
Output format:     -
Output type:       -
Templates /macros: -
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			31-07-2015  DN       Rename of StandardiseVarNameFolder
*/

*** New macro variable "folder_path". File path with the unformatted raw data;
*** New macro variable "out_path". File path for the formatted datasets;
*** New macro variable "spec_char". Special character proceeding the version number. Please do not put the character in quotation marks.
*** New macro variable "option". If option equals 1, both the domain name (the part of the item name prior to the first "_") 
									and the number after the special character is deleted.
								 If option equals 2, only the domain name is deleted
								 If option equals 3, only the number after the special character is deleted;
*** New macro variable "fname&i". Macrovariable that contains the dataset name;
*** New macro variable "check_m_&i". Flag indicating whether the dataset name starts wit m_;
*** New macro variable "rename_list". List with all variable names and their corresponding new name;
*** New macro variable "total". Number of all datasets within a folder;

*** New variable "name". Old name of the variable in the dataset
*** New variable "new_name". New name of the variable in the dataset;
*** New variable "first_". Position of the  first underscore ("_");
*** New variable "spec_char_pos". Position of the special character(e.g."$");

dm log 'clear';
%macro ModifyItemname (folder_path, out_path, option, spec_char);
*** make list of all variables in the specified folder;
			
		
	%if &folder_path ne %then %do;
		filename entry2 pipe "dir &folder_path /b";
		data m_dataset_list;
			infile entry2 truncover end=last;
			length fname $40.;
			retain i 0;
			input fname;
			i=i+1;
			if substr(fname, 1, 2) eq "m_" then check_m_ = "1";
			else if substr(fname, 1, 2) ne "m_" then check_m_ = "0";
			call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
			call symput('check_m_'||trim(left(put(i,8.))), trim(put(check_m_, 2.)));
			if last then call symput('total',trim(left(put(i,8.))));
		run;
	%end;

	%else %if &folder_path eq %then %do;
		data m_dataset_list;
			set m_dataset_list end=last;
			if compress(check_m_) eq "0" and substr(compress(upcase(fname)),1,1) ne "I" then do;
				fname 	 = "m_" || strip(fname);
				check_m_ = "1";
			end;
			call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
			call symput('check_m_'||trim(left(put(i,8.))), trim(put(check_m_,2.)));
			if last then call symput('total',trim(left(put(i,8.))));
		run;
	%end;


	*** determine libnames/libraries;
		libname libdata "&folder_path";
		libname library (libdata); 
		libname out_path "&out_path";

	%do i=1 %to &total;
	*** get a list of all names of variables in a dataset 
		and declare a macro variable with the dataset name for each dataset.;
		proc contents data=%if &folder_path ne %then libdata.&&fname&i; %else %if &folder_path eq %then work.&&fname&i; out=contents_&i noprint;
		run;
		
		data stand_var_names_&i;
			set contents_&i;
			first_ 	      = find(name, "_");
			if first_ eq 1 or first_ > 5 then delete;
			if &option in (1, 3) and (substr(name, length(strip(name))-2,1) ne "_" or substr(name, length(strip(name)),1) not in ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") or substr(name, length(strip(name))-1,1) not in ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")) then delete; 
			spec_char_pos = find(name, "&spec_char", -(length(name)));
			if &option eq 1 then new_name = substr(name, first_+1, spec_char_pos-1); 
			else if &option eq 2 then new_name = substr(name, first_+1, length(name)-first_+1);
			else if &option eq 3 then new_name = substr(name, 1, spec_char_pos-1);
		run;

		proc sort data=stand_var_names_&i;
			by name;
		run;

		proc sort data=contents_&i;
			by name;
		run;	

*** declare a macro variable that not only contains the variable names that are to be renamed
	but also the new variable names;;
		data _null_;
			length rename_list $32767.;
			merge stand_var_names_&i (in=a) contents_&i (in=b);
			retain rename_list;
			by name;
			if a and b;	
			if rename_list eq "" then rename_list = strip(name) || "=" || strip(new_name) ||" " ;
			else if rename_list ne "" then rename_list = strip(rename_list) || " " || strip(name) ||"="|| strip(new_name);
			if last.name then call symput("rename_list", rename_list); 
		run; 


		%put &rename_list;
		%put &&check_m_&i;

*** rename variables in dataset;
		%if &&check_m_&i eq 1 and &out_path eq %then data work.&&fname&i;
		%else %if &&check_m_&i eq 0 and &out_path eq %then data work.m_&&fname&i;
		%else %if &&check_m_&i eq 1 and &out_path ne %then data out_path.&&fname&i;
		%else %if &&check_m_&i eq 0 and &out_path ne %then data out_path.m_&&fname&i;;
			%if &folder_path ne %then set libdata.&&fname&i;
			%else %if &folder_path eq %then set work.&&fname&i;;
			rename &rename_list;
		run; 
		%let rename_list = ;
		%put &rename_list;
	 %end;

	 proc datasets library=work nolist;
	 	delete stand_var_names_1-stand_var_names_&total
			   contents_1-contents_&total;
	 run;
	 quit;
%mend ModifyItemname;


*****************************************************************; 
***						Example of use;
*****************************************************************;
*** The dataset "test" in the folder "01A_Data" has a variable "MHG_GLAUC_TYPE_01". The macro only modifies this variable by renaming it to "MHG_GLAUC_TYPE";
*%ModifyItemname(folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01A_Data\,
			  		 	  out_path=,
					 	  spec_char=_,
						  option=3);
