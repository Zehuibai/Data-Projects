*** To do:	6) Sonderzeichen --> Was damit tun? (gelöst, wenn nicht noch unvorhergesehenes passiert)
			7) CHECKBOX ABFRAGE SPÄTER NICHT MEHR NÖTIG, WEGEN MACRO VON THOMAS
;
/*
******************************************************************************************
Program Name:    Macro_AssignFormats_V05.sas
Initial Date:    23-Feb-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Creates a format catalog and a password protected dataset with all 
				   formats. The formats are derived from the itemlist of the actual study 
				   and are automatically assigned to the datasets containing the data of 
				   the study. The format name corresponds  to the item name (item_name) 
				   without the version number of the item. 
Risk Assessment:   High
******************************************************************************************
Source of data:    Itemlist and row data sets
Input datasets:	   All SAS datasets with raw data of the study, itemlistxxx.csv
Output format:     .sas, .sas7bcat, .sas7bdat
Output type:       SAS programmes, SAS catalogs, formatted SAS datasets
Templates /macros: - 
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    	Author	 Purpose
01			24-Feb-2015 	DN		 First initiation
02			24-Feb-2015 	DN		 Commentation
03			26-Feb-2015 	DN		 Programm now SOP conform;
04			16/17-Mar-2015 	DN		 Changes due to validation;
05			02-Apr-2015		DN		 Changes due to validation (continued);
06			25-Jun-2015		DN		 Macro now cuts of the version number of the items 
									 before creation of the format library if the
									 macro parameter "special_char" is specified.
07          31-07-2015      JW       firstobs=3; changes required due to croms update. 2nd line of itemlist is blank
*/



*****************************************************************; 
***						Macro start;
*****************************************************************;

*** Count number of observations with a specific characteristic;  
*** New macro parameter "inp_path". File path with the input-file (itemlistxxx.csv);
*** New macro parameter "input". Input-file (itemlistxxx.csv);
*** New macro parameter "spec_char". Special character that indicates the beginning of the version number of the item (version number will be cut of)
									Please do not put the character in quotation marks.
*** New macro parameter "lib_path". File path for the sas catalog);
*** New macro parameter "library_name". Name of the format catalog;
*** New macro parameter "output". Name of the sas programme with formats (saved in lib_path);
*** New macro parameter "folder_path". File path with the unformatted raw data;
*** New macro parameter "out_path". File path for the formatted datasets (sas programme with proc format)
								   If out_path is empty, the datasets are transferred to the work library;

*** New macro variable "fname&i". Macro variable that contains the dataset name;
*** New macro variable "check_m_&i". Flag indicating whether the dataset name starts wit m_;
*** New macro variable "total". Number of all datasets within a folder;
*** New macro variable "item_name&k". Item_name (see item list) of a variable whose data orgiginate from a checkbox or a multi-select field;
*** New macro variable "num_values&k". Number of different characteristics of the corresponding entry of the macro variable "item_name&k".
*** New macro variable "num_box_multis". Number of variables whose data orginiate from a checkbox or a multi-select field
*** New macro variable "num_combis". Number of combinations for the characteristics of the current entry of macro variable "item_name&k";

*** New variable "num_commas_value". Number of commas in response values (variable "response_values_or_calculations" in itemlist);
*** New variable "num_commas_format". Number of commas in response options (variable "response_options_text" in itemlist);
*** New variable "format_name". Name of the variable for which a format has to be created from itemlistxxx.csv;
*** New variable "value". Value of format_name that has to be formatted;
*** New variable "format_value". Format that is assigned to a specific value of format_name;
*** New variable "name". List with all names of all variables within a dataset with unformatted raw data;
*** New variable "format_names". List with names of all variables that have to be formatted within a dataset with unformatted raw data and the corresponding format_name;
*** New variable "digits". Indicates the current digit of a combination for the characteristics of the current entry of macro variable "item_name&k"
*** New variable "all_num_combis".  Corresponds to the variable "response_values_or_calculations" in the item list;
*** New variable "all_format_combis". Correspond to the variable "response_options_text" in the item list;
*** New variable "format_actual_value". Indicates the current format value of a combination for the characteristics of the current entry of macro variable "item_name&k";
*** New variable "all_format_values. Indicates the format values of all combinations for the characteristics of the current entry of macro variable "item_name&k";
*** New variable "spec_char_pos". Position of the special char (spec_char) within the item_name;


%macro AssignFormats (inp_path, input, spec_char, lib_path,  library_name, output, folder_path, out_path);
	data _null_;
		if upcase(substr("&library_name", 1, 2)) eq "M_" then do;
			library_name = substr("&library_name", 3, length("&library_name")-2);
			call symput ( "library_name", compress(library_name));
		end;
		if upcase(substr("&output", 1, 2)) eq "M_" then do;
			output = substr("&output", 3, length("&output")-2);
			call symput ( "output", compress(output));
		end;
	run;

	%put &output;
	%put &library_name;

	*filename formats;
	
	*** Import excel file;
	*** Before proc import, the first line in the itemlist has to be deleted in the file that has to be imported as proc import uses the first line of the itemlist to get 
		the variable names;
	libname inp_path "&inp_path";
	libname lib_path "&lib_path";
	data _null_;
		infile "&inp_path.&input" firstobs=3;
		file "&inp_path.formats.dat";
		input;
		put _infile_;
	run;

	proc import datafile="&inp_path.formats.dat"
		replace 
		dbms=csv
		out=work.formats_excel;
		guessingrows=10000;
		getnames=yes;
		delimiter=';';
	run;

	data formats_excel;
		set formats_excel;
		spec_char=compress("&spec_char", "'");
		if compress(spec_char) ne "" then do;
			spec_char_pos = find(item_name, "&spec_char", -(length(compress(item_name))));
			item_name = substr(item_name, 1, spec_char_pos-1);
		end;
	run;

	proc sort data=formats_excel nodupkey;   
		by item_name;
	run;

	
	*** Count values of each variable that is to be formatted, 
		check if there are discrepencies between the number of commas of the values and formatted values;
	data formats_excel;
		length format_name $32.;
		set formats_excel (keep=item_name 
								response_type
								response_label 
							    response_options_text	
								response_values_or_calculations  
								data_type);
		num_commas_value=countc(response_values_or_calculations,',');
		num_commas_format=countc(response_options_text,',');
		if compress(response_values_or_calculations) in ("text" "file") then delete;
		else if compress(response_values_or_calculations) eq compress(response_options_text) then delete;
		if num_commas_value ne num_commas_format then do;
			put "ERROR: PROGRAMME ABORT AS THE NUMBER OF COMMAS DIFFERS BETWEEN THE OBSERVATIONS OF VARIABLE 'RESPONSE_VALUES_OR_CALCULATIONS' AND 'RESPONSE_OPTION_TEXT' (USER-DEFINED ERROR)";
			put	"WARNING: THERE MIGHT BE AN ERROR WITH THE IMPORT OF THE ITEMLIST";
			put;
			put;
			abort 255;
		end;
		format_name = item_name;
	run;
	
	*** select all entries that originate from check boxes or multi-select fields in the dataset "formats_excel" and define 
		for each variable name a macro variable (&&item_name&l). Furthermore, define a macro variable with the number of those variable (&num_box_multis)
		and the number of different characteristics of each of this variables (&&num_values&l). The selection is necessary since checkboxes and multi-select fields
		allow the choice of multiple answers for one question;
	data _null_;
		set formats_excel;
		retain l 0;
		if compress(upcase(data_type)) eq "INTEGER" and compress(upcase(response_type)) in ("CHECKBOX" "MULTI-SELECT") then	l=l+1;
			num_values = compress(put(num_commas_value + 1,2.));
			call symput("item_name"||trim(left(put(l,8.))),scan(trim(item_name),1,'.'));
			call symput("num_values"||trim(left(put(l,8.))),scan(trim(num_values),1,'.'));
			call symput("num_box_multis",trim(left(put(l,8.))));
	run;

	*** define macro variable "num_combis" with the number of all combinations (ordered, no repitions, up to &&num_values&k digits) of the different characteristics 
		of the actual variable &&&num_values&k. Create all those combinations by proc plan and save them in the dataset combis_var_&k. Since each digit (e.g. 1) has its own 
		variable, the combinations have to be assembled and saved in the variable "combis" (e.g 1,2,4). Afterwards, the variable "all_format_values" is defined. It contains
		formatted value of each digit of the current observation of the variable "combis". The entries of "combis" for each variable ("&&item_name&k") and the corresponding 
		entries of the variable "all_format_values" are then gathered in the dataset "formats_box_multis";
	%do k = 1 %to &num_box_multis;
		%do m=1 %to	&&num_values&k;
			data _null_;
				num_combis = compress(put(comb(&&num_values&k, &m),4.)); 
				call symput ("num_combis", num_combis);
			run;

			proc plan;
				factors block=&num_combis ordered
						value=&m of &&num_values&k comb;
				ods output plan=combis_var_&k;
			run; 

			data combis_var_&k;
				length 	format_name $32.
						combis $32767.;
				set combis_var_&k;
				retain combis; 
				combis = "";
				array values{*}  value1--value&m;
				num_commas_value = &&num_values&k - 1;	
				format_name = "&&item_name&k";
				%put &m;
				do digit = 1 to &m;
					if digit eq 1 then combis = compress(vvalue(value1));
					else if digit gt 1 then combis = strip(combis) ||", " || compress(vvalue(values[digit]));
				end;
			run;
			
			proc sort data=combis_var_&k;
				by  format_name;
			run;

			data combis_var_&k;
				length all_format_values $32767.;
				merge combis_var_&k (in=a) formats_excel (in=b);
				by format_name;
				if a and b;
				retain all_format_values ;
				do v = 1 to digit-1;
					format_actual_value= scan(response_options_text,input(scan(combis, v, ","), 2.),",");
					if v eq 1 then all_format_values  = strip(format_actual_value);
					else if v gt 1 then all_format_values  = strip(all_format_values) ||", " || strip(format_actual_value);
				end;
			run;

			%if %sysfunc(exist(work.formats_box_multis)) %then %do;
				data work.formats_box_multis;
					set formats_box_multis combis_var_&k;
				run;
			%end;
			%else %do;
				data formats_box_multis;
					set combis_var_&k;
				run;
			%end;
		%let num_combis="";
		%end;

		proc sort data= formats_box_multis;
			by format_name;
		run;

	*** the variables "all_num_combis" and "all_format_combis" are generated in the dataset "formats_box_multis". They correspond to the variables 
		"response_values_or_calculations" and "response_options_text" in the dataset "formats_excel" respectively;
		data formats_box_multis;
			length all_format_combis all_num_combis $32767.;
			set formats_box_multis (drop=num_commas_value item_name);
			retain all_format_combis all_num_combis;
			retain num_commas_value 0;
			by format_name;
			if first.format_name then do;
				all_format_combis = strip(all_format_values );
				all_num_combis = strip(combis);
				num_commas_value = 0;
			end;
			else do; 
				all_format_combis = strip(all_format_combis) || "; " || strip(all_format_values);
				all_num_combis = strip(all_num_combis) || "; " || strip(combis);
				num_commas_value = num_commas_value + 1;
			end;
			if last.format_name then output;
		run;
	%end;
	
	

	*** delete entries for the variable "item_name" in the dataset "formats_excel" if it is also existing in the dataset "formats_box_multis"; 
	data formats_excel; 
		set formats_excel;
		if compress(upcase(data_type)) eq "INTEGER" and compress(upcase(response_type)) in ("CHECKBOX" "MULTI-SELECT") then delete;
	run;
	
	*** combine the datasets "formats_excel" and "formats_box_multis";
	data formats_excel;
		set formats_excel
			%if %sysfunc(exist(work.formats_box_multis)) %then formats_box_multis;;
	run;

	proc sort data= formats_excel;
		by format_name;
	run;
%put &num_box_multis;
	*** Declare variables (format_name, format_value, value) with the needed information 
		and write SAS programme that contains proc format and the formats. Formats for character variables start with a "$";
	data _null_;
		length format_value $32767.;
		set formats_excel end=eof;
		file  "&lib_path.m_&output..sas" ;
		if _n_=1 then put "proc format library=lib_path.m_&library_name noreplace cntlout=lib_path.m_&output._cntlout;";
		do i=1 to (num_commas_value+1);
			%if &num_box_multis ne 0 %then %do;
				if compress(upcase(data_type)) eq "INTEGER" and compress(upcase(response_type)) in ("CHECKBOX" "MULTI-SELECT") then do;
					format_value=strip(scan(all_format_combis,i,";")) || '"';
					format_value=compress(format_value, "â€“Â",);
					format_value=tranwrd(format_value, "  "," ");
						format_name='$'|| strip(format_name) ||'_F';
					if i eq 1 and  i eq (num_commas_value+1)then put '     value '  format_name   @44'"' value '="'   format_value  +(-1) ';'; 
					else if i eq (num_commas_value+1) then put @44'"'  value   '="'   format_value +(-1) ';';
					else if 1 lt i<min(num_commas_value+1) then put @44 '"' value   '="'   format_value; 
					else if i eq 1 then put '     value '   format_name   @44 '"'  value   '="'   format_value   ;
				end;
			%end;

			if compress(upcase(data_type)) ne 'CHARACTERSTRING' and compress(upcase(response_type)) ne 'CHECKBOX' then do;
				format_value=strip(scan(response_options_text,i,","))||'"';
				format_value=compress(format_value, "â€“Â",);
				format_value=tranwrd(format_value, "  "," ");
				value=compress(scan(response_values_or_calculations,i,","));
				format_name=strip(format_name) ||'_F';
				if i eq 1 and  i eq (num_commas_value+1) then put '     value ' format_name  @45 value '="'   format_value +(-1)  ';'; 
				else if i eq (num_commas_value+1) then put @45   value   '="'   format_value +(-1)  ';';
				else if 1 lt i<min(num_commas_value+1) then put @45  value   '="'   format_value; 
				else if i eq 1 then put '     value '  format_name    @45  value   '="'   format_value   ;
			end;
			if compress(upcase(data_type)) eq 'CHARACTERSTRING' then do;
				format_value=strip(scan(response_options_text,i,","))||'"';
				value=compress(scan(response_values_or_calculations,i,","))||'"';
				format_name='$'||strip(format_name) ||'_F';
				if i eq 1 and  i eq (num_commas_value+1)then put '     value '  format_name   @44'"' value '="'   format_value  +(-1)  ';'; 
				else if i eq (num_commas_value+1) then put @44'"'  value   '="'   format_value  +(-1) ';';
				else if 1 lt i<min(num_commas_value+1) then put @44 '"' value   '="'   format_value; 
				else if i eq 1 then put '     value '   format_name   @44 '"'  value   '="'   format_value   ;
			end;
		end;
		if eof then put 'run;';
	run;

	*** Execute SAS programme with the formats;
	%include "&lib_path.m_&output..sas";

	*** Make a list of all datasets in the specified folder and declare a macro variables for each dataset name;
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
		call symput('check_m_'||trim(left(put(i,8.))), trim(check_m_));
		if last then call symput('total',trim(left(put(i,8.))));
	run;

	*** Get a list of all names of variables in an unformatted raw dataset 
		and declare a macro variable with the dataset name for each dataset.;
		libname libdata "&folder_path";
		libname library (libdata); 
		libname out_path "&out_path";
	%do i=1 %to &total;
		%put &&check_m_&i;
		
		proc contents data=libdata.&&fname&i out=contents_&i noprint;
		run;
	
		data  contents_&i;
			set contents_&i;
			keep name;
		run; 

		data formats_excel;
			set formats_excel;
			keep data_type response_type format_name;
		run;

		proc sort data=formats_excel;
			by format_name;
		run;

		proc sort data=contents_&i;
			by name;
		run;	

	*** Declare a macro variable that not only contains the variable names that are to be formatted 
		but also the names of the corresponding format;
		data aaa;
			length format_names $32767.;
			merge formats_excel (in=a) contents_&i(rename=(name=format_name) in=b) end=eof;
			retain format_names;
			by format_name;
			if a and b;
			if compress(upcase(data_type)) ne 'CHARACTERSTRING' and compress(upcase(response_type)) ne 'CHECKBOX' then
				format_names= strip(format_names) || " " || strip(format_name) ||" "|| strip(format_name) || "_F." ;
			if compress(upcase(data_type)) eq 'CHARACTERSTRING' or compress(upcase(response_type))eq 'CHECKBOX' then
				format_names= strip(format_names) || " " || strip(format_name) ||" $"|| strip(format_name) || "_F." ;
			call symput("format_names", format_names); 
		run;
		libname library (lib_path); 
		options fmtsearch = (lib_path lib_path.m_&library_name work);
		%put &format_names;
	*** Format dataset;
		%if &&check_m_&i eq 1 and &out_path eq %then data work.&&fname&i;
		%else %if &&check_m_&i eq 0 and &out_path eq %then data work.m_&&fname&i;
		%else %if &&check_m_&i eq 1 and &out_path ne %then data out_path.&&fname&i;
		%else %if &&check_m_&i eq 0 and &out_path ne %then data out_path.m_&&fname&i;;
			set libdata.&&fname&i;
			format &format_names;
		run; 
	 %end;

	*** Delete all datasets that are not needed anymore;
	 proc datasets library=work nolist;
	 	delete  formats_excel 
				contents_1-contents_&total
				combis_var_1-combis_var_&num_box_multis
				formats_box_multis;
	 run;
	 quit;

%mend AssignFormats;

*****************************************************************; 
***						Example of use 1)
*****************************************************************;	

*%let lib_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\05_Output\;
*%AssignFormats(inp_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\04_Excelfiles\,
			   input=ITEMLIST_data_20150130.csv, 
			   spec_char=,
			   lib_path=&lib_path,
			   library_name=m_example_library,
			   output=m_formats, 
			   folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\,
			   out_path=);


*****************************************************************; 
***						Example of use 2) Abort  
*****************************************************************;	
*%let lib_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\05_Output\;
*%AssignFormats(inp_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\04_Excelfiles\,
			   input=ITEMLIST_Error_Formats.csv, 
			   spec_char=,
			   lib_path=&lib_path,
			   library_name=m_example_library,
			   output=m_formats, 
			   folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\,
			   out_path=);

