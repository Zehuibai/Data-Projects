/*****************************************************************************************
Program Name:    Macro_ReformatFrequencies_V08.sas
Initial Date:    20-Jan-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Modifies the output of the macro %Frequency (Macro_Frequency_V02.sas). 
				   The characteristics of the analysed variable are transposed from rows
				   into new columns. Furthermore, the frequencies are sorted by descending
				   value. Provided the output is compatible, the macro can reformat also
				   frequencies that are not generated from the macro %Frequency. Eventually
				   combatibilty can be achieved by renaming the variables (e.g. _freq_ has to
				   be present, but in some output it is called count).
Risk Assessment:   High
******************************************************************************************
Source of data:    -
Input datasets:	   SAS dataset containing the frequencies
Output format:     .sas7bdat 
Output type:       SAS dataset with the rearranged formatted values
Templates /macros: - 
Required programs: usually %Frequency or macros with comparable output.
*****************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			20-Jan-2015 DN       First initiation
01			21-Jan-2015 DN		 Continued
01			22-Jan-2015 DN		 Continued
01			23-Jan-2015	DN		 Continued
01			26-Jan-2015 DN		 Continued
02			13-Feb-2015	DN		 Macro can now also be used for character variable 
								 "Var_Horizontal"
03			15-Feb-2015 DN		 Adaption necessary after change from 13-Feb-2015 
04			03-Mar-2015 DN		 Macro now SOP conform
05			19-Mar-2015 DN 		 Compatibility with output of macros %Frequency and %SumStatistics 
06			24-Apr-2015 DN		 Variable "level" is now calculated intern, less conspicious notes
07			28-Apr-2015 DN		 Continuation of error handling of version 6. Marco is now
								 ready for validation.
08			06-Aug-2015 DN		 The first entry of the dataset contained frequencies, although it
								 should be just a description of the analysed item. this has been 
								 corrected.
*/


*****************************************************************; 
***						Macro start;
*****************************************************************;

*** New macro parameter:
	* "input". Input dataset;
	* "output". Output dataset;
	* "var_horizontal". Name of Variable that characteristics are transposed
      					from rows into columns;
 	* "var_vertical_1". Name of the variable with the characteristics to which the
						frequencies belong;
	* "var_vertical_2". Name of the variable with the characteristics to which the
						frequencies belong. The characteristics are hyponyms of the
						Variable stated for var_vertical_1. Variable is not specified if level=1 ;
	* "column_name". Variable with the counted frequencies (specified in macro %Frequencies);

*** New macro variable
	* "level". Variable that indicates whether var_vertical_2 exists (level=2) or not (level=1).
			   If level=2 then the entries are indented in the final output;
	* "check_m_". Flag indicating whether the dataset name starts wit m_;
	* "sort". If sort equals 1 then the data are sorted by frequency, else they are sorted as in the input dataset;

*** New variable:
	* "length_var". Length of the entry for macro variable "var_horizontal" ;
	* "var_description". Description of the analyzed variable, the formatted values belong to;
	* "all_format_char". Variable with all formatted values of macro variable "var_horizontal" separated by a blank. 
	* "order". Number indicating the rank of the entrys in the table
	* "suborder".  Number indicating the rank of the entrys within an umbrella term;

%macro ReformatFrequencies(input, output, var_horizontal, var_vertical_1, var_vertical_2, var_vertical_3, column_name);
	data _null_;
		if upcase(substr("&output", 1, 2)) eq "M_" then do;
			output = substr("&output", 3, length("&output")-2);
			call symput ( "output", compress(output));
		end;
	run;

	%if 	  &var_vertical_2 eq %then %let level=1;
	%else %if &var_vertical_3 eq %then %let level=2;
	%else %if &var_vertical_3 ne %then %let level=3;;

	%if &level eq 1 %then %do;
		%let var_vertical_2=&var_vertical_1;
	*** replace ".","," and " " to prevent errors due to those characters
		check whether the name of the output dataset starts with m_;
		data input;
			length &var_horizontal $32.;  
			set &input (rename=(&var_horizontal=var_hor_n) rename=(&var_vertical_2=var_ver_n));
			&var_horizontal = vvalue(var_hor_n);
			&var_vertical_2 = vvalue(var_ver_n);
			length_var 		= length(&var_horizontal); 
			&var_horizontal = tranwrd(&var_horizontal, " ", "_");
			&var_horizontal = tranwrd(&var_horizontal, ".", "");
			&var_horizontal = tranwrd(&var_horizontal, ",", "_");
			&var_horizontal = substr(&var_horizontal, 1, length_var);
			&var_horizontal = tranwrd(&var_horizontal, "__", "_");
		run; 

		data order_entries;
			set input (drop=suborder);
			retain suborder 0;
			if _n_=1 then suborder=order;
			suborder=suborder+0.01;
		run;
		
		proc sort data=order_entries nodupkey;
			by &var_vertical_2;
		run;

		data order_entries;
			set order_entries (keep=&var_vertical_2 order suborder);
		run;
	%end;

	
	%if &level ne 1  %then %do;
	*** replace ".","," and " " to prevent errors due to those characters
		check whether the name of the output dataset starts with m_;
		data input;
			length &var_horizontal $32.;  
			set &input (rename=(&var_horizontal = var_hor_n) rename=(&var_vertical_2 = var_ver_n) %if &var_vertical_3 ne %then rename=(&var_vertical_3 = var_ver_2_n););
			&var_horizontal = vvalue(var_hor_n);
			&var_vertical_2 = vvalue(var_ver_n);
			%if &var_vertical_3 ne %then  &var_vertical_3 = vvalue(var_ver_2_n);;
			length_var 		= length(&var_horizontal); 
			&var_horizontal = tranwrd(&var_horizontal, " ", "_");
			&var_horizontal = tranwrd(&var_horizontal, ".", " ");
			&var_horizontal = tranwrd(&var_horizontal, ",", "_");
			&var_horizontal = substr(&var_horizontal, 1, length_var);
			&var_horizontal = tranwrd(&var_horizontal, "__", "_");
		run;
		
		data suborder_entries;
			set input;
			if mod(suborder, 1) ne 0;
		run;
			
		proc sort data=suborder_entries nodupkey;
			by &var_vertical_2 %if &var_vertical_3 ne %then &var_vertical_3; &var_vertical_1;
		run;

		data suborder_entries;
			set suborder_entries (keep= &var_vertical_1 &var_vertical_2 %if &var_vertical_3 ne %then &var_vertical_3; order suborder);
		run;	
	%end;

	*** delete repeated entries of var_horizontal;
	proc sort data=input out=nodups nodupkey;
		by &var_horizontal;
	run;
	%global all_format_char;
	*** put all formatted values of var_horizontal in the variable all_format_char;
	data _null_;
		set nodups end=eof;
		if substr(strip(&var_horizontal),1,1) ne "_";
		length all_format_char $300.;
		retain all_format_char;
		all_format_char=catx(" ", all_format_char, &var_horizontal);
		call symput("all_format_char", strip(all_format_char));
	run;

	%put &all_format_char;

	proc sort data=input;
		by &var_vertical_1 &var_vertical_2 %if &var_vertical_3 ne %then &var_vertical_3; &var_horizontal;
	run;

	*** delete  all unwanted entries in the output dataset
		check whether the name of the output dataset already starts with m_;
	data output;
		set input(drop=order suborder);
		if substr(strip(&var_horizontal),1,1) ne "_";
		check_m_ = substr("&output",1,2);
		call symput ("check_m_",compress(check_m_));
	run;

	%if &level ne 3  %then %do;
	*** transpose the characteristics of var_horizontal from rows to columns;
		data output;
			set output;
			length &all_format_char $300.;
			retain &all_format_char;
			retain i 0;
			by %if &level ne 1 %then &var_vertical_1; &var_vertical_2 &var_horizontal;
			array X{*} &all_format_char;
			i = i+1;
			if first.&var_vertical_2 %if level eq 2 %then or first.&var_vertical_1; then i=1;
			X{i}=&column_name;
			if last.&var_vertical_2 %if level eq 2 %then or last.&var_vertical_1; then output;
		run;
	%end;
	%else %if &level eq 3 %then %do;
		data output;
			set output;
			length &all_format_char $300.;
			retain &all_format_char;
			retain i 0;
			by &var_vertical_1 &var_vertical_2 &var_vertical_3 &var_horizontal;
			array X{*} &all_format_char;
			i = i+1;
			if first.&var_vertical_1 or first.&var_vertical_2 or first.&var_vertical_3 then i=1;
			X{i}=&column_name;
			if last.&var_vertical_1 or last.&var_vertical_2 or last.&var_vertical_3 then output;
		run;
	%end;

	*** order the entries of var_vertical_1 as determined at the beginning of the macro (if level=1);
	%if &level eq 1 %then %do;
		proc sort data=order_entries;
			by &var_vertical_2;
		run;

		data first_entry;
			set input;
			if mod(suborder,1) = 0;
		run;

		data m_&output; 
			merge output order_entries;
			by &var_vertical_2;
			var_description=&var_vertical_2;	
			*drop _TYPE_ count &var_horizontal &var_vertical i;
			rename &column_name=missing;
		run;

		data m_&output;
			set m_&output;
			if substr(var_description,1,2) ne "  " then delete;
		run;
		
		data m_&output;
			set first_entry m_&output;
		run;

		proc sort data=m_&output;
			by suborder;
		run; 

	%end;

	*** order the entries of var_vertical_2 as determined at the beginning of the macro (if level=2);
	%if &level ne 1 %then %do;
		proc sort data=suborder_entries;
			by &var_vertical_1 &var_vertical_2 %if &level eq 3 %then &var_vertical_3;;
		run;

		data first_entry;
			set input;
			if mod(suborder,1) = 0;
		run;

		data output;
			merge output suborder_entries;
			by &var_vertical_1 &var_vertical_2 %if &level eq 3 %then &var_vertical_3;;
			var_description="  " || &var_vertical_1;
			%if &level eq 3 %then descrp_var_vertical_3 = "    " || &var_vertical_3;;
			drop  _TYPE_  &var_horizontal i;
		run;

		proc sort data=output;
			by suborder;
		run;

		data m_&output;
			set first_entry
				output;
		run;	
	%end;

	*** delete all datasets that are not needed anymore;
	proc datasets library=work nolist;
		delete first_entry
		   input
		   nodups
		   output
		   order_entries
		   suborder_entries;
	run;	
	quit;	
%mend ReformatFrequencies;

*****************************************************************; 
***				Example of use (after %Frequency)
*****************************************************************;
*dm log 'clear';
*%ReformatFrequencies(input="Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\00_single_datasets\m_gender",
					 output=output,
					 var_horizontal=pre_race, 
    				 var_vertical_1=var_description,
					 var_vertical_2=pre_educ_lev1,
					 var_vertical_3=mhg_studyeye,
					 column_name=total);


*****************************************************************; 
***				Example of use (after %SumStatistics)
*****************************************************************;
*dm log 'clear';
*%ReformatFrequencies(input="Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\00_single_datasets\m_height",
					 output=output,
					 var_horizontal=pre_race, 
    				 var_vertical_1=var_description,
					 var_vertical_2=pre_sex2,
					 var_vertical_3=,
					 column_name=total);
