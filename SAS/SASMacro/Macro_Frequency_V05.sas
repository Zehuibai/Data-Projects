
/*****************************************************************************************
Program Name:    Macro_Frequency_DN.sas
Initial Date:    19-Dec-2014
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.3/9.4
******************************************************************************************
Short Description: Counts frequency n and percentage % of variable characteristics. The
				   frequencies are automatically formatted as "n(%)". Dependent on the 
				   value of the macro variable "level" the entries are intended.
Risk Assessment:   High
******************************************************************************************
Source of data:    -
Input datasets:	   SAS dataset
Output format:     .sas7bdat 
Output type:       SAS dataset containing the frequencies
Templates /macros: - 
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    Author	 Purpose
01			19-Dec-2014 DN       First initiation
02			02-Mar-2015 DN		 Programm now SOP conform, commentation, improvement
							     of flexibility
03			21-Apr-2015 DN		 Handling of class-variables improved, Check for correct
								 name of output dataset	improved
04			18-May-2015 DN		 Problem with "condition" solved
05			13-Jul-2015 DN		 Order of entries in output now following the order of the unformatted values;
*/

*****************************************************************; 
***						Macro start;
*****************************************************************;

*** Count number of observations with a specific characteristic;  
*** New macro variable "var". Analysed Variable;
*** New macro variable "suborder". Necessary to order the entries for the variable "var" in the report;
*** New macro variable "name_tab". Name of the variable "Var" in the report;
*** New macro variable "input". Input dataset;
*** New macro variable "output". Output dataset;
*** New macro variable "format". Format of the analysed variable "Var" to ensure entries even for characteristics
	that does not occur in the study but are considered;
*** New macro variable "condition". WHERE-Statement to analyse only a subpopulation;
*** New macro variable "num_subj". Number of subjects to calculate the percentage of missings;
*** New macro variable "column_name". Name of the column with the results for the calculated statistics
	in the report. The name has to be equal for all analysed variables to have all results in the same column;
*** New macro variable "level". Indention of the formatted value (not the frequencies). A increasement of level
								by 1 corresponds to two blanks.
*** New macro variable "check_m_". Flag indicating whether the dataset name starts wit m_;

*** New variable "var_description". Description of the analyzed variable, the formatted values belong to;
*** New variable "count". number of observations with a certain characteristic;

%macro Frequency (var, suborder, name_tab, input, output, format, condition, num_subj, column_name, class_variables, level);
	data _null_;
		if upcase(substr("&output", 1, 2)) eq "M_" then do;
			output = substr("&output", 3, length("&output")-2);
			call symput ( "output", compress(output));
		end;
		if compress("&condition") ne "" then do;
			condition="where " || "&condition";
			call symput ("condition", strip(condition));
		end;
	run;

	*** count frequency of each formatted value;
	proc means data=&input completetypes n noprint;
		&condition;
		class &class_variables &var  /preloadfmt;
		format &var &Format;
		var &var;
		output out=stat N=count;	
	run;
	*** count frequency of missings;
	proc means data=&input sum noprint completetypes;
		&condition;
		%if &class_variables ne %then class &class_variables /preloadfmt;;
		var &var;
		output out=missings nmiss=count;
	run;

	*** generate description of the analyzed variable the formatted values belong to;
	data analysed_var;
		length var_description $200.;
		if &level gt 1 then var_description = repeat(" ", (&level*2)-3) || &name_tab;
		else if &level eq 1 then var_description = &name_tab;
		suborder  = &suborder;
		count  = -1;
	run;
	
	*** check whether the name of the output dataset starts with m_;
	data _null_;
		check_m_ = substr("&output",1,2);
		call symput ("check_m_",check_m_);
	run;
    
	data stat;
		length var_description $200.;
		set stat;
		retain suborder &suborder;
		suborder = suborder + 0.000001;
		var_description= vvalue(&var);
       if compress(vvalue(&var)) in ("." "") then var_description ="Overall";
	run;

	*** format the entries and determine the suborder;
	data output; 
		length var_description $200.;
		set analysed_var
			stat 	 (in=a)
			missings (in=b); 
		if b then do;
			var_description = "Missing";
			suborder =&suborder + 0.00099; 
		end;
		order = floor(suborder);
		if count gt 0 then &column_name = put(count, 4.) 	 || "  (" || put(round(count/&num_subj*100,0.1), 5.1) || ")";
		else if count eq 0 then &column_name = put(0, 4.) 	 || "  (" || put(round(0.0,0.1), 5.1) || ")";
		if count ne -1 then var_description = repeat(" ", (&level*2)-1) || var_description; 
	run;
	
	%if &class_variables ne %then %do;
		proc sort data=output;
			by &class_variables suborder;
		run;
		
		data _null_;
			 class_variables = "&class_variables";
			 class_variable_1 = scan(strip(class_variables), 1, " ");
			 class_variable_2 = scan(strip(class_variables), 2, " ");
			 class_variable_3 = scan(strip(class_variables), 3, " ");
			 call symput ("class_variable_1", compress(class_variable_1));
			 call symput ("class_variable_2", compress(class_variable_2));
			 call symput ("class_variable_3", compress(class_variable_3));
		run;
		%put &class_variable_3;

		data m_&output;
			set output (rename=(&class_variable_1=class_variable_1 %if &class_variable_2 ne %then &class_variable_2=class_variable_2; %if &class_variable_3 ne %then &class_variable_3=class_variable_3;));
			retain specify_order 0;
			specify_order=specify_order + 0.001;
			suborder=suborder + specify_order;
			if _n_ = 1 then suborder=order;
			&class_variable_1 = vvalue(class_variable_1);
			drop class_variable_1;
		%if &class_variable_2 ne %then %do;
			&class_variable_2 = vvalue(class_variable_2);
			if mod(suborder,1) ne 0 and compress(&class_variable_2) in ("." "") then &class_variable_2 = "Overall";
			drop class_variable_2;
		%end;
	
		%if &class_variable_3 ne %then %do;
			&class_variable_3 = vvalue(class_variable_3);
			if mod(suborder,1) ne 0 and compress(&class_variable_3) in ("." "") then &class_variable_3 = "Overall";
			drop class_variable_3;
		%end;
			if mod(suborder,1) ne 0 and compress(&class_variable_1) in ("." "") then &class_variable_1 = "Overall"; 	
*			output;	
*			if strip(upcase(var_description)) eq "MISSING" and not eof then do; 
*				var_description   = &name_tab;
*				_stat_ 			  = "";
*				suborder  		  = suborder + 0.00001;
*				&var			  = -1;
*				&class_variable_1 = "";
*				%if &class_variable_2 ne %then &class_variable_2  = "";	
*				%if &class_variable_3 ne %then &class_variable_3  = "";	
*				output;
*			end; 
		run;
	%end;
	%else %if &class_variables eq %then %do;
		data m_&output;
			set output;
		run;
	%end;;



%mend Frequency;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%include "Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\02_Programmes\m_formats.sas";
*%Frequency (var=PRE_SEX2,
			suborder=2,
		 	name_tab="Sex", 
			input="Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\00_Single_Datasets\m_q_01_screening_ungrouped",   
			output=gender, 	
		    format=PRE_SEX2_F., 
			condition=, 
			num_subj=10, 
			column_name=total, 
			class_variables=,
			level=1);
