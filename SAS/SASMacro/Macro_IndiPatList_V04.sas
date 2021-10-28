/*
***********************************************************************************************************
Program Name:    Macro_IndiPatList_V04.sas
Initial Date:    21-Apr-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
************************************************************************************************************
Short Description: Creates the datasets for the individual patient listings for each subject in a study.
				   The listings are divided in three categories:
						- Items recorded once 
					    - Items recorded at several visits
						- Visit independent items that may be recorded several times (datasets already
						  available since the raw datasets can be used for listings)
Risk Assessment:   High
*************************************************************************************************************
Source of data:      Study
Input datasets:	     All SAS datasets with formatted and labelled crf data of the study (in work.library) 
				     itemlistxxx.csv
				     m_dataset_list (automatically generated by macro %Import or %AssignFormats)
					 m_labels_indi_pat_list (automatically imported by macro %assign labels;
Output format:       .sas7bdat, macro variable
Output type:         m_all_singles   			(ready for proc report / macro %ReportIndiPatList)
					 m_mult_vis_recorded_report (ready for proc report / macro %ReportIndiPatList)
					 m_all_subjects_list        (list of all subjects present at screening) 
					 &num_datasets (global macro variable indicating the number of processed datasets)
Templates /macros:   %CountObs, %ReformatFreqMedCode 
Required programmes: %AssignFormats and %AssignLabels have to be executed before (import the datasets and 
					 assign formats and labels respectively);
					 %import (sufficient only in case of already labelled and formatted datasets. But caution:
						Dataset with labels (m_labels_indi_pat_list) has to be in the work-library then.
**************************************************************************************************************
Document History :
Version 	Date 	    	Author	 Purpose
01			21-Apr-2015 	DN		 First initiation
02			28-Apr-2015     DN		 Macro now ready for first review
03			05-May-2015		DN 		 Variable labels have a maximal length of 256 characters and are 
									 truncated if the assigned label has more characters. Thus, the original
									 labels are used as item description instead of the assigned labels.;
04			06-May-2015		DN		 Example of use updated. Error handling due to validation 
									 (see Macro_IndiPatList_Validation-Form_20150506).

*/



*****************************************************************; 
***						Macro start;
*****************************************************************;

*** Count number of observations with a specific characteristic;  
*** New macro variable "inp_path". File path for the excel-file (itemlistxxx.csv);
*** New macro variable "input". Excel-file (itemlistxxx.csv);
*** New macro variable "out_path". File path for the datasets containing the datasets for proc report (if out_path is not specified, the work-library is used);
*** New macro variable "num_datasets". Number of datasets with formatted and labelled crf data;
*** New macro variable "num_datasets". Number of all datasets within a folder;
*** New macro variable "mult_obs". Indicator whether or not there are multiple observations for one subject in the dataset currently processed;
*** New macro variable "name_of_var". Names of all variables in a dataset that are recorded just one time
*** New macro variable "vvalue_vars". Contains a dataset-statement that saves the formatted value of each variable "item_name" in the variable "c_item_name"
*** New macro variable "lengths". Defines the length for each of the new variables "c_item_name";
*** New macro variable "mult_vis_recorded_names". Names of all variables that are recorded at several visits.
*** New macro variable "item_names".  Names of all variables that are recorded at several visits, where each name is followed by an output-statement;
*** New macro variable "k". Number of variables that are recorded at several visits;
*** New macro variable "m_selector". Flag that indicates whether the the currently processed dataset is part of the dataset "mult_visit_recorded".;
*** New macro variable "num_sections". Flag that indicates whether the the currently processed dataset is part of the dataset "mult_visit_recorded".;
*** New macro variable "num_obs". Number of different observations. For example each AE is an an observation (not each item is an observation!);
*** New macro variable "num_tables". Number of tables needed for this section;
*** New macro variable "obs_per_page". Maximal number of observations within a section that are displayed on the same page.;
*** New macro variable "column_obs_width". Column width of the report for the observations in the tables of a section.;
*** New macro variable "column_label_width". Column width of the report for the item description in the tables of a section.;
*** New macro variable "last_obs_width". Column width of the report for the observations in the last table of a section with more than nine observations overall.;
*** New macro variable "last_label_width". Column width of the report for the item description in the last table of a section with more than nine observations overall.;

*** New variable "fname&i". Macro variable that contains the dataset name;
*** New variable "check_m_". Flag indicating whether the dataset name starts wit m_;
*** New variable "memname". Name of the dataset;
*** New variable "order". Necessary to remember the order of the variables in the itemlist and thus in the report;
*** New variable "var_occurrence". Indicates all datasets in which an specific item name is present;
*** New variable "counts". Generated by proc freq. Number of datasets in which an specific item name is present;
*** New variable "name". Equal to variable item_name in the itemlist;
*** New variable "_name_". Generated by proc report. Contains the former name of the variables.
*** New variable "obs1". Contains the value of entry for the variable "name". Each subject has an "obs1" for a "name";
*** New variable "suborder". Only necessary to have a compatible input dataset for the macro %ReformatFreqMedCode;


*dm log 'clear';
option spool;
%macro IndiPatList (inp_path, input, out_path);
	%if %sysfunc(exist(work.m_all_singles)) %then %do;
		proc datasets library=work nolist;
	 		delete m_all_singles;
		run;
	%end;
	
************************************************* Classify Variables *************************************************;
	%global num_datasets;
	*** Declare a macro variables for each dataset name ("m_dataset_list" contains the name of all datasets with crf-data);
	data m_dataset_list;
		set m_dataset_list end=last;
		if compress(check_m_) eq "0" then do;
			fname 	 = "m_" || strip(fname);
			check_m_ = "1";
		end;
		call symput('fname'||trim(left(put(i,8.))),scan(trim(fname),1,'.'));
		if last then call symput('num_datasets',trim(left(put(i,8.))));
	run;
	%let num_datasets=&num_datasets;

	%do i = 1 %to &num_datasets;
		*** Get a list of all variables and further information of the datasets (&num_datasets is the number of dataset available) ;
		proc contents data=&&fname&i out=contents_&i (rename=(name=item_name)) noprint;
		run;

		*** Assign the labels and check whether there are several observations for at least one subject. If yes, the macro variable "mult_obs" equals 1 ;
		data &&fname&i;	
			length memname $32.; 
			set &&fname&i end=last;
			memname = upcase("&&fname&i");
		run;

		*** Generate the dataset "contents" that contains the information of each dataset "contents_&i";
		data contents;
			set %if %sysfunc(exist(work.contents)) %then contents; contents_&i (keep=memname item_name varnum label);
		run;
	%end;

	proc sort data=contents;
		by item_name;
	run;

	proc sort data=labels_indi_pat_list;
		by item_name;
	run;

	data contents; 
		merge contents (drop=label) labels_indi_pat_list (keep=item_name label);
		by item_name;
	run;

	*** import the itemlist of the study;
	libname inp_path "&inp_path";
		data _null_;
		infile "&inp_path.&input" firstobs=2;
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
		set itemlist_excel (keep=study_name event_name event_order section_label section_title section_order item_name item_order);
		retain order 0;
		order = order + 1;
	run;

	proc sort data=itemlist_excel;
		by item_name event_order;
	run;

	*** the dataset "order" is necessary to have the information in which order the variables are recorded for the first time;
	data order;
		set itemlist_excel;
		by item_name;
		if first.item_name then output;
	run;


	data itemlist_contents;
		merge contents (in=a) order;
		if a;
		by item_name;
	run;
	

	***count the number of events in which an "item_name" is recorded (item_name indicates the name of the crf-item);
	proc freq data=itemlist_contents noprint;
		tables item_name /out=freq_item_name; 
	run;

	data itemlist_contents; 
		merge freq_item_name itemlist_contents;
		by item_name;
	run;

	*** generate the variable "var_occurrence" which indicates in which datasets an "item_name" is present;
	data var_occurrence;
		length var_occurrence $32767.;
		set itemlist_contents;
		retain var_occurrence;
		by item_name;
		var_occurrence = catx(" ", var_occurrence, MEMNAME);
		if last.item_name then do;
			output;
			var_occurrence = "";
		end;
	run;
	
	proc sort data=var_occurrence;
		by item_name;
	run;

	data contents_and_occurrence;
		merge itemlist_contents var_occurrence;
		by item_name;
	run;

	*** classify all variables, that is crf-items. The crf-items are divided in "single elicitation" (once recorded) or "recorded in several visits" (mult_vis_recorded);
	data mult_vis_recorded once_recorded;
		set contents_and_occurrence;
			if count gt 1 then output mult_vis_recorded;
			else if count le 1 then output once_recorded;
	run;

************************************************* Process Variables in the Dataset "once_recorded" *************************************************;
	data once_recorded;
		set once_recorded (rename=(item_name=name_old));
		name = "c_" || strip(name_old);
		drop name_old;
	run;

	*** rename item names by adding a "c_" to the old item name in order to be able to merge the dataset "once_recorded" with the dataset "var_values_trans";   
	proc sort data=once_recorded;
		by memname name;
	run;
	*** generate three macro variables, where the first contains the names of all variables in a dataset that are recorded just one time ("name_of_var"), the second
		contains a dataset-statement that saves the formatted value of each variable "item_name" in the variable "c_item_name" (vvalue_vars) and the third defines the length for each
		of the new variables "c_item_name" (lengths);
	%do i = 1 %to &num_datasets;
		data _null_;
			length names_of_var
		    vvalue_vars	 
		    lengths $32767.;
			set contents_&i end=eof;
			retain 	names_of_var
		  		 	vvalue_vars
		   			lengths;
			names_of_var = strip(names_of_var) || " "||  strip(item_name);
			vvalue_vars  = strip(vvalue_vars) || " c_"||  strip(item_name) || " = strip(vvalue(" || strip(item_name) || "));";
			lengths = strip(lengths) || " length c_" || strip(item_name) ||" $400.;"; 
 			if eof then do;
	 			names_of_var = strip(names_of_var) || ";";
				call symput("names_of_var", names_of_var);
				call symput("vvalue_vars", vvalue_vars);
				call symput ("lengths", lengths);
			end;
		run;

	*** modify the dataset "&&fname&i" (it contains the crf-items of an event) and modify the data with the macro variables defined in the previous step;
		data var_values_&i;
			set &&fname&i;
			&lengths;
			&vvalue_vars;
			studyeventrepeatkey = input(c___studyeventrepeatkey, 3.);
			itemgrouprepeatkey = input(c___itemgrouprepeatkey, 3.);
			drop 	c___itemgrouprepeatkey 
					c___studyeventrepeatkey 
					memname
					&names_of_var
				;
		run; 
	
		proc sort data=var_values_&i;
			by c_subject_id studyeventrepeatkey itemgrouprepeatkey; 
		run;
		
		data _null_;
			retain mult_obs 0;
			set var_values_&i;
			if studyeventrepeatkey gt 1 or itemgrouprepeatkey gt 1 then mult_obs = 1;
			call symput("mult_obs", compress(put(mult_obs, 2.)));			
		run;
	*** transpose the dataset "var_values" in order to have each recorded crf-item and its value/entry in a vertical structure and not a horizontal structure.
			Furthermore, the data of each subject remain in a vertical structure (that is the data of one subject are directly below the data of another subject and thus 
			elicitated in the same variable;			
		proc transpose data=var_values_&i  out=var_values_trans_&i prefix=obs;
			by c_subject_id;
			var _all_; 
		run;
	
	*** prepare data_set var_value_trans for merging by the rename-statement and generate a macro variable for the dataset currently processed;
		data var_values_trans_&i;
			set var_values_trans_&i (rename=(_name_=name));
			memname = upcase("&&fname&i");
			dataset_order = &i;
			name = compress(name);
		run;

		proc sort data=var_values_trans_&i;
			by memname name;
		run;

	*** merging of the indicated datasets. Only the observations that are present in both datasets are kept. Variable dummy is necessary for the proc report; 
		data singles_event_&i;
			length section_descr $32767.;
			merge once_recorded (in=a) var_values_trans_&i (in=b);
 			by memname name;
			array obs{*} obs:;
	 		if a and b;
			if section_order lt 10 then section_descr = "0" || strip(put(section_order,2.)) || " - " || strip(section_title);
			else if section_order ge 10 then  section_descr = strip(put(section_order,2.)) || " - " || strip(section_title);
			label section_descr="Section ";
			mult_obs = &mult_obs;
			rename c_subject_id=subject_id;	
			do p = 1 to dim(obs);
				if compress(obs[p]) eq "" then obs[p]="."; 
			end;
			dummy=1;
		run;

	*** generate a dataset that contains all datasets "singles_event_&i";
		data m_all_singles;
			set %if %sysfunc(exist(work.m_all_singles)) %then m_all_singles; singles_event_&i;
		run;
	%end;

	proc sort data=m_all_singles;
		by subject_id;
	run;

	%if &out_path ne %then %do;
		data out_path.m_all_singles;
			set m_all_singles;
		run;
	%end;

	data m_all_subjects_list;
		set m_all_singles;
		if event_order eq 1;
	run;

	data m_all_subjects_list;
		set m_all_subjects_list;
		by subject_id;
		if first.subject_id then output;
		keep subject_id;
	run;





************************************************* Process Variables in the Dataset "mult_vis_recorded" *************************************************;
	***	Only those variables are kept that are not variables generated by the system but crf-items;
	data mult_vis_recorded;
		set mult_vis_recorded;
		if upcase(item_name) in ("SUBJECT_ID" "__SUBJECTKEY" "MEMNAME"
								 "__STUDYEVENTREPEATKEY" "__ITEMGROUPREPEATKEY" "__STUDYEVENTOID") then delete;
	run;

	proc sort data=mult_vis_recorded;
		by item_name;
	run;

	*** generate five macro variables, where the first contains the names of all variables in the dataset "mult_vis_recorded" ("mult_vis_recorded_names"),
		the second contains a dataset-statement that creates for each variable of the dataset "mult_vis_recorded" an observation for the variable "item_name" that corresponds to the
		variable name, the third contains a dataset-statement that saves the formatted value of each variable "item_name" in the variable "c_item_name" (vvalue_vars), the fourth
		defines the length for each of the new variables "c_item_name" (lengths) and the fifth counts the num_datasets number of items in the dataset currently processed;
	%let lengths=;
	%let vvalue_vars=;
	data _null_;
		length mult_vis_recorded_names
		  	   vvalue_vars	 
			   lengths 
			   item_names $32767.;
		set mult_vis_recorded end=eof;
		by item_name;
		retain mult_vis_recorded_names
			   vvalue_vars
		   	   lengths
			   item_names;
		retain k 0;
		if first.item_name and item_name ne "FU_OPHT_ABN_PUPIL_OTH_SPEC_SEV1" then do;
			item_names = strip(item_names) || " " ||  'item_name = "' || strip(item_name) || '"; output;';
			mult_vis_recorded_names = strip(mult_vis_recorded_names) || " " ||  strip(item_name);
			vvalue_vars  = strip(vvalue_vars) || " c_"||  strip(item_name) || " = strip(vvalue(" || strip(item_name) || "));";
			lengths = strip(lengths) || " length c_" || strip(item_name) ||" $400.;"; 
			k = k + 1;
		end;
		if last.item_name then output;
		if eof then do; 
			call symput ("item_names", item_names);
			call symput ("mult_vis_recorded_names", mult_vis_recorded_names);
			call symput ("vvalue_vars", vvalue_vars);
			call symput ("lengths", lengths);
			call symput ("k", trim(left(put(k,8.))));
		end;
	run;

	*** generate a dataset that contains the name of all crf-items that are recorded at several visits and keep some selected information;
	data itemname_list;
		set mult_vis_recorded;
		by item_name;
			if first.item_name then output;
			keep item_name var_occurrence label order;
	run;

	*** generate a dataset that contains the name of each visit that comprises an crf-item that was recorded at several visits (this process needs three steps);	
	data memname_list;
		merge itemname_list (in=a) itemlist_contents;
		if a;
		by item_name;
	run;
	
	proc sort data=memname_list;
		by memname;
	run;

	data memname_list;
		set memname_list (keep=memname);
		by memname;
		if first.memname and memname ne "" then output;
	run;	

	*** generate a dataset with all data recorded;
	%do i = 1 %to &num_datasets;  
		data check_mult_visit_data;	
			merge &&fname&i (in=a) memname_list (in=b) end=last;
			by memname;
			if a and b;
		run;		
		
		%CountObs(input=check_mult_visit_data, name=selector, condition=);

		%put &m_selector;

		%if &m_selector gt 0 %then %do;
			data all_data;
				set %if %sysfunc(exist(work.all_data)) %then all_data; &&fname&i;
			run;
		%end;
	%end;

	*** keep only variables that have been recorded at several visits;
	data all_data;
		set all_data;
		keep &mult_vis_recorded_names subject_id __subjectkey memname __studyeventrepeatkey __itemgrouprepeatkey;
	run; 

	data all_data;
		&lengths
		set all_data;
		&vvalue_vars;
		drop &mult_vis_recorded_names;
	run;

	proc sort data=all_data out=subject_list (keep=subject_id) nodupkey;
		by subject_id;
	run;

	proc sort data=all_data;
		by subject_id __subjectkey memname __studyeventrepeatkey __itemgrouprepeatkey;
	run;

	*** transpose the dataset "all_data" in order to have each recorded crf-item and its value/entry in a vertical structure and not a horizontal structure.
		Furthermore, the data of each subject remain in a vertical structure (that is the data of one subject are directly below the data of another subject and thus 
		elicitated in the same variable;
	proc transpose data=all_data out=all_data_trans prefix=obs;
		by subject_id  __subjectkey memname __studyeventrepeatkey __itemgrouprepeatkey;;
		var c_:;
	run;

	*** prepare merging by correcting the names of the crf-items;
	data all_data_trans;
		set all_data_trans;
			_name_ = substr(_name_, 3, length(_name_)-2);
	run;

	proc sort data=all_data_trans;
		by memname;
	run;

	*** add an entry of each itemname to each entry for the variable "memname" (that corresponds to the study events) whether or not it was recorded at this study event;
	data itemname_memname_list;
		length item_name $32.;
		set memname_list;
		&item_names;
	run;

	*** generate a dataset that contains the same information as the dataset "itemname_memname_list" but with an entry for each subject_id, item_name and memname;
	proc sql;
		create table itemname_memname_subject_list as select * from itemname_memname_list as a cross join subject_list as b; 
	quit;

	proc sort data=itemname_memname_subject_list;
		by item_name;
	run;

	*** create a dummy dataset "dummy_dataset" that stucks up the dataset with the real data in order to ensure correct performance of the macro %ReformatFreqMedCode.
		This dataset is necessary, as the macro assigns values in a specified order to the visits. For example, if the entry for visit 4 is missing, the entry of visit 5
		would be assigned to visit 4. The missing value may result from the fact that the corresponding item is not part of the crf for visit 4. Thus, the dummy datasets 
		ensures that there is an entry for each item that has been recorded in several visits whether or not it is part of the crf for this visit;  
	data dummy_dataset;
		merge itemname_memname_subject_list itemname_list;
		by item_name;
	run;

	proc sort data=dummy_dataset;
		by memname item_name subject_id;
	run;
	
	proc sort data=memname_list;
		by memname;
	run;

	*** mergen of the indicated datasets in order to keep only entries of visits at which at least some items have been recorded at several visits;
	data selected_data_trans;
		merge memname_list (in=a) all_data_trans (in=b rename=(_name_=item_name));
		by memname;
		if a;
	run;

	proc sort data=selected_data_trans;
		by  memname item_name subject_id;
	run;

	*** stuck up the dataset "selected_data_trans" by the dataset "dummy_dataset";  
	data mult_vis_recorded_data;
		merge selected_data_trans (in=a) dummy_dataset (in=b);
		by memname item_name subject_id;
		if compress(obs1) eq "" then obs1=".";
	run; 
	
	proc sort data=mult_vis_recorded_data;
		by item_name;
	run;

	***  this step is necessary in order to differentiate between missing values (indicated by ".") and an item that was not recorded at the visit (indicated by "").
		 Furthermore, the variable "suborder" is generated in order to ensure the correct performance of the macro %ReformatFreqMedCode; 
	data mult_vis_recorded_data;
		merge mult_vis_recorded_data (in=a drop=order)
			  order;
		by item_name;
		if a;
		if find(strip(upcase(var_occurrence)), strip(upcase(memname)), 1) eq 0 then obs1="";
		suborder=1;
	run;
	
	%ReformatFreqMedCode(input=mult_vis_recorded_data, output=mult_vis_recorded_report, umbrella_term=, var_horizontal=memname, var_vertical_1=item_name, var_vertical_2=subject_id, var_vertical_3=, column_name=obs1);

	proc sort data=m_mult_vis_recorded_report;
		by order;
	run;

	%if &out_path ne %then %do;
		data out_path.m_mult_vis_recorded_report;
			set m_mult_vis_recorded_report;
		run;
	%end;

	*** delete all datasets that are not needed anymore;
	proc datasets library=work nolist;
	 	delete all_data
			   all_data_trans
			   check_mult_visit_data
			   contents
			   contents_1 - contents_&num_datasets
			   contents_and_occurrence
			   dummy_dataset 
			   freq_item_name
			   itemlist_contents
			   itemlist_excel
			   itemname_list
			   itemname_memname_subject_list
			   itemname_memname_list
			   labels_indi_pat_list
			   memname_list
			   mult_vis_recorded_data
			   mult_vis_recorded
			   once_recorded
			   order
			   selected_data_trans
			   singles_event_1 - singles_event_&num_datasets
			   subject_list
			   var_occurrence
			   var_values_1 - var_values_&num_datasets
			   var_values_trans_1 - var_values_trans_&num_datasets;	   
	run;	
	quit;	
%mend IndiPatList;

*****************************************************************; 
***						Example of use;
*****************************************************************;
/*
dm log 'clear';
%include "Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\03_Macros\Macro_CountObs_V03.sas";
%include "Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\03_Macros\Macro_ReformatFreqMedCode_V07.sas";
%include "Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\03_Macros\Macro_AssignLabels_V02.sas";
%include "Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\03_Macros\Macro_AssignFormats_V05.sas";
%let lib_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\05_Output\;
%AssignFormats(inp_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\04_Excelfiles\,
			   input=ITEMLIST_data_20150130.csv, 
			   lib_path=&lib_path,
			   library_name=m_example_library,
			   output=m_formats, 
			   folder_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\01_Data\,
			   out_path=);
%AssignLabels  (lab_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\02_Programmes\,
				label_prog=m_new_labels_duplicates.sas,
				label_dat=labels_indi_pat_list, 
				folder_path=,
			   	out_path=, 
				mode=2);
*/
/*
dm log 'clear';
%IndiPatList (inp_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\04_Excelfiles\, 
			  input=ITEMLIST_data_20150130.csv,
			  out_path=);
*/
