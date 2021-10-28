*** To do:	(alles auﬂer erste Buchstabe klein schreiben? Problem: was wenn Eigenname also Firmenname oder Bakterienart?;

/*
******************************************************************************************
Program Name:    Macro_CreateLabelProg_V04.sas
Initial Date:    21-Apr-2015
Author:          Dennis Neuschwander
Shorthand Symbol:DN
Sponsor/ Study:  -
SAS-Version:     9.4
******************************************************************************************
Short Description: Creates two programmes, where each writes a dataset containing labels
				   of variables that correspond to CRF-items.
				   The programmes can be adapted afterwards, for example in case of 
				   erroneous labels. The datasets created by the programmes are necessary 
				   for the execution of the macro %AssignLabels. 
				   Usually, the label corresponds to the item description in the itemlist. 
				   If, however, there is a unit that is not a date indicated for the item, 
				   the label corresponds to the item description and the item unit in the 
				   CRF_itemlist(e.g. Height [cm])
Risk Assessment:   High
******************************************************************************************
Source of data:    Itemlist 
Input datasets:	   itemlist_xxx.csv
Output format:     .sas,
Output type:       m_&output._duplicates.sas (creates a dataset with labels of duplicated
											  variables. That means, the item is
											  recorded in several CRFs at different visits
											  and has the same item name.)
				   m_&output._all.sas		 (creates a dataset with labels of all CRF-items)
				   m_no_label			 	 (items with missing label)
				   m_label_spec_char		 (labels with special chars)

Templates /macros: - 
Required programs: -
*****************************************************************************************
Document History :
Version 	Date 	    	Author	 Purpose
01			21-Apr-2015 	DN		 First initiation
02			29-Apr-2015		DN		 Changes necessary due to validation 
									 (see Macro_CreatLable_Validation-Form_20150423)
03			30-Apr-2015		DN		 Reduction of postprocessing of the labels by a new
									 organisation of the label datasets
04			05-May-2015		DN		 User-defined warnings added to log.
05			24-Jun-2015	    DN		 Macro now considers that the item names are unified
									 (no visit specific shorthand symbol) prior to 
									 labeling
06			25-Jun-2015		DN		 Macro now cuts of the version number of the items 
									 before creation of the labels if the
									 macro parameter "special_char" is specified.
07			-				DN		 -
08			05-Aug-2015		DN		 New macro parameter "uppercase_list". Words in this 
									 list are written in uppercase if they occur in a 
									 label. It is important to separate the words by a "\"!

*/



*****************************************************************; 
***						Macro start;
*****************************************************************;

*** New macro parameter "inp_path". File path of itemlist;
*** New macro parameter "input". Input-file (itemlistxxx.csv);
*** New macro parameter "output". Name of the sas programme which generates the dataset with the labels;
*** New macro parameter "out_path". File path for the output;
*** New macro parameter "spec_char". Special character that indicates the beginning of the version number of the item (version number will be cut of)
									 Please do not put the character in quotation marks.
*** New macro parameter "uppercase_list". Words in this list are written in uppercase if they occur in a label. It is important to separate the words 
									 	  by a "\".

*** New variable "order". Necessary to keep the order of the variables as in the itemlist;
*** ... (uppercase list_variables!;
*** All other variables are variables of the itemlist;

%macro CreateLabelProg (inp_path, input, out_path, output, spec_char, uppercase_list);
	%put &uppercase_list;
	data _null_;
		if upcase(substr("&output", 1, 2)) eq "M_" then do;
			output = substr("&output", 3, length("&output")-2);
			call symput ( "output", compress(output));
		end;
	run;

	*** Import excel file;
	libname inp_path "&inp_path";
	data _null_;
		infile "&inp_path.&input" firstobs=3;
		file "&inp_path.labels.dat";
		input;
		put _infile_;
	run;

	proc import datafile="&inp_path.labels.dat"
		replace 
		dbms=csv
		out=work.labels_excel;
		guessingrows=10000;
		getnames=yes;
		delimiter=';';
	run;

	data labels_excel;
		set labels_excel;
		spec_char=compress("&spec_char", "'");
		if compress(spec_char) ne "" then do;
			spec_char_pos = find(item_name, "&spec_char", -(length(compress(item_name))));
			item_name 	  = substr(item_name, 1, spec_char_pos-1);
		end;
	run;

	*** generate variable "order" to remember the order of the variables in the itemlist;
	data labels_excel;
		length			entry				  
						modified_entry 		  
						lowercase_entry       
						entry_with_comma  	  
						entry_with_dot	 	  
						entry_with_hyphen 	  
						uppercase_entry  	  
						uppercase_entry_hyphen $50.;
		set labels_excel;
		retain order 0;
		order = order + 1;
			if compress(item_description) ne "" then do;
				num_dots = count(strip(item_description), ". ");
				item_description = upcase(substr(item_description, 1,1))||lowcase(substr(item_description, 2,length(item_description)-1));
				
				do l = 1 to num_dots;
					if l = 1 then item_description = substr(item_description, 1, find(item_description, ". ", 1)+1) ||
												 upcase(substr(item_description, find(item_description, ". ", 1)+2, 1)) ||
												 substr(item_description, find(item_description, ". ", 1)+3, length(item_description)-find(item_description, ". ", 1)-2);
					else if l gt 1 then item_description = substr(item_description, 1, find(item_description, ". ", length(scan(item_description,2,'. '))+1)+1) ||
												 	   upcase(substr(item_description, find(item_description, ". ", length(scan(item_description,2,'. '))+1)+2, 1)) ||
												 	   substr(item_description, find(item_description, ". ", length(scan(item_description,2,'. '))+1)+3, 
													   length(item_description)-find(item_description, ". ", length(scan(item_description,2,'. '))+1)-2);
				end;
			end;
			if compress("&uppercase_list") ne "" then do;
				uppercase_list= "&uppercase_list";
				num_entries   = count(uppercase_list, '/')+1;
				item_description2=tranwrd(strip(item_description), " ", "| |") || "| |";
				do entry_of_list=1 to num_entries*2;
					if 		entry_of_list le num_entries then entry = strip(scan(uppercase_list, entry_of_list,'/'));
					else if entry_of_list gt num_entries then entry = strip(scan(uppercase_list, entry_of_list-num_entries,'/')) || 's';
					modified_entry 		   = "| |" || strip(entry) || "| |";
					lowercase_entry  	   = "| |" || strip(lowcase(entry))|| "| |";
					entry_with_comma  	   = "| |" || strip(lowcase(entry)) ||',';
					entry_with_dot	 	   = "| |" || strip(lowcase(entry))|| '.';
					entry_with_hyphen 	   = "| |" || strip(lowcase(entry))||'-';
					entry_question		   = "| |" || strip(lowcase(entry))||'?';
					entry_exclamation_mark = "| |" || strip(lowcase(entry))||'!';
					uppercase_entry  	   = strip(upcase(substr(entry,1,1))) || strip(lowcase(substr(entry,2, length(entry)-1))) || "| |";
					uppercase_entry_hyphen = strip(upcase(substr(entry,1,1))) || strip(lowcase(substr(entry,2, length(entry)-1))) || '-';
					t=find(strip(item_description3), strip(lowercase_entry), 1);
					item_description2 = tranwrd(item_description2, strip(lowercase_entry), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(entry_with_comma), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(entry_with_dot), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(entry_with_hyphen), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(uppercase_entry), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(uppercase_entry_hyphen), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(entry_question), strip(modified_entry));
					item_description2 = tranwrd(item_description2, strip(entry_exclamation_mark), strip(modified_entry));
				end;
				item_description = strip(tranwrd(item_description2,"| |", " "));										
			end;
	run;

	*** create a dataset that contains all items that have no entry for the "item_description" in the itemlist;
	data m_no_label;
		set labels_excel;
		retain only_one_warning 0;
		if item_description eq "";
		if item_description eq "" and only_one_warning eq 0 then do;
			put "WARNING: MISSING ITEM_DESCRIPTION! SEE DATASET 'OUTPATH.NO_ITEM_DESCR' FOR MORE INFORMATION (USER-DEFINED WARNING)";
			only_one_warning = 1;
		end;
		keep event_name
			 event_order
			 section_label
			 section_title
			 item_name
			 item_order;
	run;

	*** create a dataset that contains all items with not permitted special characters in the item_description; 
	data m_label_spec_chars;
		set labels_excel;
		retain only_one_warning 0;
		if verify(item_description,"ABCDEFGHIKLMNOPQRSTUVWXYZ 0123456789 abcdefghijklmnopqrstuvwxyz()[]%!?.,;:<>/-+*_") ne 0;
		if verify(item_description,"ABCDEFGHIKLMNOPQRSTUVWXYZ 0123456789 abcdefghijklmnopqrstuvwxyz()[]%!?.,;:<>/-+*_") ne 0 and only_one_warning eq 0 then do;
			put "WARNING: NOT PERMITTED SPECIAL CHARACTER FOUND! SEE DATASET 'OUTPATH.LAB_SPEC_CHARS' FOR MORE INFORMATION (USER-DEFINED WARNING)";
			only_one_warning = 1;
		end;
		keep event_name
			 event_order
			 section_label
			 section_title
			 item_name
			 item_description
			 item_order;
	run;

	proc sort data=labels_excel;
		by event_oid item_name;
	run;

	*** count whether an item has duplicates in the itemlist (if count  gt 1 then there are duplicates!);
	proc freq data=labels_excel noprint;
		table item_name / out=freq_items;
	run;

	proc sort data=labels_excel;   
		by item_name item_description;
	run;
	
	data labels_excel_freq;
		merge labels_excel
			  freq_items (rename=(count=freq_item));
		by item_name;

	run;

	%let init_day=%sysfunc(today(),date9.);

	%do i = 1 %to 2;
		*** Write the SAS programme that creates the dataset with the labels;
		data _null_;
			set labels_excel_freq end=eof;
			%if &i eq 1 %then file "&out_path.m_&output._all.sas";
			%else %if &i eq 2 %then file  "&out_path.m_&output._duplicates.sas";;
			if _n_ eq 1 then do;
				put "/*";
				put 112*"*";
				if &i eq 1 then put "Programme name:      m_&output._all.sas";
				else if &i eq 2 then put "Programme name:      m_&output._duplicates.sas";
				put "Initial date:        &init_day";
				put "Author:              ";      
				put "Shorthand symbol:    ";
				put "Sponsor/study:       ";
				put "SAS-version:         9.4";
				put 112*"*";
				put "Short description:   Programme that creates a dataset with the labels for all variables that correspond to CRF items"; 
				put "Risk assessment:     High";
				put 112*"*";
				put "Source of data:      Itemlist";
				put "Input datasets:      -";
				put "Output format:       .sas7bdat";
				put "Output type:          Dataset with the labels for all variables that correspond to CRF items";
				put "Templates/macros:    ";
				put "Required programmes: ";
				put 112*"*";
				put "Document history:    ";
				put "Version     Date        Author   Purpose";
				put "01	         &init_day            First initiation";
				put "*/";
				put; 
				put 112*"*";
				put "*** Programme start";
				put 112*"*" ";";
				put;
			end;
			if compress(item_units) ne "" and compress(upcase(item_units)) ne "YYYY" and find(upcase(data_type), "DATE", 1) eq 0 
				then item_description = trim(item_description) || " [" || trim(item_units) || "]"; 
				by item_name item_description;
			%if &i eq 1 %then %do;
				if _n_=1 then put "data labels_all;" / @5 "length item_name $32. " / @12 "label" / @12 "__STUDYEVENTOID $32767.; " /;
				if upcase(compress(lag1(item_description))) eq upcase(compress(item_description)) then do;
					put @15 'item_name = "' item_name +(-1) '";';
					put @15 '__STUDYEVENTOID = "' event_oid +(-1) '";'/ @15 "output;";
				end;
				if first.item_name then do;
					put @5 'item_name = "' item_name +(-1) '";' / @5 'label' +4 ' = "'  item_description +(-1) '";' / ;
					put @5 '__STUDYEVENTOID = "' event_oid +(-1) '";'/ @5 "output;";
				end;
				else if first.item_description then do;
					put @35 'item_name = "' item_name +(-1) '";' / @35 'label' +4 ' = "'  item_description +(-1) '";' / ;
					put @35 '__STUDYEVENTOID = "' event_oid +(-1) '";'/ @35 "output;";
				end;
				else do;
					put @15 '__STUDYEVENTOID = "' event_oid +(-1) '";'/ @15 "output;";
				end;
				put;
				if eof then put 'run;';
			%end;	
			%else %if &i eq 2 %then %do;
				if _n_= 1 then put "data labels_duplicates;" / @5 "length item_name $32. " / @12 "label" / @12 "__STUDYEVENTOID $32767.;" /;
				first_=find(item_name, "_");
				if first_ gt 5 or first_ in (0,.) then first_ = 0;
				if first_=1 then first_=0;
				item_name=substr(item_name, first_+1, length(item_name)-first_);
				if freq_item gt 1 then do;
					if first.item_name then put @5'item_name = "' item_name +(-1) '";' / @5 'label' +4 ' = "'  item_description +(-1) '";' / / @5 '__STUDYEVENTOID = "' event_oid +(-1) '";'/ @5 "output;";
					else put @10 '/*' / @10 'item_name = "' item_name +(-1) '";' / @10 'label' +4 ' = "'  item_description +(-1) '";' / / @10 '__STUDYEVENTOID = "' event_oid +(-1) '";'/ @10 "output;" / @10 "*/";
					put;
				end;
				if eof then do;
					put "run;";
					put;
					put "%" "include '&out_path.m_&output._all.sas';";
					put;
					put "data labels_all_modified;" / @5 "set labels_all;" / @5 "first_=find(item_name, '_');" / @5 "if first_ gt 5 or first_ in (0,.) then first_ = 0;"
						/ @5 "if first_=1 then first_=0;" / @5 "item_name=substr(item_name, first_+1, length(item_name)-first_);" / "run;"; 
					put;
					put "proc sort data=labels_all_modified;" / @5 "by item_name;" / @5 "run;";
					put;
					put "data labels_singles;" / @5 "merge labels_all_modified (in=a)" / @11 "labels_duplicates (in=b);" / @5 "by item_name;" / @5 "if a and not b;" / "run;"; 
					put;
					put "data labels_unified_var_names;" / @5 "set labels_singles" / @9 "labels_duplicates;" / "run;"; 
				end;
			%end;
		run;
	%end;

	*** delete all datasets that are not needed anymore;
	proc datasets library=work nolist;
		 delete doubled_crf_items
		 		freq_items
				
				labels_excel_freq;
	run;	
	quit;	
%mend CreateLabelProg;

*****************************************************************; 
***						Example of use;
*****************************************************************;
*%let lib_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\02_Programmes\;
*%CreateLabelProg(inp_path=Z:\STATISTICS\07_Templates\01_Analysis\07_Example_of_Use\Macros\04_Excelfiles\,
			   	 input=ITEMLIST_manipulated.csv, 
			   	 out_path=&lib_path,
			   	 output=new_labels,
				 spec_char=);
