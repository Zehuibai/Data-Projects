##install.packages("officer")
library(officer)
library(tidyverse)
library(lubridate)
library(stringr)

####################################################################################################
#                        USER INPUT
#
####################################################################################################

Study_Acronym <- "UVEA839"
Sponsor <- "Carl Zeiss Meditec AG"
Author <- "RPA"

# project_path <- "Z:/STUDIES/ZEISS/UVEA839/WORKSPACE/"
project_path <- "C:/Users/zbai/Documents/GitHub/R-Projects/SAS_Mock_Templete_Generator/"

mock_file <- "Mock TFLs_Template_V1.0.docx"
output_path <- "07_STATISTIC/04_Statistical_Analysis/01_Final"
####################################################################################################


### Read document and get footnote and header
Mock_T <- officer::read_docx(paste0(project_path, "07_STATISTIC/08_Mock_TFL/", mock_file))
df <- docx_summary(Mock_T)
df2 <- df %>% filter(style_name %in% c("Mock Footnote Table", "Mock Header Table")) %>% 
  mutate(ID = ifelse(style_name == "Mock Header Table", doc_index, NA)) %>% 
  fill(ID)

### Get current date
Sys.setlocale("LC_ALL","English")
today <- as.character(toupper(format(Sys.Date(), "%d/%b/%Y")))


textSpltr1 <- function(string){
  if(length(str_split(string, "/")[[1]]) == 2){
    splitCheck <- str_split(string, "/")[[1]]
    pos1 <- regexpr("<", splitCheck[1], fixed = T)[1]
    end1 <- regexpr(">", splitCheck[2], fixed = T)[1]
    return( paste0(substr(splitCheck[1], 1,pos1-1), substr(splitCheck[2], 1, end1-1)))
  } else {
    return(string)
  }
}

textSpltr2 <- function(string){
  if(length(str_split(string, "/")[[1]]) == 2){
    splitCheck <- str_split(string, "/")[[1]]
    return( gsub("<", "",splitCheck[1], fixed=T))
  } else {
    return(string)
  }
}

multiuseTable <- function(df){
  df2 <- input %>% 
    mutate(text = map(text,textSpltr))
}

createBody <- function(IDselect, input = NULL){
  
  if(is.null(input)){
   input <- df2 %>% filter(ID == IDselect)
  }
  
  ### 0) Check if this is a multi-use Mock-Table
  Check <- input %>% 
    filter(row_id == 1 & cell_id == 1 & style_name == "Mock Header Table") %>% 
    pluck("text")
  
  if(grepl("<", Check, fixed = T)& grepl(">", Check, fixed = T) ){
    inputCont <- input %>% 
      mutate(text = map(text,textSpltr1))
    
    inputRec <- input %>% 
      mutate(text = map(text,textSpltr2))
    
    createBody(input = inputRec)
    input <- inputCont
    
  }
  
  
  ### 1) Put together the file name
  filename <- input %>% 
    filter(row_id == 2 & cell_id == 1 & style_name == "Mock Header Table") %>% 
    pluck("text")
  
  filenumber <- input %>% 
    filter(row_id == 1 & cell_id == 1 & style_name == "Mock Header Table") %>% 
    pluck("text")
  
  filenumber2 <- gsub("Table"  , "T", filenumber)
  filenumber2 <- gsub("Listing", "L", filenumber2)
  filenumber2 <- gsub("Figure" , "F", filenumber2)
  filenumber2 <- gsub(" ", ""  , gsub(".", "_", filenumber2, fixed = T), fixed = T)
  filenumber2 <- gsub(":", "", filenumber2, fixed = T)
  fullname <- paste(Study_Acronym, filenumber2, filename, "D01_0_0", sep = "_")
  
  if(grepl("Table",filenumber)){
    type = "Table"
  } else if(grepl("Listing",filenumber)){
    type = "Listing"
  } else if(grepl("Figure",filenumber)){
    type = "Figure"
  }
  
  
  ### 2) Get table titles
  caption1 <- input %>% 
    filter(row_id == 1 & cell_id == 2 & style_name == "Mock Header Table") %>% 
    pluck("text")
  
  caption2 <- input %>% 
    filter(row_id == 2 & cell_id == 2 & style_name == "Mock Header Table") %>% 
    pluck("text")
  
  caption3 <- input %>% 
    filter(row_id == 3 & cell_id == 2 & style_name == "Mock Header Table") %>% 
    pluck("text")
  
  ### 3) Build header
  Header <- paste0(c("/************************************************************************************\n",
    paste0("Programme name:   \t\t",fullname,".sas\n"),
    "Programme language: \tSAS 9.4\n",
    paste0("Initial date: 		\t",today,"\n"),
    paste0("Sponsor | study: 	\t",Sponsor," | ",Study_Acronym,"\n"),
    paste0("Author(s): 		 \t",Author,"\n"),
    					
    "*************************************************************************************\n",
    paste0("Short description: 	",filenumber,"	  - ",caption1,"\n"), 
    paste0("\t \t \t \t \t \t \t \t \t \t \t",caption2,"\n"),
    paste0("\t \t \t \t \t \t \t \t \t \t \t",caption3,"\n"),
    					
    "Requirements: \t \t\t---\n",
    "Risk assessment: \t\tLow\n",
    "*************************************************************************************\n",
    "Input:	 		\t\t<TBD> dataset\n",
    					
    	
    paste0("Output: 		\t\t",type," in RTF document\n"),
    					    
    paste0("Required programmes: \tcurrent version of master programme '",Study_Acronym,"_MasterProg_TFLs'\n"),
    "*************************************************************************************\n",
    "Document history :\n",
    "Version 	Date 		Author 		Purpose\n",
    paste0("D01_0_0 	",today," ",Author,"	 		First initiation\n"),
    "*************************************************************************************/\n\n")  )

  ### 4) Build Settings section
  Settings <- paste0(
    c("*************************************************************************************;\n", 
      "**************************        General settings	    ******************************;\n",
      "*************************************************************************************;\n",
      "\n",
      "*** Change the output destination to SAS and empty the log as well as the output window;\n",
      "ODS LISTING;\n",
      "DM LOG 'CLEAR';\n",
      "DM OUTPUT 'CLEAR';\n",
      "ODS EXCLUDE ALL;\n",
      "\n",
      "*** Clear old titles and footnotes;\n",
      "TITLE;\n",
      "FOOTNOTE;\n",
      "\n",
      "*** Name of this program;\n",
      paste0( "%LET THIS_PROG= ",fullname,";\n"),
      paste0("%LET THIS_TOC_LABEL = ",filenumber,";\n"),
      paste0("%LET DS_NAMES = ",Study_Acronym,"_", filenumber2,";\n"),
      paste0("%LET CAPTION_LVL1 = ",caption1,";\n"),
      paste0("%LET CAPTION_LVL2 = ",caption2,";\n"),
      paste0("%LET CAPTION_LVL3 = ",caption3,";\n\n")
      )
  )
  
  ### 5) Build programme start
  Start <- paste0(
    c("*************************************************************************************;\n", 
      "**************************        Programme start	    ******************************;\n",
      "*************************************************************************************;\n\n\n")
  )
    
  ### 6) Get all footnotes
  Footnotes <- input %>% 
    filter(style_name == "Mock Footnote Table") %>% 
    mutate(command = sprintf("footnote%d j=l height=8pt font=courier", row_id),
           foot = paste(command, paste0('"',text, '";\n')))

  ### 7) Prepare report
  Report <- paste0(
    c("*************************************************************************************;\n", 
      "**************************             Report  	      ******************************;\n",
      "*************************************************************************************;\n\n\n",
      "%IF &OUTPUT. EQ FALSE %THEN %DO;\n",
      paste0('ODS TAGSETS.RTF FILE="',project_path,'07_STATISTIC/04_Statistical_Analysis/01_Final/02_Output/01_Tables/&THIS_PROG..rtf"\n'),
      "STYLE=gcpservice_style;\n",
      "%END;\n",
      "\n",
      'ODS ESCAPECHAR="^";\n',
      "OPTIONS NODATE NONUMBER NOCENTER;\n",
      "ODS EXCLUDE NONE;\n",
    
      "\n",
      'ODS PROCLABEL="&THIS_TOC_LABEL.: &CAPTION_LVL3.";\n',	
      "TITLE1 j=l height=10pt font=courier \"&THIS_TOC_LABEL:^R/RTF'\\tab' &CAPTION_LVL1.\";\n",	
      "TITLE2 j=l height=10pt font=courier \"^R/RTF'\\tab\\tab\\tab' &CAPTION_LVL2.\";\n",	
      "TITLE3 j=l height=10pt font=courier \"^R/RTF'\\tab\\tab\\tab' &CAPTION_LVL3.\";\n",	
      "\n",
      c(Footnotes$foot) ,
      #footnote1 j=l height=8pt font=courier "* Percentage based on the number of patients with early discontinuation;";
      #footnote2 j=l height=8pt font=courier "n: Number of patients in the respective category; %: Percentage based on non-missing observations;";
      #footnote3 j=l height=8pt font=courier "&SAF_DEF.; &MITT_DEF.; &PP_DEF.;";
      sprintf('footnote%d j=l height=8pt font=courier "Output generated by program \'&this_prog.\';";\n', nrow(Footnotes)+2),
      "\n",
      "ods listing close;\n",
      "\n",
      paste0("PROC REPORT DATA=",Study_Acronym,"_", filenumber2,"_D99 HEADLINE HEADSKIP NOWD SPLIT='|' MISSING CONTENTS='';\n"),
      "COLUMNS (DUMMY (\n",
      "\tEMPTY_COLUMN\n",
      "));\n",
      "DEFINE DUMMY				/ 		ORDER 	NOPRINT;\n",
      'define empty_column		/ "" 		  	style(column)=[cellwidth=0.2 cm];\n',
      "\n",
      "\n",
      "compute before;\n",
      "line ' ';\n",
      "endcomp;\n",
      "\n",
      "compute after dummy;\n",
      "line ' ';\n",
      "endcomp;\n",
      "\n",
      'break before dummy / contents="" page;\n',
      "run; \n",
      "\n",
      "footnote; title;\n",
      "\n",
      "%IF &OUTPUT. EQ FALSE %THEN %DO;\n",
      "ods tagsets.rtf close;\n",
      "ods _all_ close;\n\n",
      "%END;\n"
    )
  )
  
  ### 8) Program end
  End <- paste0(
    "\n\n*** Cleaning up;\n",
    "PROC DATASETS NOLIST;\n",
    "DELETE ",paste0(Study_Acronym,"_",filenumber2),"_D:;\n",
    "QUIT;\n",
    "\n",
    "*************************************************************************************;\n", 
    "**************************        Programme End	      ******************************;\n",
    "*************************************************************************************;\n\n\n"
  )
  
  ### Output
  cat(c(Header, Settings, Start, Report, End), file = paste0(project_path, output_path,"/",fullname,".sas"))
  
}

sapply(unique(df2$ID), createBody)
