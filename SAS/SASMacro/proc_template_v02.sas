
/*
**********************************************************************************************************
Program Name:     proc_template_EA_v01.sas
Initial Date:     01-JUN-2015
Authors:          Joanna Wengrzik / Dennis Neuschwander
Shorthand Symbol: JW / DN
Sponsor/ Study:   - 
SAS-Version:      9.4
**********************************************************************************************************
Short Description: Output style for the reports of the tables, figures and listings
Risk Assessment:   High
**********************************************************************************************************
Source of data:    -
Input datasets:	   -
Output format:     -
Output type:       -
Templates /macros: -
Required programs: -
**********************************************************************************************************
Document History :
Version 	Date 	   	  Authors	 Purpose
01			01-JUN-2015	  JW / DN	 First initiation



*/

*****************************************************************************************************;
*****************************************************************************************************;
***																								  ***;
***											Programme start										  ***;
***																								  ***;
*****************************************************************************************************;
*****************************************************************************************************;


***************************************************************************************************************;
*** Define output style by proc template ;
***************************************************************************************************************;

proc template;
	define style gcpservice_style;

	*** define colors that are used in this style template (see http://www.kelm-ol.de/Ditundat/sasfarben.html);               
      class GraphColors                                                       
         "Abstract colors used in graph styles" /
		 'gramp3cstart'	  = CX0000FF 
         'gcoutlier' 	  = CX3883A8   
         'goutlier' 	  = cxB9CFE7    
		 'glabel'   	  = cx000000
         'gramp3cend' 	  = cxDD6060                                              
         'gramp3cneutral' = cxFFFFFF                                          
         'gramp3cstart'   = cx6497EB
		 'gconramp3cend'  = cxFF0000                                           
         'gconramp3cneutral' = cxFF00FF                                       
         'gconramp3cstart'   = cx0000FF  
         'gtext' 		  = cx000000
		 'headerfgemph'   = cx0033AA                                   
         'headerbgemph'   = cxB0B0B0 
		 'gcdata12' = cxF9DA04                                                
         'gcdata11' = cxB38EF3                                                
         'gcdata10' = cx47A82A                                                
         'gcdata9' = cxD17800                                                 
         'gcdata8' = cxB26084                                                 
         'gcdata6' = cx7F8E1F                                                 
         'gcdata7' = cx2597FA                                                 
         'gcdata4' = cx543005                                                 
         'gcdata5' = cx9D3CDB                                                 
         'gcdata3' = cx01665E                                                 
         'gcdata2' = cxB2182B                                                 
         'gcdata1' = cx2A25D9                                                 
         'gdata12' = cxDDD17E                                                 
         'gdata11' = cxB7AEF1                                                 
         'gdata10' = cx87C873                                                 
         'gdata9' = cxCF974B                                                  
         'gdata8' = cxCD7BA1                                                  
         'gdata6' = cxBABC5C                                                  
         'gdata7' = cx94BDE1                                                  
         'gdata4' = cxA9865B                                                  
         'gdata5' = cxB689CD                                                  
         'gdata3' = cx66A5A0                                                  
         'gdata2' = cxDE7E6F                                                  
         'gdata1' = cx7C95CA;  

     *** define fonts that are used in this style template;               
     class GraphFonts                                                        
         "Fonts used in graph styles" /                                               
         'GraphLabelFont'  = ("<monospace>, Courier",11pt,bold)  
		 'GraphLabel2Font' = ("<monospace>, Courier",11pt)  
         'GraphTitleFont'  = ("<monospace>, Courier",11pt,bold)        
         'GraphTitle1Font' = ("<monospace>, Courier",14pt,bold)       
		 'GraphValueFont'  = ("<monospace>, Courier",9pt);           
                          
      ***  define the color of the graphic background;
      class GraphBackground /                                                 
         backgroundcolor = _undef_                                    
         color = _undef_;                 
 
	  *** define the appearance of the content of the graphic, e.g. the color of bars and the symbol for simple data points;
	  *** define default symbols and colors;
      class GraphDataDefault /                                                
         endcolor = GraphColors('gramp3cend')                                 
         neutralcolor = GraphColors('gramp3cneutral')                         
         startcolor = GraphColors('gramp3cstart')                             
         markersize = 7px                                                     
         markersymbol = "X"                                              
         linethickness = 1px                                                  
         linestyle = 1                                                        
         contrastcolor = GraphColors('glabel')                                
         color = GraphColors('gcoutlier');                                        
     
 	  class ThreeColorRamp /                                                  
         endcolor = GraphColors('gramp3cend')                                 
         neutralcolor = GraphColors('gramp3cneutral')                         
         startcolor = GraphColors('gramp3cstart');                            
      class ThreeColorAltRamp /                                               
         endcolor = GraphColors('gconramp3cend')                              
         neutralcolor = GraphColors('gconramp3cneutral')                      
         startcolor = GraphColors('gconramp3cstart');    

	 * class GraphDataText /                                                   
         font = GraphFonts('GraphDataFont')                                   
         color = GraphColors('gtext');

	  *** define the font and color of the labels of the x- and y-axis ;
      class GraphLabelText /                                                  
         font = GraphFonts('GraphLabelFont')                                  
         color = GraphColors('glabel');                                       
      class GraphLabel2Text /                                                 
         font = GraphFonts('GraphLabel2Font')     
          color = GraphColors('glabel');           

	 *** define the font and colors of the values of the x- and y-axis;
      class GraphValueText /                                                  
         font = GraphFonts('GraphValueFont')                                  
         color = GraphColors('gtext');   

	  *** define color and font of the graphic title;
	  class GraphTitle1Text /           			                                      
         font = GraphFonts('GraphTitle1Font')                                 
         color = GraphColors('gtext');                                        
      class GraphTitleText /                                                  
         font = GraphFonts('GraphTitleFont')                                  
         color = GraphColors('gtext');
	  *** define style of data points;
	  class GraphData1 /                                                      
         markersymbol = "circle"                                              
         linestyle = 1                                                        
         contrastcolor = GraphColors('gcdata1')                               
         color = GraphColors('gdata1');                                       
      class GraphData2 /                                                      
         markersymbol = "plus"                                                
         linestyle = 4                                                        
         contrastcolor = GraphColors('gcdata2')                               
         color = GraphColors('gdata2');                                       
      class GraphData3 /                                                      
         markersymbol = "X"                                                   
         linestyle = 8                                                        
         contrastcolor = GraphColors('gcdata3')                               
         color = GraphColors('gdata3');                                       
      class GraphData4 /                                                      
         markersymbol = "triangle"                                            
         linestyle = 5                                                        
         contrastcolor = GraphColors('gcdata4')                               
         color = GraphColors('gdata4');                                       
      class GraphData5 /                                                      
         markersymbol = "square"                                              
         linestyle = 14                                                       
         contrastcolor = GraphColors('gcdata5')                               
         color = GraphColors('gdata5');                                       
      class GraphData6 /                                                      
         markersymbol = "asterisk"                                            
         linestyle = 26                                                       
         contrastcolor = GraphColors('gcdata6')                               
         color = GraphColors('gdata6');                                       
      class GraphData7 /                                                      
         markersymbol = "diamond"                                             
         linestyle = 15                                                       
         contrastcolor = GraphColors('gcdata7')                               
         color = GraphColors('gdata7');                                       
      class GraphData8 /                                                      
         linestyle = 20                                                       
         contrastcolor = GraphColors('gcdata8')                               
         color = GraphColors('gdata8');                                       
      class GraphData9 /                                                      
         linestyle = 41                                                       
         contrastcolor = GraphColors('gcdata9')                               
         color = GraphColors('gdata9');                                       
      class GraphData10 /                                                     
         linestyle = 42                                                       
         contrastcolor = GraphColors('gcdata10')                              
         color = GraphColors('gdata10');                                      
      class GraphData11 /                                                     
         linestyle = 2                                                        
         contrastcolor = GraphColors('gcdata11')                              
         color = GraphColors('gdata11');                                      
      class GraphData12 /                                                     
         contrastcolor = GraphColors('gcdata12')                              
         color = GraphColors('gdata12');   
	  class proctitle / color=white; 
	*** define title page;
		style contenttitle /
			font_face = 'Courier New'
			font_size = 14pt
			font_weight = medium
			just = center
			pretext="
^2n
 Statistical Output - &outputtype ^2n
 Version &version (&today) ^4n
 Study: &studyacronym ^n
 Sponsor: &sponsor ^4n	
 Author: GCP-Service Int. Ltd. & Co.KG ^n
         &report_author ^n
         Anne-Conway-Straﬂe 2 ^n
         28359 Bremen, Germany"
		OutputWidth = 100%;
	
		*** define style for data within the output document;
		*style titleAndNoteContainer / outputwidth = _undef_;
		style data / foreground= black 
			 font_face = Courier
			 font_weight = medium
			 font_size = 9pt 
			 protectspecialchars=off
			 just=left
			 asis=on
		; 
		* asis is used for indent variable entries;
		
		*** define style for header of the document;
		style header /
			protectspecialchars=off
			font_face = Courier
			font_weight = medium
			font_size = 9pt
			just=left
			asis=on
		;
		style PageNo from TitlesAndFooters / 
            font_face= Courier 
            cellpadding = 0
            cellspacing = 0
            pretext = "Page "
            posttext = " of ^{lastpage}"
            just=left 
            vjust=b; 
		*** define style for table body;
		style Table /
			cellspacing=0pt
			cellpadding=1.25
			frame=hsides
			rules=groups
			just=left;
		
		*style systemtitle /
			*font_face = Courier
			*font_weight = medium
			*font_size = 11pt
			*protectspecialchars=off
			*just=left;
		
		*** define style for footer of the document;
		style systemfooter /
			font_face = Courier
			font_weight = medium
			font_size = 11pt
			just=left;
		
		*** define style for columns of listings/tables;
		style column /
			protectspecialchars=off
			font_size = 9pt;
		
		*style notecontent;

		
		*style SysTitleAndFooterContainer;
		
		*** define page margins of the document;
		style body "Set page margins" /
			bottommargin = 0.7in
			topmargin = 0.7in
			rightmargin = 0.7in
			leftmargin = 0.7in;  
	   end;                                                                       
run;                   
quit;

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
		
		
