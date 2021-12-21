/****Auswertung Forschungsprojekt*****/
libname fw "C:/Users/flori/OneDrive/Dokumente/1_Uni/Forschungsprojekt/Auswertung";

/****Import der Daten und Aufbereitung****/
proc import datafile="C:/Users/flori/OneDrive/Dokumente/1_Uni/Forschungsprojekt/Auswertung/results_2021_11_04_no_push.csv"
        out=results
        dbms=csv
		replace;
		delimiter=';';
     	getnames=yes;
		guessingrows=1500;
run;
/*Formate für die Übersicht im Datenblatt*/
Proc format;
	value ImpfEin 	1="PRO"
					2="CON";
	value SEX 		1="MAENNLICH"
					2="WEIBLICH"
					3="DIVERS";
	value JN	 	1="JA"
					2="NEIN";
	value Deu		1="sehr gut"
					2="eher gut"
					3="mittel"
					4="eher schlecht"
					5="sehr schlecht";
	value SoWahl	1="CDU/ CSU"
					2="SPD"
					3="Gruene"
					4="Die Linke"
					5="AFD"
					6="FDP"
					7="keine Angabe"
					99="Andere";
	value FaStand	1="ledig"
					2="verheiratet"
					3="geschieden"
					4="verwitwet"
					99="sonstige";
	value Eink		1="Bis unter 500€"
					2="500 bis unter 1.000€"
					3="1.000 bis unter 1.500€"
					4="1.500 bis unter 2.000€" 
					5="2.000 bis unter 3.000 €" 
					6="3.000 bis unter 4.000 €"
					7="4.000 bis unter 5.000 €" 
					8="5.000 bis unter 6.000 €"
					9="6.000 € und mehr"; 
	value SchulAb	1="Kein Schulabschluss"
					2="Hauptschulabschluss"
					3="Realschulabschluss"
					4="Abitur/ Fachabitur"
					99="sonstige";
	value Aufm		1="Freunde und Bekannte"
					2="Flyer/ QR Codes"
					3="Facebook"
					4="Whatsapp"
					5="Telegram"
					6="Emailverteiler"
					7="Schwarzesbrett der Uni"
					8="Instagram"
					9="LinkedIn"
					10="Online Schwarzesbrett"
					11="Reddit"
					12="Studydrive"
					13="WeChat"
					99="Andere";
	value BerufAb	1="Keinen beruflichen Abschluss"
					2="beruflich-betriebliche Ausbildung"
					3="(Fach-)Hochschulabschluss"
					4="Saatsexamen"
					5="Hochschulabschluss (Promotion, Habilitation)"
					99="sonstige";
	value AgeKat	1="18 bis 25"
					2="25 bis 35"
					3="35 bis 45"
					4="45 bis 55"
					5="55 bis 65"
					6="<65";
	value ErwerbKat	1="Nicht Erwerbstätig"
					2="Vollzeit"
					3="Teilzeit"
					4="Selbstständing"
					5="Student"
					6="Student + Teilzeit"
					7="Student + Aushilfe"
					8="Ausbildung"
					9="Schüler"
					10="Praktikum / FSJ"
					11="Rentner"
					12="Minijob";
run;

/*Ersten Überblick verschaffen: Wie weit wurde der Fragebogen beantwortet*/
proc freq data=results;
	tables lastpage;
run;

/*Alle löschen die den Teil der Impfstatus abfrage nicht beantwortet haben und alle Minderjährigen*/
data results_work;
	set results;
	if lastpage<5 then delete;
	if age<18 then delete;
run;

/*Deutsch sprechen und Verstehen rauswerfen ab: 4*/
*proc freq data= results_work;
	*tables DeuSprech DeuVerst;
	*format DeuSprech Deu. DeuVerst Deu.;
*run;

*data results_work;
	*set results_work;
	*if DeuSprech>3 | DeuVerst>3 then delete;
*run;

/*Aufbereitung einzelner Variablen bei denen die Codierung fehlgeschlagen ist*/
data results_work;
	set results_work;
	if MigHin="A1" then MigHin=1;				*Migrationshintergrund zu 1&2 statt A1 und A2 ändern, kosistent mit restlicher Programmierung;
	else if MigHin="A2" then MigHin=2;
	MigHin_numeric=Input(MigHin, 8.);
	drop MigHin;
	rename MigHin_numeric=MigHin;
	if SoWahl="-oth-" then SoWahl=99; 			*Andere laut Codeplan: Kat 8;
	SoWahl_numeric=Input(SoWahl, 8.);
	drop SoWahl;
	rename SoWahl_numeric=SoWahl;
	if FaStand="-oth-" then FaStand=99; 		*Andere laut Codeplan: Kat 4;
	FaStand_numeric=Input(FaStand, 8.);
	drop FaStand;
	rename FaStand_numeric=FaStand;
	if SchulAb="-oth-" then SchulAb=99; 		*Andere laut Codeplan: Kat 5;
	SchulAb_numeric=Input(SchulAb, 8.);
	drop SchulAb;
	rename SchulAb_numeric=SchulAb;
	if BerufAb="-oth-" then BerufAb=99;			*Andere laut Codeplan: Kat 6;
	BerufAb_numeric=Input(BerufAb, 8.);
	drop BerufAb;
	rename BerufAb_numeric=BerufAb;
	if Aufm="-oth-" then Aufm=99; 				*Andere laut Codeplan: Kat 7;
	Aufm_numeric=Input(Aufm, 8.);
	drop Aufm;
	rename Aufm_numeric=Aufm;
run;

/*Zuordnung von Angaben in der Kategorie Sonstige zu den Antwortmöglichkeiten, sowie Erstellung neuer Kategorien für diese Anagben*/
/*Dokumentation der Zuordnung im Dokument: Sonstige_Übersicht.docx*/

/*Zusammenfassen des Erwerbstatus, vorherige Kodierung mit 12 Variablen*/
data results_work;
	set results_work;
	if erwerbstat8=1 & erwerbstat9=1 then ErwerbKat=6;	*Student + Teilzeit;
	else if erwerbstat8=1 & erwerbstat5=1 | erwerbstat8=1 & erwerbstat7=1 then ErwerbKat=7;	*Student + Aushilfstätigkeit;
	else if erwerbstat1=1 then ErwerbKat=1; 	*Nicht erwerbstätig;
	else if erwerbstat10=1 then ErwerbKat=2;	*Vollzeit erwerbstätig;
	else if erwerbstat9=1 then ErwerbKat=3;		*Teilzeit erwerbstätig;	
	else if erwerbstat11=1 then ErwerbKat=4;	*Selbstständig;
	else if erwerbstat8=1 then ErwerbKat=5;		*Student;
	else if erwerbstat6=1 then ErwerbKat=8;		*Ausbildung;
	else if erwerbstat3=1 then ErwerbKat=9;		*Schüler;
	else if erwerbstat4=1 then ErwerbKat=10;	*Praktikum / FSJ;
	else if erwerbstat2=1 then ErwerbKat=11;	*Rentner;
	else if erwerbstat5=1 then ErwerbKat=12;	*Minijob;
	else ErwerbKat=.;
run;


/*Zuordung anderer -oth's- zu bestehenden und neuen Kategorien*/
data results_work;
	set results_work;
	/*Sonstige: Schulabschlüsse*/
	if id=86 or id=404 then SchulAb=3;
	if id=322 then SchulAb=4;
	if SchulAb=99 and SchulAbOther="" then SchulAb=.; *Alle Personen die sonstiger Abschluss angegeben haben und dann nichts da nach werden auf Missing gesetzt;
	/*Sonstige Familienstand*/
	if id=69 or id=125 or id=488 or id=1098 then fastand=1;
	if id=505 then fastand=2;
	if id=273 or id=532 then fastand=4; *neue Kategorie für verwitwet;
	if id=533 or id=1061 then fastand=.; *unlogische Angaben, siehe Sonstige_Übersicht.docx;
	if fastand=99 and fastandother="" then fastand=.; *Alle Personen die sonstige angeben haben aber dann nicht welchen sonstigen Familienstand auf Missing;
	/*Sonstige: beruflicher Abschluss*/
	if id=238 or id=385 or id=388 or id=859 then BerufAb=1;
	if id=403 or id=446 or id=710 or id=821 or id=1077 or id=1101 then BerufAb=2;
	if BerufAb=99 and BerufAbOther="" then BerufAb=.; *Alle Personen die sonstige angeben haben aber dann nicht welchen sonstigen Berufabschluss auf Missing;
	/*Sonstige: Aufmerksam geworden*/
	if AufmOther="#fragebogen" then Aufm=.;
	if AufmOther="An der Straßenbahnhaltestelle" then Aufm=2;
	if AufmOther="Anzeige auf dem schwarzen Brett der Uni Oldenburg" then Aufm=7;
	if AufmOther="Anzeige auf stud.ip" then Aufm=7;
	if AufmOther="Anzeige bei Stud.IP der Universität Oldenburg" then Aufm=7;
	if AufmOther="Aushang im Testzentrum" then Aufm=2;
	if AufmOther="Blackboard der Uni" then Aufm=7;
	if AufmOther="Digitales Schwarzes Brett der Uni" then Aufm=7;
	if AufmOther="digitales Schwarzes Brett der Uni" then Aufm=7;
	if AufmOther="Digitales schwarzes Brett der Universität" then Aufm=7;
	if AufmOther="Digitales Schwarzes Brett einer Universität" then Aufm=7;
	if AufmOther="Digitales Schwarzes Brett Uniportal" then Aufm=7;
	if AufmOther="DSB" then Aufm=10;
	if AufmOther="DSBLE" then Aufm=10;
	if AufmOther="dsble-de" then Aufm=10;
	if AufmOther="ein Forum" then Aufm=99;
	if AufmOther="Ein Forum" then Aufm=99;
	if AufmOther="elearning uni bremen" then Aufm=7;
	if AufmOther="E-Mail-Verteiler der Uni" then Aufm=6;
	if AufmOther="Familie" then Aufm=1;
	if AufmOther="Forum" then Aufm=99;
	if AufmOther="Forum der Universität" then Aufm=7;
	if AufmOther="Geteilt durch Uni Bremen auf Instagram" then Aufm=8;
	if AufmOther="Homepage der Uni" then Aufm=7;
	if AufmOther="Inserat Uni Bremen" then Aufm=7;
	if AufmOther="Insta" then Aufm=8;
	if AufmOther="Instagram" then Aufm=8;
	if AufmOther="Instagram (Uni Bremen)" then Aufm=8;
	if AufmOther="Instagram @uni_bremen" then Aufm=8;
	if AufmOther="Instagram account der Uni Bremen" then Aufm=8;
	if AufmOther="Instagram der Uni Bremen" then Aufm=8;
	if AufmOther="Instagram der Universität Bremen" then Aufm=8;
	if AufmOther="Instagram Post der Uni Bremen" then Aufm=8;
	if AufmOther="Instagram Story der Uni Bremen" then Aufm=8;
	if AufmOther="Instagram Story der Universität Bremen" then Aufm=8;
	if AufmOther="Instagram Story Uni Bremen" then Aufm=8;
	if AufmOther="Instagram Story Uni HB" then Aufm=8;
	if AufmOther="Instagram Uni Bremen" then Aufm=8;
	if AufmOther="Instagram uni Bremen" then Aufm=8;
	if AufmOther="Instagram von der Uni" then Aufm=8;
	if AufmOther="Instagramaccount der Uni Bremen" then Aufm=8;
	if AufmOther="Instagram-Account der Uni Bremen" then Aufm=8;
	if AufmOther="Instagramseite der Uni Bremen" then Aufm=8;
	if AufmOther="Internet" then Aufm=99;
	if AufmOther="Internetforum" then Aufm=99;
	if AufmOther="Linked in" then Aufm=9;
	if AufmOther="Linkedin" then Aufm=9;
	if AufmOther="LinkedIn" then Aufm=9;
	if AufmOther="linkedin" then Aufm=9;
	if AufmOther="LinkedIn." then Aufm=9;
	if AufmOther="Meine Frau Studiert in der Uni Bremen sie hat davon gehört." then Aufm=1;
	if AufmOther="meine Tochter" then Aufm=1;
	if AufmOther="Meinen Uni Server" then Aufm=7;
	if AufmOther="Online Schwarzes Brett Uni Oldenburg" then Aufm=7;
	if AufmOther="Onlineforum" then Aufm=99;
	if AufmOther="Online-Forum" then Aufm=99;
	if AufmOther="Plattform das schwarze brett leipzig" then Aufm=10;
	if AufmOther="Poll Pool" then Aufm=99;
	if AufmOther="Pollpool" then Aufm=99;
	if AufmOther="Post am schwarzen Brett der Uni" then Aufm=7;
	if AufmOther="Post auf Stud.IP" then Aufm=7;
	if AufmOther="Reddit" then Aufm=11;
	if AufmOther="Reddit /r/samplesize_de" then Aufm=11;
	if AufmOther="Reddit r/samplesize_DACH" then Aufm=11;
	if AufmOther="Schwarzes Brett" then Aufm=10;
	if AufmOther="Schwarzes Brett (online) meiner Universität" then Aufm=7;
	if AufmOther="Schwarzes Brett an der Uni" then Aufm=7;
	if AufmOther="Schwarzes Brett der Uni" then Aufm=7;
	if AufmOther="Schwarzes Brett der Uni Oldenburg" then Aufm=7;
	if AufmOther="Schwarzes Brett der Uni-Hannover" then Aufm=7;
	if AufmOther="Schwarzes Brett der Universität" then Aufm=7;
	if AufmOther="Schwarzes Brett in StudIP" then Aufm=7;
	if AufmOther="Schwarzes Brett online Uni Oldenburg" then Aufm=7;
	if AufmOther="Schwarzes Brett Stud.IP" then Aufm=7;
	if AufmOther="schwarzes Brett STUD.IP Bremen" then Aufm=7;
	if AufmOther="Schwarzes Brett StudIP" then Aufm=7;
	if AufmOther="schwarzes Brett Studip" then Aufm=7;
	if AufmOther="Schwarzes Brett StudIP Uni" then Aufm=7;
	if AufmOther="Schwarzes Brett Uni" then Aufm=7;
	if AufmOther="Schwarzes Brett Uni Bremen" then Aufm=7;
	if AufmOther="Schwarzes Brett Uni OL" then Aufm=7;
	if AufmOther="Schwarzes Brett Uni-Portal" then Aufm=7;
	if AufmOther="schwarzes brett uol" then Aufm=7;
	if AufmOther="Status WhatsApp einer Freundin" then Aufm=1; *oder Whatsapp???;
	if AufmOther="Stud IP" then Aufm=7;
	if AufmOther="stud ip" then Aufm=7;
	if AufmOther="Stud IP Uni Oldenburg" then Aufm=7;
	if AufmOther="stud.ip" then Aufm=7;
	if AufmOther="Stud.IP" then Aufm=7;
	if AufmOther="Stud.iP" then Aufm=7;
	if AufmOther="Stud.Ip - Schwarzes Brett" then Aufm=7;
	if AufmOther="Stud.IP (Uni Hannover)" then Aufm=7;
	if AufmOther="Stud.ip (Uni Oldenburg)" then Aufm=7;
	if AufmOther="Stud.IP Schwarzes Brett" then Aufm=7;
	if AufmOther="STUD.IP Schwarzes Brett" then Aufm=7;
	if AufmOther="Stud.IP schwarzes Brett" then Aufm=7;
	if AufmOther="Stud.IP Uni Hannover" then Aufm=7;
	if AufmOther="Stud.IP Uni Oldenburg" then Aufm=7;
	if AufmOther="StudIP" then Aufm=7;
	if AufmOther="Studip" then Aufm=7;
	if AufmOther="StudIP der LUH" then Aufm=7;
	if AufmOther="StudIP LUH" then Aufm=7;
	if AufmOther="StudIP schwarzes Brett" then Aufm=7;
	if AufmOther="StudIP Schwarzes Brett" then Aufm=7;
	if AufmOther="studIP Uni Bremen" then Aufm=7;
	if AufmOther="StudIP Uni Bremen" then Aufm=7;
	if AufmOther="studip uni bremen" then Aufm=7;
	if AufmOther="Studip Uni Hannover" then Aufm=7;
	if AufmOther="StudIP Uni Hannover" then Aufm=7;
	if AufmOther="StudIP Uni Oldenburg" then Aufm=7;
	if AufmOther="Studydrive" then Aufm=12;
	if AufmOther="StupIp Oldenburg" then Aufm=7;
	if AufmOther="Über die Uni-Mail" then Aufm=6;
	if AufmOther="über einen Facebooklink" then Aufm=3;
	if AufmOther="Über Instagram (Uni Bremen)" then Aufm=8;
	if AufmOther="Über Instagram Account der Uni Bremen" then Aufm=8;
	if AufmOther="über studip" then Aufm=7;
	if AufmOther="über Stud-IP der Uni Oldenburg (Schwarzes Brett dort)" then Aufm=7;
	if AufmOther="Über Wechat (Gruppe)" then Aufm=13;
	if AufmOther="Uni" then Aufm=7;
	if AufmOther="uni" then Aufm=7;
	if AufmOther="Uni Bremen" then Aufm=7;
	if AufmOther="Uni Bremen auf Instagram" then Aufm=8;
	if AufmOther="Uni Bremen Instagram" then Aufm=8;
	if AufmOther="Uni Bremen Instagramstory" then Aufm=8;
	if AufmOther="Uni Bremen Sozialen Medien" then Aufm=8;
	if AufmOther="Uni Bremen StudIP" then Aufm=7;
	if AufmOther="Uni Bremen Website" then Aufm=7;
	if AufmOther="Uni Email" then Aufm=6;
	if AufmOther="Uni Forum" then Aufm=7;
	if AufmOther="uni hannover" then Aufm=7;
	if AufmOther="Uni Hannover" then Aufm=7;
	if AufmOther="Uni Hinweis Instagram" then Aufm=8;
	if AufmOther="Uni Newsletter" then Aufm=6;
	if AufmOther="Uni Oldenburg" then Aufm=7;
	if AufmOther="uni schwarzes brett" then Aufm=7;
	if AufmOther="Uni Seite" then Aufm=7;
	if AufmOther="Uni StudIp" then Aufm=7;
	if AufmOther="Uni Website" then Aufm=7;
	if AufmOther="Uni/ Instagram" then Aufm=8;
	if AufmOther="Unii-Kassel-Seite" then Aufm=7;
	if AufmOther="uninetzwerk" then Aufm=7;
	if AufmOther="Uni-Portal" then Aufm=7;
	if AufmOther="Uniseite" then Aufm=7;
	if AufmOther="Universität" then Aufm=7;
	if AufmOther="Universität Bremen" then Aufm=7;
	if AufmOther="Universität Bremen Instagram Profil" then Aufm=8;
	if AufmOther="Universitäts Website" then Aufm=7;
	if AufmOther="Universitätsforum" then Aufm=7;
	if AufmOther="War in einem  Foren-Beitrag verlinkt" then Aufm=99;
	if AufmOther="Wechat" then Aufm=13;
	if AufmOther="wechat Gruppen" then Aufm=13;
	if AufmOther="www.dsble.de/793133" then Aufm=10;
	if AufmOther="zufällig Internet" then Aufm=99;
run;

/*Überprüfen wie viele sonstige im Datensatz verbleiben*/
proc freq data=results_work;
	tables SchulAb; 	*Im Datensatz verbleibt nur die Person mit Alter 99 und Schule: "Hundeschule" wird später noch aus den Daten gelöscht;
	tables BerufAb;		*Im Datensatz verbleibt nur die Person mit Alter 99 und BerufAb: "Erkenntnis" wird später noch aus den Daten gelöscht;
	tables fastand; 	*Keine Sonstigen mehr Vorhanden, da verwitwet eine neue 
						Kategorie bekommen hat und die 2 anderen Beobachtungen mit 99 nichts unter fastandother angegeben haben (->Missing);
	tables Aufm;		*47 Andere verbleiben im Datensatz ohne zuordnung zu bestehenden und neu erstellten Kategorien;
	format SchulAb SchulAb. BerufAb BerufAb. Fastand Fastand. Aufm Aufm.;
run;


/*Erstellen der Variable Pro/Con Impf um zu überprüfen wie viele je Gruppe den Fragebogen beendet haben (außerdem: Label Hinzufügen)*/
data results_work;
	set results_work;
	if ImpfZust=1 | ImpfMot=1 then ImpfEin=1; *Geimpft oder moechte sich Impfen;
	else if ImpfMot=2 then ImpfEin=2; *Moechte sich nicht Impfen lassen;
	else ImpfEin="Nicht angegeben";
	label ImpfEin = "Impfeinstellung" MigHin="Migrationshintergrund" PersoKenn="Bekannt mit Cov pos" KennErkr="Bekannt schwerer Verlauf"
	SoWahl="Sonntagswahlfrage" FaStand="Familienstand" PersHaus="Personen im Haushalt";
run;

proc freq data=results_work;
	tables ImpfEin*lastpage;
	format ImpfEin ImpfEin.;
run;

/*Uebprüfen der Besetzung der einzelnen (schon im Datensatz vorhanden) Kategorien und dies in Verbindung mit dem Outcome ImpfEin*/
Proc freq data=results_work;
	tables sex*ImpfEin MigHin*ImpfEin PersoKenn*ImpfEin KennErkr*ImpfEin SoWahl*ImpfEin FaStand*ImpfEin PersHaus*ImpfEin Eink*ImpfEin SchulAb*ImpfEin BerufAb*ImpfEin ErwerbKat*ImpfEin;
	format ImpfEin ImpfEin. sex sex. MigHin JN. PersoKenn JN. KennErkr JN. SoWahl SoWahl. FaStand FaStand. Eink Eink. Schulab SchulAb. BerufAb BerufAb. ErwerbKat ErwerbKat.;
run;

/*Hinzufügen von Beobachtungen zu Kategorien und löschen einzelner unrealistischer Beobachtungen*/
data results_work;
	set results_work;
	if age=99 then delete; *gelöscht aufgrund der Antworten im Freitextbereich, machen eine Manipulation ersichtlich;
run;
 
/*Export der bis hierhin erstellten Datei für alle die R nutzen*/
proc export data=work.results_work
    outfile="C:/Users/flori/OneDrive/Dokumente/1_Uni/Forschungsprojekt/Auswertung/Auswertungsdatei_Export_R.csv"
	dbms=csv replace;
run;

/***Erstellen von neuen Variablen***/
/*Kategorisierung des Alters*/
proc means data=results_work;
	var age;
run;

data results_kat;
	set results_work;
	if 		18<=age<=25 then 	age_kat=1;
	else if 25<age<=35 	then 	age_kat=2;
	else if 35<age<=45 	then 	age_kat=3;
	else if 45<age<=55 	then 	age_kat=4;
	else if 55<age<=65 	then 	age_kat=5;
	else if age>65 		then 	age_kat=6; *nur 6 Personen;
	else age_kat= .;
run;

proc freq data=results_kat;
	tables age*age_kat age_kat /missing;
run;

/*Kategorien Impfungen*/
proc freq data=results_kat;
	tables anamfern*ImpfEin;
run;
proc freq data=results_kat;
	tables anammas*ImpfEin;
run;


/***weitere Bivariate Analysen***/


/***Multivariate Analysen***/
