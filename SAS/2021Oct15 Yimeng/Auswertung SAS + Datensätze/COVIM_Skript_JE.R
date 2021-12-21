#	MANUSCRIPT:Datenauswertung Forschungsprojekt COVIM Epi Master WiSe 20/21

#       Welche Faktorn sind mit der Impfbereitschaft bei SARS-Cov-2 assoziiert?

#	Letztes ?nderungsdatum: 29/11/2021 - JE

  rm(list=ls(all=TRUE))

#### Set Working Directory ####

## Pfad zum Arbeitsordner festlegen
  setwd ("/Users/jetteechterhoff/Documents/R/COVIM") 
  getwd()

#### Packages ####

## install if missing

  #install.packages("dplyr")
  #install.packages("reshape")
  #install.packages("ggplot2")
  #install.packages("psych")

## load

  library (dplyr)
  library (reshape)
  library (ggplot2)
  library(psych)

## Vorhande / Installierte Pakete
  
  library()

#### Daten einlesen ####

    COVIM <- read.csv("Auswertungsdatei_Export_R.csv") # Bereinigter Datensatz von Flo


#	============================================================================================================================================
#	DATEN AUFBEREITUNG------------------------------------------------------------------------------------------------------------------
#	============================================================================================================================================

#### Allgemeine Informationen ?ber den Datensatz ####

  # Namen der Variablen, die in dem Datensatz enthalten sind
    names(COVIM)

  # Gr??e des Datensatzes
    dim(COVIM)

  # L?nge des Datensatzes
    length(COVIM)

  # Aufbau des Datensatzes
    str(COVIM)
    
  
#	============================================================================================================================================
#	DESKRIPTIVE STATISTIK 	#--------------------------------------------------------------------------------------------------------------
#	============================================================================================================================================

#### Soziodemografische Informationen ####
    
  ## Geschlecht ----

    COVIM$Sex
    table(COVIM$Sex)
  
  # Variablen umbenennen 
    
    COVIM$sex[COVIM$Sex=="1"]<-"maennlich" 
    COVIM$sex[COVIM$Sex=="2"]<-"weiblich"
    COVIM$sex[COVIM$Sex=="3"]<-"Divers"
    
    table(COVIM$sex)
    
    t.sex.abs <- table(COVIM$sex)
    t.sex.rel <- prop.table(t.sex.abs)
    t.sex.rel <- round(t.sex.rel, digits = 2)
    t.sex.abs.kum <- cumsum(t.sex.abs)
    t.sex.rel.kum <- cumsum(t.sex.rel)
    
    cbind(t.sex.abs, t.sex.rel, t.sex.abs.kum, t.sex.rel.kum)
    cbind(t.sex.abs, t.sex.rel)
    
    pie(table(COVIM$sex))
    barplot(table(COVIM$sex))
    
    
  ## Alter ----
    
  # Altersgruppen
    
    COVIM$age
    min(COVIM$age)
    max(COVIM$age)
    summary(COVIM$age)
    
    COVIM$age.cat <- cut(COVIM$age,
                              breaks = c(18, 25, 35, 45, 55, 73),
                              right = FALSE)
    COVIM[1:900, c('age', 'age.cat')] #Pr?fung, ob Einteilung richtig funktioniert hat
    
    
    summary(COVIM$age)
    sd(COVIM$age, na.rm = TRUE)
    
    cbind (age.abs = table(COVIM$age), 
           age.rel = round(prop.table(table(COVIM$age)), digits=2),
           age.abs.kum = cumsum(table(COVIM$age)),
           age.rel.kum = cumsum(round(prop.table(table(COVIM$age)), digits=2)))
    
    cbind (age.cat.abs = table(COVIM$age.cat), 
           age.cat.rel = round(prop.table(table(COVIM$age.cat)), digits=2),
           age.cat.abs.kum = cumsum(table(COVIM$age.cat)),
           age.cat.rel.kum = cumsum(round(prop.table(table(COVIM$age.cat)), digits=2)))
    
    pie(table(COVIM$age))
    barplot(table(COVIM$age))
    
    pie(table(COVIM$age.cat))
    barplot(table(COVIM$age.cat))
    
#### Corona-Teil ####

  ## Bekannte Covid-Erkrankung im Umfeld ----

    COVIM$PersoKenn
    
  # Umbenennen
    
    COVIM$PersoKenn2[COVIM$PersoKenn=="1"]<-"Ja"
    COVIM$PersoKenn2[COVIM$PersoKenn=="2"]<-"Nein"
  
  # Tabellarische Darstellung: Bekannte Covid-Erkrankung im Umfeld
    
    cbind (Bek.Cov.Erkr.Abs. = table(COVIM$PersoKenn2), 
            Bek.Cov.Erkr.Rel. = round(prop.table(table(COVIM$PersoKenn2)), digits=2))
  
    # Grafische Darstellung: Bekannte Covid-Erkrankung im Umfeld
    
      pie(table(COVIM$PersoKenn))
      barplot(table(COVIM$PersoKenn))

      
  ## BekCovErkr: Schwerer oder leichter Verlauf bei Covid-Erkranung im Umfeld ----
  
    COVIM$KennErkr

  # Umbennenen
  
    COVIM$KennErkr2[COVIM$KennErkr=="1"]<-"Schwerer Verlauf"
    COVIM$KennErkr2[COVIM$KennErkr=="2"]<-"Leichter Verlauf"
  
  # Tabellarische Darstellung: Leichter oder schwerer Verlauf der Covid-Erkrankung im Umfeld
    
    cbind (Abs. = table(COVIM$KennErkr2), 
           Rel. = round(prop.table(table(COVIM$KennErkr2)), digits=2))
  
  # Grafische Darstellung: Leichter oder schwerer Verlauf der Covid-Erkrankung im Umfeld
    
    pie(table(COVIM$KennErkr2))
    barplot(table(COVIM$KennErkr2))
  
  # Umbenennen und Zusammenfassen -> Erstellung der Variable: BekCovErkr
    
    COVIM$BekCovErkr[COVIM$PersoKenn2=="Ja"]<-"Person bekannt"
    COVIM$BekCovErkr[COVIM$PersoKenn2=="Nein"]<-"Keine Person bekannt"
    COVIM$BekCovErkr[COVIM$KennErkr2=="Schwerer Verlauf"]<-"Schwerer Verlauf"
    COVIM$BekCovErkr[COVIM$KennErkr2=="Leichter Verlauf"]<-"Leichter Verlauf"
  
  # Tabellarische Darstellung der Variable: BekCovErkr  
    
    cbind (BekCovErkr.Abs. = table(COVIM$BekCovErkr), 
          Bek.Cov.Erkr.Rel. = round(prop.table(table(COVIM$BekCovErkr)), digits=2))
  
    
  ## Impfstatus ----

  # Impfung Ja/Nein
    
    COVIM$ImpfZust
    
    # Umbenennen
    
      COVIM$ImpfZust2[COVIM$ImpfZust=="1"]<-"Ja"
      COVIM$ImpfZust2[COVIM$ImpfZust=="2"]<-"Nein"
    
    # Tabellarische Darstellung: Impfung Ja/Nein
    
      cbind (Impfung.Abs. = table(COVIM$ImpfZust2), 
           Impfung.Rel. = round(prop.table(table(COVIM$ImpfZust2)), digits=2))
    
    # Grafische Darstellung: Impfung Ja/Nein
    
      pie(table(COVIM$ImpfZust))
      barplot(table(COVIM$ImpfZust))
      
  # 1/2 Impfungen, vollständig geimpft
      
      COVIM$ImpDos
      
      # Umbennen und Zusammenfassen -> Erstellung der Variable "Impfstatus"
      
        # 1       -> 1/2 Impfungen
        # 2, 3, 4 -> Vollständig geimpft
        # "Nein"  -> Keine Impfung (Bei ImpfZust)
      
          COVIM$Impfstatus[COVIM$ImpDos=="1"]<-       "2    1/2 Impfungen"
          COVIM$Impfstatus[COVIM$ImpDos=="2"]<-       "3    Vollständig Geimpft"
          COVIM$Impfstatus[COVIM$ImpDos=="3"]<-       "3    Vollständig Geimpft"
          COVIM$Impfstatus[COVIM$ImpDos=="4"]<-       "3    Vollständig Geimpft"
          COVIM$Impfstatus[COVIM$ImpfZust2=="Nein"]<-  "1    Keine Impfung"
          
          # Tabellarische Darstellung der Variable: Impfstatus
          
          cbind (Impfstatus.Abs. = table(COVIM$Impfstatus), 
                 Impfstatus.Rel. = round(prop.table(table(COVIM$Impfstatus)), digits=3))

          
  ## Impfstoff ----
  
  # Umbennen und Zusammenfassen -> Erstellung der Variable "Impfstoff"

    # Johnson & Johnson             -> ImpDos               = 2
    #                               -> BevorImpStoff        = 3
          
    # AstraZeneca                   -> Impfstofferst        = 1
    #                               -> Impfstoffzweit       = 1
    #                               -> BevorImpStoff        = 1
          
    # Biontech                      -> Impfstofferst        = 2 
    #                               -> Impfstoffzweit       = 2
    #                               -> BevorImpStoff        = 2
  
    # Moderna                       -> Impfstofferst        = 3
    #                               -> Impfstoffzweit       = 3
    #                               -> BevorImpStoff        = 4
    
    # Kein bevorzugter Impfstoff    -> BevorImpStoff        = 5
  
    # -oth-                         -> ImpfStofferstother   = Staatliche Pharma Zhongsheng
    #                               -> ImpfStofferstother   = sinovac
  
    # -oth-                         -> ImpfStoffzweitother  = sinovac
    
          
        COVIM$Impfstoff[COVIM$ImpDos=="2"]<-          "1    Johnson & Johnson"
        COVIM$Impfstoff[COVIM$ImpfStofferst=="1"]<-   "2    Erstimpfung:    AstraZeneca"
        COVIM$Impfstoff[COVIM$ImpStoffzweit=="1"]<-   "7    Zweitimpfung:   AstraZeneca"
        COVIM$Impfstoff[COVIM$ImpfStofferst=="2"]<-   "3    Erstimpfung:    Biontech"
        COVIM$Impfstoff[COVIM$ImpStoffzweit=="2"]<-   "8    Zweitimpfung:   Biontech"
        COVIM$Impfstoff[COVIM$ImpfStofferst=="3"]<-   "4    Erstimpfung:    Moderna"
        COVIM$Impfstoff[COVIM$ImpStoffzweit=="3"]<-   "9    Zweitimpfung:   Moderna"
        
        #COVIM$Impfstoff[COVIM$BevorImpStoff1=="1"]<-"Bevorzugter Impfstoff: AstraZeneca"
        #COVIM$Impfstoff[COVIM$BevorImpStoff2=="2"]<-"Bevorzugter Impfstoff: Biontech"
        #COVIM$Impfstoff[COVIM$BevorImpStoff3=="3"]<-"Bevorzugter Impfstoff: Johnson & Johnson"
        #COVIM$Impfstoff[COVIM$BevorImpStoff4=="4"]<-"Bevorzugter Impfstoff: Moderna"
        #COVIM$Impfstoff[COVIM$BevorImpStoff5=="5"]<-"Kein bevorzugter Impfstoff"
    
        COVIM$Impfstoff[COVIM$ImpfStofferstother=="Staatliche Pharma Zhongsheng"]<-"6    Erstimpfung:    Staatliche Pharma Zhongsheng"
        COVIM$Impfstoff[COVIM$ImpfStofferstother=="sinovac"]<-"5    Erstimpfung:    Sinovac"
        COVIM$Impfstoff[COVIM$ImpfStoffzweitother=="sinovac"]<-"10     Zweitimpfung:  Sinovac"
    
        cbind (Impfstatus.Abs. = table(COVIM$Impfstoff), 
            Impfstatus.Rel. = round(prop.table(table(COVIM$Impfstoff)), digits=7))
   
             
  ## Impfmotivation Positiv: Gründe ----
     
    # Grund: Beruf ----
        
    COVIM$ImpGrundberuf
    
    cbind (Impfgrundberuf.Abs. = table(COVIM$ImpGrundberuf), 
               Impfgrundberuf.Rel. = round(prop.table(table(COVIM$ImpGrundberuf)), digits=4))
      
      # Umbennen
        
        COVIM$Impfgrundberuf[COVIM$ImpGrundberuf=="1"]<-"1    Trifft zu"
        COVIM$Impfgrundberuf[COVIM$ImpGrundberuf=="2"]<-"2    Trifft eher zu"
        COVIM$Impfgrundberuf[COVIM$ImpGrundberuf=="4"]<-"3    Trifft eher nicht zu"
        COVIM$Impfgrundberuf[COVIM$ImpGrundberuf=="5"]<-"4    Trifft nicht zu"
        
    cbind (Impfgrundberuf.Abs. = table(COVIM$Impfgrundberuf), 
            Impfgrundberuf.Rel. = round(prop.table(table(COVIM$Impfgrundberuf)), digits=4))

    
    # Grund: Risiko Corona-Infektion ----
    
    COVIM$ImpGrundrisiko
    
    cbind (Impfgrundrisiko.Abs. = table(COVIM$ImpGrundrisiko), 
           Impfgrundrisiko.Rel. = round(prop.table(table(COVIM$ImpGrundrisiko)), digits=4))
    
    # Umbenennen
    
    COVIM$Impfgrundrisiko[COVIM$ImpGrundrisiko=="1"]<-"1    Trifft zu"
    COVIM$Impfgrundrisiko[COVIM$ImpGrundrisiko=="2"]<-"2    Trifft eher zu"
    COVIM$Impfgrundrisiko[COVIM$ImpGrundrisiko=="4"]<-"3    Trifft eher nicht zu"
    COVIM$Impfgrundrisiko[COVIM$ImpGrundrisiko=="5"]<-"4    Trifft nicht zu"    
    
    cbind (Impfgrundrisiko.Abs. = table(COVIM$Impfgrundrisiko), 
           Impfgrundrisiko.Rel. = round(prop.table(table(COVIM$Impfgrundrisiko)), digits=4))    

    
    # Grund: Alltag ----
     
    COVIM$ImpGrundnorm

    cbind (Impfgrundalltag.Abs. = table(COVIM$ImpGrundnorm), 
           Impfgrundalltag.Rel. = round(prop.table(table(COVIM$ImpGrundnorm)), digits=4))
    
    # Umbennenen
    
    COVIM$Impfgrundalltag[COVIM$ImpGrundnorm=="1"]<-"1    Trifft zu"
    COVIM$Impfgrundalltag[COVIM$ImpGrundnorm=="2"]<-"2    Trifft eher zu"
    COVIM$Impfgrundalltag[COVIM$ImpGrundnorm=="4"]<-"3    Trifft eher nicht zu"
    COVIM$Impfgrundalltag[COVIM$ImpGrundnorm=="5"]<-"4    Trifft nicht zu"    
    
    cbind (Impfgrundalltag.Abs. = table(COVIM$Impfgrundalltag), 
           Impfgrundalltag.Rel. = round(prop.table(table(COVIM$Impfgrundalltag)), digits=4))

    
    # Grund: Gesundheit Familie ----
    
    COVIM$ImpGrundgesfam
    
    cbind (Impfgrundgesfam.Abs. = table(COVIM$ImpGrundgesfam), 
           Impfgrundgesfam.Rel. = round(prop.table(table(COVIM$ImpGrundgesfam)), digits=4))
    
    # Umbennenen
    
    COVIM$Impfgrundgesfam[COVIM$ImpGrundgesfam=="1"]<-"1  Trifft zu"
    COVIM$Impfgrundgesfam[COVIM$ImpGrundgesfam=="2"]<-"2  Trifft eher zu"
    COVIM$Impfgrundgesfam[COVIM$ImpGrundgesfam=="4"]<-"3  Trifft eher nicht zu"
    COVIM$Impfgrundgesfam[COVIM$ImpGrundgesfam=="5"]<-"4  Trifft nicht zu"    
    
    cbind (Impfgrundgesfam.Abs. = table(COVIM$Impfgrundgesfam), 
           Impfgrundgesfam.Rel. = round(prop.table(table(COVIM$Impfgrundgesfam)), digits=4))

    
    # Grund: Soziales Umfeld ----
    
    COVIM$ImpGrundsozum
    
    cbind (Impfgrundsozumf.Abs. = table(COVIM$ImpGrundsozum), 
           Impfgrundsozumf.Rel. = round(prop.table(table(COVIM$ImpGrundsozum)), digits=4))
    
      # Umbennenen
    
      COVIM$Impfgrundsozumf[COVIM$ImpGrundsozum=="1"]<-"1  Trifft zu"
      COVIM$Impfgrundsozumf[COVIM$ImpGrundsozum=="2"]<-"2  Trifft eher zu"
      COVIM$Impfgrundsozumf[COVIM$ImpGrundsozum=="4"]<-"3  Trifft eher nicht zu"
      COVIM$Impfgrundsozumf[COVIM$ImpGrundsozum=="5"]<-"4  Trifft nicht zu"    
    
    cbind (Impfgrundsozumf.Abs. = table(COVIM$Impfgrundsozumf), 
           Impfgrundsozumf.Rel. = round(prop.table(table(COVIM$Impfgrundsozumf)), digits=4))

    
    # Grund: Gesellschaft ----
    
    COVIM$ImpGrundgesell
    
    cbind (Impfgrundgesell.Abs. = table(COVIM$ImpGrundgesell), 
           Impfgrundgesell.Rel. = round(prop.table(table(COVIM$ImpGrundgesell)), digits=4))
    
      # Umbennenen
    
      COVIM$Impfgrundgesell[COVIM$ImpGrundgesell=="1"]<-"1  Trifft zu"
      COVIM$Impfgrundgesell[COVIM$ImpGrundgesell=="2"]<-"2  Trifft eher zu"
      COVIM$Impfgrundgesell[COVIM$ImpGrundgesell=="4"]<-"3  Trifft eher nicht zu"
      COVIM$Impfgrundgesell[COVIM$ImpGrundgesell=="5"]<-"4  Trifft nicht zu"    
    
    cbind (Impfgrundgesell.Abs. = table(COVIM$Impfgrundgesell), 
           Impfgrundgesell.Rel. = round(prop.table(table(COVIM$Impfgrundgesell)), digits=4))

    
    # Grund: Gesundheitswesen entlasten ----
    
    COVIM$ImpGrundgeswesent
    
    cbind (ImpGrundgeswesent.Abs. = table(COVIM$ImpGrundgeswesent), 
           ImpGrundgeswesent.Rel. = round(prop.table(table(COVIM$ImpGrundgeswesent)), digits=4))
    
      # Umbennenen
    
      COVIM$Impfgrundgeswes[COVIM$ImpGrundgeswesent=="1"]<-"1  Trifft zu"
      COVIM$Impfgrundgeswes[COVIM$ImpGrundgeswesent=="2"]<-"2  Trifft eher zu"
      COVIM$Impfgrundgeswes[COVIM$ImpGrundgeswesent=="4"]<-"3  Trifft eher nicht zu"
      COVIM$Impfgrundgeswes[COVIM$ImpGrundgeswesent=="5"]<-"4  Trifft nicht zu"    
    
    cbind (Impfgrundgeswes.Abs. = table(COVIM$Impfgrundgeswes), 
           Impfgrundgeswes.Rel. = round(prop.table(table(COVIM$Impfgrundgeswes)), digits=4))
  
  
    # Grund: Andere ----
    
    COVIM$WeiGruFuer


  ## Impfmotivation negativ: Gründe ----
 
    # Grund: Gewünschter Impfstoff wurde nicht angeboten ----
    
    COVIM$GruNichtImpfGImpf
    
    cbind (Impfmotnegimpfstoff.Abs. = table(COVIM$GruNichtImpfGImpf), 
           Impfmotnegimpfstoff.Rel. = round(prop.table(table(COVIM$GruNichtImpfGImpf)), digits=2))
    
      # Umbennenen
    
      COVIM$Impfmotnegimpfstoff[COVIM$GruNichtImpfGImpf=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegimpfstoff[COVIM$GruNichtImpfGImpf=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegimpfstoff.Abs. = table(COVIM$Impfmotnegimpfstoff), 
           Impfmotnegimpfstoff.Rel. = round(prop.table(table(COVIM$Impfmotnegimpfstoff)), digits=2))

  
    # Grund: Schwanger ----
    
    COVIM$GruNichtImpfSchwang
    
    cbind (Impfmotnegschwanger.Abs. = table(COVIM$GruNichtImpfSchwang), 
           Impfmotnegschwanger.Rel. = round(prop.table(table(COVIM$GruNichtImpfSchwang)), digits=2))
    
    # Umbennenen
    
    COVIM$Impfmotnegschwanger[COVIM$GruNichtImpfSchwang=="1"]<-"1  Trifft zu"
    COVIM$Impfmotnegschwanger[COVIM$GruNichtImpfSchwang=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegschwanger.Abs. = table(COVIM$Impfmotnegschwanger), 
           Impfmotnegschwanger.Rel. = round(prop.table(table(COVIM$Impfmotnegschwanger)), digits=2))
    
  
    # Grund: Schlechter Gesundheitszustand ----
    
    COVIM$GruNichtImpfSchlechGes
    
    cbind (Impfmotnegschlges.Abs. = table(COVIM$GruNichtImpfSchlechGes), 
           Impfmotnegschlges.Rel. = round(prop.table(table(COVIM$GruNichtImpfSchlechGes)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegschlges[COVIM$GruNichtImpfSchlechGes=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegschlges[COVIM$GruNichtImpfSchlechGes=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegschlges.Abs. = table(COVIM$Impfmotnegschlges), 
           Impfmotnegschlges.Rel. = round(prop.table(table(COVIM$Impfmotnegschlges)), digits=2)) 
    
    
    # Grund: Allergische Reaktion ----
    
    COVIM$GruNichtImpfAllerg
    
    cbind (Impfmotnegallerg.Abs. = table(COVIM$GruNichtImpfAllerg), 
           Impfmotnegallerg.Rel. = round(prop.table(table(COVIM$GruNichtImpfAllerg)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegallerg[COVIM$GruNichtImpfAllerg=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegallerg[COVIM$GruNichtImpfAllerg=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegallerg.Abs. = table(COVIM$Impfmotnegallerg), 
           Impfmotnegallerg.Rel. = round(prop.table(table(COVIM$Impfmotnegallerg)), digits=2)) 
  
  
    # Grund: Angst vor Nebenwirkungen ----
    
    COVIM$GruNichtImpfAngst
    
    cbind (Impfmotnegallerg.Abs. = table(COVIM$GruNichtImpfAngst), 
           Impfmotnegallerg.Rel. = round(prop.table(table(COVIM$GruNichtImpfAngst)), digits=2))
    
    # Umbennenen
    
      COVIM$ImpfmotnegangstNW[COVIM$GruNichtImpfAngst=="1"]<-"1  Trifft zu"
      COVIM$ImpfmotnegangstNW[COVIM$GruNichtImpfAngst=="2"]<-"2  Trifft nicht zu"    
    
    cbind (ImpfmotnegangstNW.Abs. = table(COVIM$ImpfmotnegangstNW), 
           ImpfmotnegangstNW.Rel. = round(prop.table(table(COVIM$ImpfmotnegangstNW)), digits=2)) 
  
    
    # Grund: Zweifel an der Sicherheit des Impfstoffs ----
    
    COVIM$GruNichtImpfZweif
    
    cbind (Impfmotnegzwimpfstoff.Abs. = table(COVIM$GruNichtImpfZweif), 
           Impfmotnegzwimpfstoff.Rel. = round(prop.table(table(COVIM$GruNichtImpfZweif)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegzwimpfstoff[COVIM$GruNichtImpfZweif=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegzwimpfstoff[COVIM$GruNichtImpfZweif=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegzwimpfstoff.Abs. = table(COVIM$Impfmotnegzwimpfstoff), 
           Impfmotnegzwimpfstoff.Rel. = round(prop.table(table(COVIM$Impfmotnegzwimpfstoff)), digits=2)) 

    
    # Grund: In letzter Zeit eine andere Impfung verabreicht bekommen ----
    
    COVIM$GruNichtImpfAndImpf
    
    cbind (Impfmotnegandimpfung.Abs. = table(COVIM$GruNichtImpfAndImpf), 
           Impfmotnegandimpfung.Rel. = round(prop.table(table(COVIM$GruNichtImpfAndImpf)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegandimpfung[COVIM$GruNichtImpfAndImpf=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegandimpfung[COVIM$GruNichtImpfAndImpf=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegandimpfung.Abs. = table(COVIM$Impfmotnegandimpfung), 
           Impfmotnegandimpfung.Rel. = round(prop.table(table(COVIM$Impfmotnegandimpfung)), digits=2))

    
    # Grund: Zweifel an der Wirksamkeit / der Effektivität des Impfstoffs ----
    
    COVIM$GruNichtImpfNiWir
    
    cbind (Impfmotnegwirks.Abs. = table(COVIM$GruNichtImpfNiWir), 
           Impfmotnegwirks.Rel. = round(prop.table(table(COVIM$GruNichtImpfNiWir)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegwirks[COVIM$GruNichtImpfNiWir=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegwirks[COVIM$GruNichtImpfNiWir=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegwirks.Abs. = table(COVIM$Impfmotnegwirks), 
           Impfmotnegwirks.Rel. = round(prop.table(table(COVIM$Impfmotnegwirks)), digits=2))
  
    
    # Grund: Einschätzung des Gesundheitszustands (GZ) als so gut, dass keine Impfung benötigt wird ----
    
    COVIM$GruNichtImpfGutGes
    
    cbind (Impfmotneggutges.Abs. = table(COVIM$GruNichtImpfGutGes), 
           Impfmotneggutges.Rel. = round(prop.table(table(COVIM$GruNichtImpfGutGes)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotneggutges[COVIM$GruNichtImpfGutGes=="1"]<-"1  Trifft zu"
      COVIM$Impfmotneggutges[COVIM$GruNichtImpfGutGes=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotneggutges.Abs. = table(COVIM$Impfmotneggutges), 
           Impfmotneggutges.Rel. = round(prop.table(table(COVIM$Impfmotneggutges)), digits=2))


    # Grund: Aufwand einen Termin zu erhalten ist / war zu hoch ----
    
    COVIM$GruNichtImpfAufImpf
    
    cbind (Impfmotnegaufwtermin.Abs. = table(COVIM$GruNichtImpfAufImpf), 
           Impfmotnegaufwtermin.Rel. = round(prop.table(table(COVIM$GruNichtImpfAufImpf)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegaufwtermin[COVIM$GruNichtImpfAufImpf=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegaufwtermin[COVIM$GruNichtImpfAufImpf=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegaufwtermin.Abs. = table(COVIM$Impfmotnegaufwtermin), 
           Impfmotnegaufwtermin.Rel. = round(prop.table(table(COVIM$Impfmotnegaufwtermin)), digits=2))
  

    # Grund: Verunsicherung, da die Berichterstattung über Corona als Unzureichend empfunden wird ----
    
    COVIM$GruNichtImpfBerErst
    
    cbind (Impfmotnegbererst.Abs. = table(COVIM$GruNichtImpfBerErst), 
           Impfmotnegbererst.Rel. = round(prop.table(table(COVIM$GruNichtImpfBerErst)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegbererst[COVIM$GruNichtImpfBerErst=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegbererst[COVIM$GruNichtImpfBerErst=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegbererst.Abs. = table(COVIM$Impfmotnegbererst), 
           Impfmotnegbererst.Rel. = round(prop.table(table(COVIM$Impfmotnegbererst)), digits=2))

    
    # Grund: Umfeld hat überzeugt, sich nicht gegen Corona impfen zu lassen ----
    
    COVIM$GruNichtImpfAngUmf
    
    cbind (Impfmotnegangumf.Abs. = table(COVIM$GruNichtImpfAngUmf), 
           Impfmotnegangumf.Rel. = round(prop.table(table(COVIM$GruNichtImpfAngUmf)), digits=2))
    
    # Umbennenen
    
      COVIM$Impfmotnegangumf[COVIM$GruNichtImpfAngUmf=="1"]<-"1  Trifft zu"
      COVIM$Impfmotnegangumf[COVIM$GruNichtImpfAngUmf=="2"]<-"2  Trifft nicht zu"    
    
    cbind (Impfmotnegangumf.Abs. = table(COVIM$Impfmotnegangumf), 
           Impfmotnegangumf.Rel. = round(prop.table(table(COVIM$Impfmotnegangumf)), digits=2))
    
 
    # Grund: Andere (Freitext) ----
  
    COVIM$GruNichtImpfother
    COVIM$WeiGruGe
    
    
    