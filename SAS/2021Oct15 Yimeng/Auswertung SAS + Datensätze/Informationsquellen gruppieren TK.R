#-------------------------Informationsquellen Gruppieren------------------------ 
#### Daten einlesen ####
getwd()
setwd("/Users/theresa/Desktop/Uni Kram Bremen/3. Semester/Forschungsprojekt/Datensatz")

COVIM=read.csv(file="/Users/theresa/Desktop/Uni Kram Bremen/3. Semester/Forschungsprojekt/Datensatz/Bereinigter_Datensatz_Florian.csv")
View(COVIM)
summary(COVIM)
dim(COVIM) 

# was machen wir mit Missings aus Infomationsquellen? 

# Informationsquellen (Variable >XXX2< nicht beachtet, weil print/online)

### /// Infoquelle = 1, Keine Infoquelle = 2 /// ###

#### Variable InfoZeit ####
COVIM$InfoZeit1 = ifelse(COVIM$InfoZeitDeu1 <= 2, 1, 2)
table(COVIM$InfoZeit1)
COVIM$InfoZeit2 = ifelse(COVIM$InfoZeitRe1 <= 2, 1, 2)
table(COVIM$InfoZeit2)
COVIM$InfoZeit3 = ifelse(COVIM$InfoZeitInt1 <= 2, 1, 2)
table(COVIM$InfoZeit3)
COVIM$InfoZeit4 = ifelse(COVIM$InfoZeitSchrift1 <= 2, 1, 2)
table(COVIM$InfoZeit4)

#Kreuztabellen aus den Variablen InfoZeit1, InfoZeit2, InfoZeit3, InfoZeit4
InfoZeit = xtabs (~ COVIM$InfoZeit1 + COVIM$InfoZeit2 + COVIM$InfoZeit3 + COVIM$InfoZeit4)
InfoZeit

### Scores bilden InfoZeitScore ### 
COVIM$InfoZeitScore = rowMeans(subset(COVIM, select = 
                      c(InfoZeitDeu1, InfoZeitRe1, InfoZeitInt1, InfoZeitSchrift1)), na.rm=TRUE)

View(COVIM$InfoZeitScore)
table(COVIM$InfoZeitScore)

range(COVIM$InfoZeitScore, na.rm = TRUE)

# Finale Kategorisierung aus dem Score der InfoZeit Variable 
COVIM$InfoZeitFinal = ifelse(COVIM$InfoZeitScore <= 2, 1, 2)
View(COVIM$InfoZeitFinal)
table(COVIM$InfoZeitFinal)


#### Variable InfoYP #### 
COVIM$InfoYP = ifelse(COVIM$InfoZeitBou1 <= 2, 1, 2)
table(COVIM$InfoYP)

View(COVIM$InfoYP) 
table(COVIM$InfoYP)

COVIM$InfoYP = as.numeric (COVIM$InfoZeitBou1)


#### Variable InfoWeb ####
COVIM$InfoWeb1 = ifelse(COVIM$InfoIntSu <= 2, 1, 2) 
table(COVIM$InfoWeb1)
COVIM$InfoWeb2 = ifelse(COVIM$InfoIntStell <= 2, 1, 2)
table(COVIM$InfoWeb2)

# Kreuztabelle aus den Variablen InfoWeb1, InfoWeb2
InfoWeb = xtabs (~ COVIM$InfoWeb1 + COVIM$InfoWeb2)
InfoWeb

### Score bilden InfoWebScore ###
COVIM$InfoWebScore = rowMeans(subset(COVIM, select = 
                                        c(InfoWeb1, InfoWeb2)), na.rm=TRUE)

View(COVIM$InfoWebScore)
table(COVIM$InfoWebScore)


#### Variable InfoneuMed ####
COVIM$InfoneuMed1 = ifelse(COVIM$InfoIntVid <= 2, 1, 2) 
table(COVIM$InfoneuMed1)
COVIM$InfoneuMed2 = ifelse(COVIM$InfoIntPod <= 2, 1, 2)
table(COVIM$InfoneuMed2)

# Kreuztabelle aus den Variablen InfoneuMed1, InfoneuMed2
InfoneuMed = xtabs (~ COVIM$InfoneuMed1 + COVIM$InfoneuMed2)
InfoneuMed

### Score bilden InfoneuMedScore ###
COVIM$InfoMedScore = rowMeans(subset(COVIM, select = 
                                       c(InfoneuMed1, InfoneuMed2)), na.rm=TRUE)

View(COVIM$InfoMedScore)
table(COVIM$InfoMedScore)


#### Variable InfoTV ####
COVIM$InfoTV1 = ifelse(COVIM$InfoIntTVrecht <= 2, 1, 2) 
table(COVIM$InfoTV1)
COVIM$InfoTV2 = ifelse(COVIM$InfoIntTVp <= 2, 1, 2)
table(COVIM$InfoTV2)

# Kreuztabelle aus den Variablen InfoneuMed1, InfoneuMed2
InfoTV = xtabs (~ COVIM$InfoTV1 + COVIM$InfoTV2)
InfoneuMed

### Score bilden InfoTVScore ###
COVIM$InfoTVScore = rowMeans(subset(COVIM, select = 
                                       c(InfoTV1, InfoTV2)), na.rm=TRUE)

View(COVIM$InfoTVScore)
table(COVIM$InfoTVScore)

#### Variable InfoIntAlt ####
COVIM$InfoIntAltFinal = ifelse(COVIM$InfoIntAlt <= 2, 1, 2) 
table(COVIM$InfoIntAltFinal)

COVIM$InfoIntAltScore = rowMeans(subset(COVIM, select = 
                                      c(InfoIntAlt1)), na.rm=TRUE)

View (COVIM$InfoIntAltScore)


#### Variable InfoSocMed #### 
COVIM$InfoSocMed1 = ifelse(COVIM$InfoIntInst <= 2, 1, 2) 
table(COVIM$InfoSocMed1)
COVIM$InfoSocMed2 = ifelse(COVIM$InfoIntFB <= 2, 1, 2)
table(COVIM$InfoSocMed2)
COVIM$InfoSocMed3 = ifelse(COVIM$InfoIntTwitt <= 2, 1, 2) 
table(COVIM$InfoSocMed3)
COVIM$InfoSocMed4 = ifelse(COVIM$InfoIntLinkX <= 2, 1, 2)
table(COVIM$InfoSocMed4)

# Kreuztabelle aus den Variablen InfoSocMed1, InfoSocMed2, InfoSocMed3, InfoSocMed4
InfoSocMed = xtabs (~ COVIM$InfoSocMed1  + COVIM$InfoSocMed2 + COVIM$InfoSocMed3 + COVIM$InfoSocMed4)
InfoSocMed

### Score bilden InfoSocMedScore ###
COVIM$InfoSocMedScore = rowMeans(subset(COVIM, select = 
                                      c(InfoSocMed1, InfoSocMed2, InfoSocMed3, InfoSocMed4)), 
                                      na.rm=TRUE)

View(COVIM$InfoSocMedScore)
table(COVIM$InfoSocMedScore)

# Finale Kategorisierung aus dem Score der InfoSocMed Variable 
COVIM$InfoSocMedFinal = ifelse(COVIM$InfoSocMedScore <= 1.5, 1, 2)
View(COVIM$InfoSocMedFinal)
table(COVIM$InfoSocMedFinal)


#### Variable InfoMess ####
COVIM$InfoMess1 = ifelse(COVIM$InfoIntSnap <= 2, 1, 2) 
table(COVIM$InfoMess1)
COVIM$InfoMess2 = ifelse(COVIM$InfoIntWhat <= 2, 1, 2)
table(COVIM$InfoMess2)
COVIM$InfoMess3 = ifelse(COVIM$InfoIntTel <= 2, 1, 2) 
table(COVIM$InfoMess3)
COVIM$InfoMess4 = ifelse(COVIM$InfoIntAnder <= 2, 1, 2)
table(COVIM$InfoMess4)

# Kreuztabelle aus den Variablen InfoSocMed1, InfoSocMed2, InfoSocMed3, InfoSocMed4
InfoMess = xtabs (~ COVIM$InfoMess1  + COVIM$InfoMess2 + COVIM$InfoMess3 + COVIM$InfoMess4)
InfoMess

### Score bilden InfoSocMedScore ###
COVIM$InfoMessScore = rowMeans(subset(COVIM, select = 
                                          c(InfoMess1, InfoMess2, InfoMess3, InfoMess4)), 
                                 na.rm=TRUE)

View(COVIM$InfoMessScore)
table(COVIM$InfoMessScore)

range(COVIM$InfoMessScore, na.rm = TRUE)

# Finale Kategorisierung aus dem Score der InfoSocMed Variable 
COVIM$InfoMessFinal = ifelse(COVIM$InfoMessScore <= 1.3, 1, 2)
View(COVIM$InfoMessFinal)
table(COVIM$InfoMessFinal)


### /// Wichtig = 1, Unwichtig = 2 /// ###

#### Variable InfoEigGut ####
COVIM$InfoEigGut1 = ifelse(COVIM$InfoEigAkt <= 6, 1, 2) 
table(COVIM$InfoEigGut1)
COVIM$InfoEigGut2 = ifelse(COVIM$InfoEigFach <= 6, 1, 2)
table(COVIM$InfoEigGut2)
COVIM$InfoEigGut3 = ifelse(COVIM$InfoEigVert <= 6, 1, 2) 
table(COVIM$InfoEigGut3)
COVIM$InfoEigGut4 = ifelse(COVIM$InfoEigInter <= 6, 1, 2)
table(COVIM$InfoEigGut4)

# Kreuztabelle aus den Variablen InfoEigGut1, InfoEigGut2, InfoEigGut3, InfoEigGut4
InfoEigGut = xtabs (~ COVIM$InfoEigGut1  + COVIM$InfoEigGut2 + COVIM$InfoEigGut3 + COVIM$InfoEigGut4)
InfoEigGut

### Score bilden InfoEigGutScore ###
COVIM$InfoEigGutScore = rowMeans(subset(COVIM, select = 
                                        c(InfoEigGut1, InfoEigGut2, InfoEigGut3, InfoEigGut4)), 
                               na.rm=TRUE)

View(COVIM$InfoEigGutScore)
table(COVIM$InfoEigGutScore)

# Finale Kategorisierung aus dem Score der InfoEigGut Variable 
COVIM$InfoEigGutFinal = ifelse(COVIM$InfoEigGutScore <= 1.5, 1, 2)
View(COVIM$InfoEigGutFinal)
table(COVIM$InfoEigGutFinal)

#### Variable InfoEigPerIn ####
COVIM$InfoEigPerIn1 = ifelse(COVIM$InfoEigPerIndi <= 6, 1, 2) 
table(COVIM$InfoEigPerIn1)
COVIM$InfoEigPerIn2 = ifelse(COVIM$InfoEigThem <= 6, 1, 2)
table(COVIM$InfoEigPerIn2) 
COVIM$InfoEigPerIn3 = ifelse(COVIM$InfoEigSprach <= 6, 1, 2) 
table(COVIM$InfoEigPerIn3)

# Kreuztabelle aus den Variablen InfoEigPerIn1, InfoEigPerIn2, InfoEigPerIn3
InfoEigPerIn = xtabs (~ COVIM$InfoEigPerIn1 + COVIM$InfoEigPerIn2 + COVIM$InfoEigPerIn3)
InfoEigPerIn

### Score bilden InfoEigPerInScore ###
COVIM$InfoEigPerInScore = rowMeans(subset(COVIM, select = 
                                          c(InfoEigPerIn1, InfoEigPerIn2, InfoEigPerIn3)), 
                                 na.rm=TRUE)

View(COVIM$InfoEigPerInScore)
table(COVIM$InfoEigPerInScore)

# Finale Kategorisierung aus dem Score der InfoEigPerIn Variable 
COVIM$InfoEigPerInFinal = ifelse(COVIM$InfoEigPerInScore <= 1.5, 1, 2)
View(COVIM$InfoEigPerInFinal)
table(COVIM$InfoEigPerInFinal)


### /// Oft = 1, Nie = 2 /// ###


#### Variable InfoFam ####
COVIM$InfoFam1 = ifelse(COVIM$Face2FaceFam <= 3, 1, 2) 
table(COVIM$InfoFam1)

### Score bilden InfoFamScore ###
COVIM$InfoFamScore = rowMeans(subset(COVIM, select = 
                                          c(InfoFam1)), na.rm=TRUE)

View (COVIM$InfoFamScore)
table (COVIM$InfoFamScore)

#### Variable InfoFach ####
COVIM$InfoFach1 = ifelse(COVIM$Face2FaceFachpers <= 3, 1, 2) 
table(COVIM$InfoFach1)

### Nicht notwendig: Score bilden InfoFamScore ###
COVIM$InfoFachScore = rowMeans(subset(COVIM, select = 
                                       c(InfoFach1)), na.rm=TRUE)

View (COVIM$InfoFachScore)
table (COVIM$InfoFachScore)

#### Variable ZeitInfo ####
COVIM$ZeitInfo1 = ifelse(COVIM$ZeitInfo <= 3, 1, 2) 
table(COVIM$ZeitInfo1)


