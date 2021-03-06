### Problem 1
### W�rfeln im Casino

# Beobachtete Werte
x = c(rep(1,8),rep(2,9),rep(3,19),rep(4,6),rep(5,8),rep(6,10))
x
table(x)

# Klassischer Test: Chi-Quadrat Test
chisq.test(table(x))

# Chi-Quadrat Teststatistik berechnen
# Summe der quadrierten relativen Differenzen zwischen beobachteter und
# erwarteter H�ufigkeit
chisq_stat = function(x) {
  
  expected = length(x)/length(table(x))    # erwartet = (Anzahl Beobachtungen)/(Anzahl Auspr�gungen)
  
  result = 0                               # Zum Ergebnis wird jeweils die relative quadrierte 
                                           # Differenz f�r jede Auspr�gung addiert
  for (i in 1:length(table(x))) {
    result = result + (table(x)[i]-expected)^2/expected
  }
  
  return(result)
  
}

chisq_stat(x)

# Monte Carlo Simulation
# Werfe n_w�rfe mal einen W�rfel (in unserem Beispiel n_w�rfe = 60) mit einem
# fairen W�rfel und berechne die Chi-Quadrat Teststatistik
# Wiederhole das Experiment n mal (z.B. n=1000) und schaue in der Liste mit
# den Statistiken, wie h�ufig eine noch gr��ere Statistik als unsere beobachtete
# Teststatistik erscheint. Dies sind ja gerade die noch extremeren Ergebnisse, 
# die aber trotzdem mit einem fairen W�rfel zustande kamen
monte_carlo_chisq = function(n, n_w�rfe, original) {
  
  result = list()
  
  for (i in 1:n) {
    x = sample(1:6, size = n_w�rfe, replace = TRUE)
    result$distribution[i] = chisq_stat(x)
  }
  
  result$distribution = sort(result$distribution)
  
  result$p_value = sum(original < result$distribution)/n
  
  return(result)
}

monte_carlo_chisq(1000,60,10.6)

# Habe erst sp�ter gemerkt, dass die Funktion chisq.test das auch kann
chisq.test(table(x), simulate.p.value = TRUE, B = 1000)


###############################################################################

### Problem 2

# Erstellen der beiden Stichproben und der Teststatistik
A = c(23,17,8,12)
B = c(13,12,4,9,4,7,2,11,12)
test_stat = mean(A) - mean(B)
test_stat

# Klassisch: t-Test
t.test(A,B, alternative = 'two.sided', var.equal = FALSE)

# F�r die nachfolgenden Permutationstests nutzen wir die Implementierung im
# Paket "EnvStats"
library(EnvStats)

# Ein Permutationstest f�r die Nullhypothese "gleiche Mittelwerte" l�sst sich
# folgenderma�en ausf�hren:
# Beachte, dass "exact=TRUE" bedeutet, dass alle m�glichen Permutationen durchprobiert werden
# "alternative = two.sided" bedeutet, dass ein zweiseitiger Test ausgef�hrt wird,
# das kann auch mit "alternative=less" oder "alternative=greater" angepasst werden
test = twoSamplePermutationTestLocation(A, B, exact = TRUE, alternative = 'two.sided')
test

# Uns interessieren vor allem der p-Wert...
test$p.value

# ...oder die Verteilung unter der Nullhypothese
test$stat.dist

# Diese l�sst sich auch als Histogramm darstellen
# Wir zeichnen au�erdem in rot unsere originale Teststatistik ein
hist(test$stat.dist, breaks = 15, col = 'grey', main = "Permutation Distribution", xlab = '')
abline(v = test$statistic, lwd = 3, col = "red")

# Anmerkung: In diesem Fall kann man die exakte Verteilung schnell berechnen,
# da wir "nur" 715 m�gliche Permutationen betrachten m�ssen.
# Im Allgemeinen ist das nicht m�glich: Wir k�nnen dann einen "random permutation test"
# durchf�hren. Dazu f�gen wir "exact=FALSE" ein. Standardm��ig werden
# dann 1000 Permutationen berechnet. Mit "n.permutations=n" kann man diese Zahl �ndern
test_random = twoSamplePermutationTestLocation(A, B, exact = FALSE, alternative = 'two.sided',
                                        n.permutations = 500)

test_random$p.value
hist(test_random$stat.dist, breaks = 15, col = 'grey', main = "Permutation Distribution", xlab = '')
abline(v = test_random$statistic, lwd = 3, col = "red")


################################################################################

### Problem 3: Verbundene Stichproben

# Erstelle zwei Messreihen (vorher, nachher oder sowas)
Messreihe_1 = c(2,4,7,3,5,6,1,7,2)
Messreihe_2 = c(5,8,6,8,6,5,6,7,6)

# Wir nutzen wieder die Funktion aus EnvStats
# Um einen Permutationstest f�r die Nullhypothese "Differenzen sind 0 oder weniger"
# auszuf�hren, m�ssen wir nur "paired = TRUE" erg�nzen
test = twoSamplePermutationTestLocation(Messreihe_1, Messreihe_2,
                                        exact = TRUE, paired = TRUE, alternative = 'less')
test

# Uns interessieren vor allem der p-Wert...
test$p.value

# ...oder die Verteilung unter der Nullhypothese
test$stat.dist
hist(test$stat.dist, breaks = 20, col = 'grey', main = "Permutation Distribution", xlab = '')
abline(v = test$statistic, lwd = 3, col = "red")


################################################################################

### Problem 4: Unabh�ngigkeitstest

# Generieren eines Datensatzes mit Exposition und Krankheit
exposed = c(rep('exposed', 10),rep('not exposed',15))
sick = c(rep('krank',6),rep('nicht krank',4),rep('krank',3),rep('nicht krank',12))
data = data.frame(exposed,sick)
table(data)

# Permutationsbasierte Unabh�ngigkeitstest f�r kategorielle Daten
# liefert unter anderem das Paket "coin"
library(coin)

# Die Funktion "chisq_test" kann das Problem auf drei Arten l�sen:
# 1: Der Klassiker: Chi-Quadrat Test (asymptotisch)
chisq_test(table(data))

# 2: Permutationstest (exakt)
chisq_test(table(data), distribution = 'exact')

# F�r dieses Problem entspricht das dem exakten Test nach Fisher
# Das gilt aber nur f�r 2 x 2 Tafeln
fisher.test(table(data))

# 3: Random permutation test
chisq_test(table(data), distribution = approximate(nresample=1000))

# Anscheinend wird die Verteilung unter der Nullhypothese bei diesen Tests
# nicht gespeichert. Wir k�nnen das aber schnell selbst programmieren

# Erwartete Werte
expected = matrix(c(9*10/25, 16*10/25, 9*15/25, 16*15/25), ncol = 2, nrow = 2, byrow = TRUE)
expected

# Teststatistik berechnen
original = sum((table(data)-expected)^2/expected)
original

# Wir samplen jetzt 1000 mal und speichern die Teststatistik
n_permutations = 1000
distribution = c()
for(i in 1:n_permutations) {
  data = data.frame(exposed, sample(sick))
  distribution[i] = sum((as.matrix(table(data))-expected)^2/expected)
}

# Das ergibt die folgende (sortierte) Verteilung
sort(distribution)

# Der p-Wert ist der Anteil der Permutationen, die ein genauso oder noch
# extremeres Ereignis als die originale Teststatistik produziert haben
p_value = 1/n_permutations * sum(distribution >= original)
p_value

