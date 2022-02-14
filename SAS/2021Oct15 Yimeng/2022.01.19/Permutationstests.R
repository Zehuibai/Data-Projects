### Problem 1
### Würfeln im Casino

# Beobachtete Werte
x = c(rep(1,8),rep(2,9),rep(3,19),rep(4,6),rep(5,8),rep(6,10))
x
table(x)

# Klassischer Test: Chi-Quadrat Test
chisq.test(table(x))

# Chi-Quadrat Teststatistik berechnen
# Summe der quadrierten relativen Differenzen zwischen beobachteter und
# erwarteter Häufigkeit
chisq_stat = function(x) {
  
  expected = length(x)/length(table(x))    # erwartet = (Anzahl Beobachtungen)/(Anzahl Ausprägungen)
  
  result = 0                               # Zum Ergebnis wird jeweils die relative quadrierte 
                                           # Differenz für jede Ausprägung addiert
  for (i in 1:length(table(x))) {
    result = result + (table(x)[i]-expected)^2/expected
  }
  
  return(result)
  
}

chisq_stat(x)

# Monte Carlo Simulation
# Werfe n_würfe mal einen Würfel (in unserem Beispiel n_würfe = 60) mit einem
# fairen Würfel und berechne die Chi-Quadrat Teststatistik
# Wiederhole das Experiment n mal (z.B. n=1000) und schaue in der Liste mit
# den Statistiken, wie häufig eine noch größere Statistik als unsere beobachtete
# Teststatistik erscheint. Dies sind ja gerade die noch extremeren Ergebnisse, 
# die aber trotzdem mit einem fairen Würfel zustande kamen
monte_carlo_chisq = function(n, n_würfe, original) {
  
  result = list()
  
  for (i in 1:n) {
    x = sample(1:6, size = n_würfe, replace = TRUE)
    result$distribution[i] = chisq_stat(x)
  }
  
  result$distribution = sort(result$distribution)
  
  result$p_value = sum(original < result$distribution)/n
  
  return(result)
}

monte_carlo_chisq(1000,60,10.6)

# Habe erst später gemerkt, dass die Funktion chisq.test das auch kann
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

# Für die nachfolgenden Permutationstests nutzen wir die Implementierung im
# Paket "EnvStats"
library(EnvStats)

# Ein Permutationstest für die Nullhypothese "gleiche Mittelwerte" lässt sich
# folgendermaßen ausführen:
# Beachte, dass "exact=TRUE" bedeutet, dass alle möglichen Permutationen durchprobiert werden
# "alternative = two.sided" bedeutet, dass ein zweiseitiger Test ausgeführt wird,
# das kann auch mit "alternative=less" oder "alternative=greater" angepasst werden
test = twoSamplePermutationTestLocation(A, B, exact = TRUE, alternative = 'two.sided')
test

# Uns interessieren vor allem der p-Wert...
test$p.value

# ...oder die Verteilung unter der Nullhypothese
test$stat.dist

# Diese lässt sich auch als Histogramm darstellen
# Wir zeichnen außerdem in rot unsere originale Teststatistik ein
hist(test$stat.dist, breaks = 15, col = 'grey', main = "Permutation Distribution", xlab = '')
abline(v = test$statistic, lwd = 3, col = "red")

# Anmerkung: In diesem Fall kann man die exakte Verteilung schnell berechnen,
# da wir "nur" 715 mögliche Permutationen betrachten müssen.
# Im Allgemeinen ist das nicht möglich: Wir können dann einen "random permutation test"
# durchführen. Dazu fügen wir "exact=FALSE" ein. Standardmäßig werden
# dann 1000 Permutationen berechnet. Mit "n.permutations=n" kann man diese Zahl ändern
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
# Um einen Permutationstest für die Nullhypothese "Differenzen sind 0 oder weniger"
# auszuführen, müssen wir nur "paired = TRUE" ergänzen
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

### Problem 4: Unabhängigkeitstest

# Generieren eines Datensatzes mit Exposition und Krankheit
exposed = c(rep('exposed', 10),rep('not exposed',15))
sick = c(rep('krank',6),rep('nicht krank',4),rep('krank',3),rep('nicht krank',12))
data = data.frame(exposed,sick)
table(data)

# Permutationsbasierte Unabhängigkeitstest für kategorielle Daten
# liefert unter anderem das Paket "coin"
library(coin)

# Die Funktion "chisq_test" kann das Problem auf drei Arten lösen:
# 1: Der Klassiker: Chi-Quadrat Test (asymptotisch)
chisq_test(table(data))

# 2: Permutationstest (exakt)
chisq_test(table(data), distribution = 'exact')

# Für dieses Problem entspricht das dem exakten Test nach Fisher
# Das gilt aber nur für 2 x 2 Tafeln
fisher.test(table(data))

# 3: Random permutation test
chisq_test(table(data), distribution = approximate(nresample=1000))

# Anscheinend wird die Verteilung unter der Nullhypothese bei diesen Tests
# nicht gespeichert. Wir können das aber schnell selbst programmieren

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

