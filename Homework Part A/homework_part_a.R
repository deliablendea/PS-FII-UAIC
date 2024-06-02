# A1.

# Valori aleatorii pentru lambda, p, n, m, k
lambda = 2.5; p = 0.3; n = 10; m = 8; k = 3

# a)
calcul_probabilitati = function(lambda, p, n, m, k) {
  interval = k:m
  
  poisson = dpois(interval, lambda)
  geometric = dgeom(interval, p)
  b = dbinom(interval, n, p)
  
  cat(" Poisson:", "\n", poisson, "\n\n", "Geometric:", "\n", geometric, "\n\n", "Binomial:", "\n", b, "\n\n")
}

calcul_probabilitati(lambda, p, n, m, k)

# b)
grafic_probabilitati = function(lambda, p, n, m, k) {
  interval = k:m
  
  # Setez layout-ul graficului pentru a afisa simultan 3 grafice
  par(mfrow = c(3, 1))
  
  # Calculam probabilitatile
  poisson = dpois(interval, lambda)
  geometric = dgeom(interval, p)
  b = dbinom(interval, n, p)
  
  # Grafic Poisson
  barplot(poisson, interval, main = 'Distributia Poisson', xlab = 'Valori', ylab = 'Probabilitati', col = 'lightcoral')
  
  # Grafic Geometric
  barplot(geometric, interval, main = 'Distributia Geometrica', xlab = 'Valori', ylab = 'Probabilitati', col = 'lightcoral')
  
  # Grafic Binomial
  barplot(b, interval, main = 'Distributia Binomiala', xlab = 'Valori', ylab = 'Probabilitati', col = 'lightcoral')
  
  # Resetam layout-ul graficului
  par(mfrow = c(1, 1))
}

grafic_probabilitati(lambda, p, n, m, k)

# c)
calcul_k0_minim = function(lambda) {
  k0 = 0
  suma_probabilitati = 0
  probabilitate = 1 - 10 ^ (- 6)
  
  # Iteram pentru a gasi cea mai mica valoare a lui k0 care satisface conditia
  while (suma_probabilitati <= probabilitate) {
    suma_probabilitati = suma_probabilitati + dpois(k0, lambda)
    if (suma_probabilitati <= probabilitate)
      k0 = k0 + 1
  }
  
  cat("Cea mai mica valoare a lui k0 care satisface conditia este:", k0, "\n")
}

calcul_k0_minim(lambda)

# ********************************************************************************************************************************************* #

# A2. 

# a)
frecvente_note_PS = function(file_name) {
  # Citim notele din fisierul specificat
  note = read.csv(file_name, header = TRUE, sep = ",")
  
  # Construim cele doua esantioane din coloanele corespunzatoare
  esantion_P = note$P
  esantion_S = note$S
  
  # Calculam frecventele absolute pentru fiecare esantion
  frecvente_absolute_P = as.vector(table(esantion_P))
  frecvente_absolute_S = as.vector(table(esantion_S))
  
  # Calculam frecventele relative pentru fiecare esantion
  frecvente_relative_P = frecvente_absolute_P / length(esantion_P)
  frecvente_relative_S = frecvente_absolute_S / length(esantion_S)
  
  # Calculam media pentru fiecare esantion
  media_P = mean(esantion_P)
  media_S = mean(esantion_S)
  
  # Rezultatele pentru esantionul P
  cat(" Frecventele absolute pentru esantionul P:\n", frecvente_absolute_P, "\n\n")
  cat(" Frecventele relative pentru esantionul P:\n", frecvente_relative_P, "\n\n")
  
  # Rezultatele pentru esantionul S
  cat(" Frecventele absolute pentru esantionul S:\n", frecvente_absolute_S, "\n\n")
  cat(" Frecventele relative pentru esantionul S:\n", frecvente_relative_S, "\n\n")
  
  # Mediile pentru fiecare esantion
  cat(" Media esantionului P:", media_P, "\n\n")
  cat(" Media esantionului S:", media_S, "\n\n")
}

frecvente_note_PS("note_PS.csv")

# b)
elimina_valori_aberante = function(file_name, esantion_name) {
  # Citim datele din fisierul specificat
  date = read.csv(file_name, header = TRUE, sep = ",")
  
  # Extragem esantionul specificat
  esantion = date[[esantion_name]]
  
  # Calculam media si deviatia standard a esantionului
  m = mean(esantion)
  s = sd(esantion)
  
  # Detectam valorile aberante si le eliminam din esantion
  outliers =  esantion_curatat = vector()
  j = k = 0
  for (i in 1:length(esantion)) {
    if (esantion[i] < m - 2 * s || esantion[i] > m + 2 * s) {
        j = j + 1
        outliers[j] = esantion[i]
    }
    else {
      k = k + 1
      esantion_curatat[k] = esantion[i]
    }
  }
  
  # Reprezentam grafic distributia frecventelor din esantionul curatat
  intervale = cut(esantion, breaks = seq(1, 10, by = 1))
  frecvente = table(intervale)
  barplot(frecvente, main = "Distributia frecventelor", xlab = "Intervale", ylab = "Frecventa", col = "lightcoral")
  
  # Returnam esantionul curatat
  return(esantion_curatat)
}

elimina_valori_aberante("note_PS.csv", "P")