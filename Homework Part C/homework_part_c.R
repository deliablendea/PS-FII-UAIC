# C1.

# a)
permutare_aleatoare = function(n) {
  # Generam n valori uniforme aleatorii din intervalul [0, 1]
  U = runif(n, 0, 1)
  
  # Initializam vectorul de permutare cu indicii de la 1 la n
  permutare = 1:n
  
  # Sortam valorile din U si permutam indicii corespunzatori 
  for (i in 2:n) {
    key = U[i]
    key_index = permutare[i]
    j = i - 1
    
    while (j >= 1 && U[j] > key) {
      U[j + 1] = U[j]
      permutare[j + 1] = permutare[j]
      j = j - 1
    }
    
    U[j + 1] = key
    permutare[j + 1] = key_index
  }
  
  cat("Permutarea aleatorie arata astfel:", permutare)
}

permutare_aleatoare(10)

# ************************************************************************* #

# b)
generare_biti = function(n, k) {
  # Generam n siruri de biti de lungime k
  bit_strings = list()
  
  for (i in 1:n) {
    string = sample(c(0,1), k, replace = TRUE)
    bit_strings[[i]] = string
  }
  
  return(bit_strings)
}

comparare_lexicografica = function(Wi, Wj) {
  # Calculam lungimea minima a celor doua siruri
  Lij = min(length(Wi), length(Wj))
  
  # Comparam sirurile bit cu bit
  for (h in 1:Lij) {
    if (Wi[[h]] < Wj[[h]])
      return(TRUE) # Wi este strict mai mic decat Wj
    else if (Wi[[h]] > Wj[[h]])
      return(FALSE) # Wi nu este strict mai mic decat Wj
  }
  
  # Daca sirurile sunt egale pana la lungimea minima, adaugam aleatoriu biti
  while (TRUE) {
    if (length(Wi) < length(Wj))
      Wi = c(Wi, sample(c(0, 1), 1))
    else if (length(Wj) < length(Wi))
      Wj = c(Wj, sample(c(0, 1), 1))
    else {
      Wi = c(Wi, sample(c(0, 1), 1))
      Wj = c(Wj, sample(c(0, 1), 1))
    }
    
    if (Wi[[length(Wi)]] < Wj[[length(Wj)]])
      return(TRUE)
    else if (Wi[[length(Wi)]] > Wj[[length(Wj)]])
      return(FALSE)
  }
}

# Generam cuvinte
n = 6 ; k = 5
words = generare_biti(n, k)
words

# Comparam doua exemple
Wi = words[[4]] ; Wj = words[[6]]

# Rezultat
rezultat = comparare_lexicografica(Wi, Wj)
cat("Wi este lexicografic strict mai mic decat Wj:", rezultat, "\n")

# ************************************************************************* #

# c)
RandQuickSort = function(S) {
  if (length(S) <= 1)
    return(S)
  
  pivot_index = sample(1:length(S), 1)
  pivot = S[[pivot_index]]
  
  st = list() ; dr = list()
  
  for (i in 1:length(S))
    if (i != pivot_index) {
      if (comparare_lexicografica(S[[i]], pivot) == TRUE)
        st = c(st, list(S[[i]]))
      else
        dr = c(dr, list(S[[i]]))
    }
  
  sorted_st = RandQuickSort(st)
  sorted_dr = RandQuickSort(dr)
  
  return(c(sorted_st, list(pivot), sorted_dr))
}

# Generam n cuvinte de k biti
n = 6 ; k = 5
words = generare_biti(n, k)
words

# Afisam cele n cuvinte sortate lexicografic
RandQuickSort(words)

# ************************************************************************* #

# d)
cuvinte_egale = function(Wi, Wj) {
  # Calculam lungimea minima a celor doua siruri
  Lij = min(length(Wi), length(Wj))
  
  # Comparam sirurile bit cu bit
  for (i in 1:Lij)
    if (Wi[[i]] != Wj[[i]])
      return(FALSE)
  
  return(TRUE)
}

permutare_aleatoare_RQS = function(n, k) {
  # Generam n cuvinte de k biti
  words = generare_biti(n, k)
  
  # Sortam cele n cuvinte
  sorted_words = RandQuickSort(words)
  
  # Aflam indicii cuvintelor sortate
  indici = numeric(n)
  for (i in 1:length(sorted_words))
    for (j in 1:length(words))
      if (cuvinte_egale(sorted_words[[i]], words[[j]])) {
        indici[i] = j
        break
      }
  
  return(indici)
}

print(permutare_aleatoare_RQS(6, 5))

# ************************************************************************* #

# C2.

# a)
max_cut_random_algorithm = function(n) {
  # Initializam un graf bipartit cu 2n noduri și n*n muchii
  graph = matrix(0, nrow = n * 2, ncol = n * 2)
  
  for (i in 1:(n * 2))
    for (j in 1:(n * 2)) {
      # Adaugam muchii intre prima jumatate de noduri si cea de-a doua jumatate
      if (i <= n & j > n) 
        graph[i, j] = 1
      # Adaugam muchii intre a doua jumatate de noduri si prima jumatate
      else if (i > n & j <= n)
        graph[i, j] = 1
    }
  
  # Alegem aleatoriu n noduri pentru A si restul de n noduri formeaza B
  A = sample(1:(n * 2), n)
  B = setdiff(1:(n * 2), A)
  
  # Determinam muchiile din taietura
  cut_edges = matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) 
    for (j in 1:n) {
      # Obtinem muchiile din taietura
      if (graph[A[i], B[j]] == 1)
        cut_edges[i, j] = 1
    }
      
  cut_cardinality = sum(cut_edges)  # Calculam cardinalul taieturii
  
  return(list(A = A, B = B, cut_edges = cut_edges, cut_cardinality = cut_cardinality))
}

# b)
# Pentru a crește sansele de a gasi o taietura de cardinal maxim,
# putem rula algoritmul de mai multe ori si sa alegem taietura cu cardinalul maxim
increase_max_cut_chance = function(n, num_trials) {
  max_cut = NULL
  max_cut_cardinality = 0
  for (i in 1:num_trials) {
    result = max_cut_random_algorithm(n)
    if (result$cut_cardinality > max_cut_cardinality) {
      max_cut = result
      max_cut_cardinality = result$cut_cardinality
    }
  }
  return(max_cut)
}

# Testam algoritmul cu un numar de noduri n
n = 4
result = increase_max_cut_chance(n, 1)
result