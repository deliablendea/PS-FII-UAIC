# B1

estimare_volum_tor_circular = function(R, r, esantion) {
  # Initializam numarul de puncte in interiorul torului
  N_C = 0
  
  # Definim limitele cubului care contine torul
  x1_min = -R - r; x1_max = R + r
  x2_min = -R - r; x2_max = R + r
  x3_min = -r; x3_max = r
  
  # Generam puncte aleatorii in cubul care contine torul
  for (i in 1:esantion) {
    x = runif(1, x1_min, x1_max)
    y = runif(1, x2_min, x2_max)
    z = runif(1, x3_min, x3_max)
    
    if ((z^2 + (sqrt(x^2 + y^2) - R)^2) < r^2)
      N_C = N_C + 1
  }
  
  # Calculam volumul cubului
  volum_cub = (x1_max - x1_min) * (x2_max - x2_min) * (x3_max - x3_min)
  
  # Estimam volumul torului
  volum_estimat = (N_C / esantion) * volum_cub
  
  return(volum_estimat)
}

erori_relative = function(R, r, esantioane) {
  for (i in 1:length(esantioane)) {
    volum_exact = 2 * pi^2 * R * r^2
    volum_estimat = estimare_volum_tor_circular(R, r, esantioane[i])
    
    eroare_relativa = abs((volum_estimat - volum_exact) / volum_exact) * 100
    
    print(eroare_relativa)
  }
}

erori_relative(10, 3, c(10000, 20000, 50000))

# ********************************************************************************************************* #

# B2.

estimare_arie_T = function(a, b, c, d, esantion) {
  # Initializam numarul de puncte in interiorul triunghiului
  N_C = 0
  
  # Generam puncte aleatorii in patrulaterul care contine triunghiul
  for (i in 1:esantion) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    
    if (y >= 0 && y <= 2*x && y <= 6 - 3*x)
      N_C = N_C + 1
  }
  
  # Estimam aria triunghiului
  arie_estimata = (N_C / esantion) * ((b - a)  * (d - c))
  
  print(arie_estimata)
}

arie_estimata = estimare_arie_T(0, 4, 0, 8, 20000)

# ********************************************************************************************************* #

# B3.

# Fiecare rezultat din return il inmultesc cu intervalul de integrare

# a)
MC_a = function(N) {
  sum = 0
  
  for (i in 1:N) {
    x = runif(1, -1, 1)
    sum = sum + (2 * x - 1) / (x^2 - x - 6)
  }
  
  return(2*sum/N)
}

MC_a_average = function(k, N) {
  estimates = 0
  
  for (i in 1:k)
    estimates[i] = MC_a(N)
  
  cat(mean(estimates), "|", "Valoarea exacta este:", log(3) - log(2))
}

MC_a_average(30, 50000)

# b)
MC_b = function(N) {
  sum = 0
  
  for (i in 1:N) {
    x = runif(1, 3, 11)
    sum = sum + (x + 4) / (x - 3)^(1 / 3)
  }
  
  return(8*sum/N)
}

MC_b_average = function(k, N) {
  estimates = 0
  
  for (i in 1:k)
    estimates[i] = MC_b(N)
  
  cat(mean(estimates), "|", "Valoarea exacta este: 61.2")
}

MC_b_average(30, 50000)

# c)
MC_c = function(N) {
  sum = 0
  
  for (i in 1:N) {
    x = runif(1, 0, 100)
    sum = sum + x * exp(-x^2)
  }
  
  return(100*sum/N)
}

MC_c_average = function(k, N) {
  estimates = 0
  
  for (i in 1:k)
    estimates[i] = MC_c(N)
  
  cat(mean(estimates), "|", "Valoarea exacta este: 0.5")
}

MC_c_average(30, 50000)

# ********************************************************************************************************* #

# B4

# a)
simulate_iSocialize_a = function(n, p, q, initial_users, target_users) {
  users = initial_users
  years = 0
  
  while (users < target_users && years < 10000) {
    # Utilizatorii care parasesc platforma
    withdrawn_users = rbinom(1, users, q)
    
    # Utilizatorii vechi care raman
    remaining_users = users - withdrawn_users
    
    # Utilizatorii noi
    new_users = rbinom(1, n, p)
    
    # Numarul total curent de utilizatori
    users = remaining_users + new_users
    
    years = years + 1
  }
  
  return(years)
}

# Simulam de mai multe ori pentru a estima numarul mediu de ani
results = numeric(1000)
for (i in 1:1000)
  results[i] = simulate_iSocialize_a(1000, 0.25, 0.01, 10000, 15000)
average_years = mean(results)
cat("Estimarea numarului mediu de ani in care iSocialize va avea +15000 utilizatori:", average_years, "\n")

# ********************************************************************************************************* #

# b)
simulate_users = function(initial_users, n, p, q, periods) {
  users = initial_users
  full_years = floor(periods)
  fractional_year = periods - full_years
  
  for (i in 1:full_years) {
    # Utilizatorii care parasesc platforma
    withdrawn_users = rbinom(1, users, q)
    
    # Utilizatorii vechi care raman
    remaining_users = users - withdrawn_users
    
    # Utilizatorii noi
    new_users = rbinom(1, n, p)
    
    # Numarul total curent de utilizatori
    users = remaining_users + new_users
  }
  
  # Partea fractionara
  if (fractional_year > 0) {
    # Utilizatorii care parasesc platforma
    withdrawn_users = rbinom(1, users, q * fractional_year)
    
    # Utilizatorii vechi care raman
    remaining_users = users - withdrawn_users
    
    # Utilizatorii noi
    new_users = rbinom(1, n, p * fractional_year)
    
    # Numarul total curent de utilizatori
    users = remaining_users + new_users
  }
  
  return(users)
}

# Parametri
years = 40 ; months = 10
total_periods = years + months / 12 # Convert years and months to total periods (fractional years)

# Simulam de mai multe ori pentru a estima probabilitatea
user_counts = numeric(100000)
for (i in 1:100000)
  user_counts[i] = simulate_users(10000, 1000, 0.25, 0.01, total_periods)

# Estimam probabilitatea
probability = mean(user_counts >= 15000)
cat("Probabilitate estimata:", probability, "\n")

# ********************************************************************************************************* #

# c)
# Definim parametrii necesari calculului
alfa = 1 - 0.99 # 99% confidence level
z = qnorm(1 - alfa / 2) # Z-score for the confidence level
epsilon = 0.01 # Marginea erorii
p_estimate = 0.69 # Rezultatul asteptat

# Numarul necesar de simulari
N_min = p_estimate * (1 - p_estimate) * (z / epsilon)^2
N_min = ceiling(N_min)

# Simulam de N_min ori pentru a estima probabilitatea
user_counts = numeric(N_min)
for (i in 1:N_min)
  user_counts[i] = simulate_users(10000, 1000, 0.25, 0.01, total_periods)

# Estimam probabilitatea
probability = mean(user_counts >= 15000)
cat("Probabilitate estimata:", probability, "\n")
