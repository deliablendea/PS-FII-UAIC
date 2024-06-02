# Am lucrat cu .txt in loc de .csv, pentru ca imi formata numerele in date (31.05 -> 31.mai)

# D1.

zconfidence_interval = function(file_name, confidence_level) {
  date = read.table(file_name, header = TRUE)
  
  n = length(date$probabilitati)
  sample_mean = mean(date$probabilitati)
  alpha = 1 - confidence_level
  sigma = sqrt(92.16)
  
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  
  lower_bound = xn - critical_z * sigma / sqrt(n)
  upper_bound = xn + critical_z * sigma / sqrt(n)
  
  return(c(lower_bound, upper_bound))
}

cat("Interval de incredere de 95%:", zconfidence_interval("probabilitati.txt", 0.95), "\n")
cat("Interval de incredere de 99%:", zconfidence_interval("probabilitati.txt", 0.99), "\n")

# ****************************************************************************************** #

# D2.

t_conf_interval = function(file_name, confidence_level) {
  date = read.table(file_name, header = TRUE)
  
  n = length(date$statistica)
  sample_mean = mean(date$statistica)
  alpha = 1 - confidence_level
  sigma = sd(date$statistica)
  
  se = sigma / sqrt(n)
  
  critical_t = qt(1 - alpha / 2, n - 1);
  
  lower_bound = sample_mean - critical_t * se
  upper_bound = sample_mean + critical_t * se
  
  return(c(lower_bound, upper_bound))
}

cat("Interval de incredere de 95%:", t_conf_interval("statistica.txt", 0.95), "\n")
cat("Interval de incredere de 99%:", t_conf_interval("statistica.txt", 0.99), "\n")

# ****************************************************************************************** #

# D3.

test_proportion = function(n, alpha, succese, p0) {
  p_prim = succese / n
  z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  critical_z = qnorm(1 - alpha, 0, 1)
  
  cat("Statistica testului", alpha * 100, "%: z_score:", z_score, "| critical_z:", critical_z)
}

n = 100
succese = 86 # Cel mult 14 nu pot rezolva temele
p0 = 0.85

cat(test_proportion(n, 0.01, succese, p0), "\n")
cat(test_proportion(n, 0.05, succese, p0), "\n")