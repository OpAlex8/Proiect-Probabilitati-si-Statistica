
#----------------------------- Bonus a -------------------------------

# Generez valorile repartitiei comune
generate_matrix <- function(mtx, m, n) {
  sum_y <- array(0, m)
  for (i in 1:n) {
    sum_x <- 0
    for (j in 1:m) {
        aux <- sample(1:100, 1)
        mtx[i, j] <- aux
        sum_x <- sum_x + aux
        sum_y[j] <- sum_y[j] + aux
    }
    mtx[i, m + 1] <- sum_x
  }
  
  sum <- 0
  for (i in 1:m) {
    mtx[n + 1, i] <- sum_y[i]
    sum <- sum + sum_y[i]
  }
  
  mtx[n + 1, m + 1] <- sum
  for (i in 1: (n + 1)){
    for (j in 1: (m + 1))
      mtx[i, j] <- ceiling(as.numeric(mtx[i, j]) / sum * 1000) / 1000;
  }
  
  return (mtx)
}

# Reinitializez o serie de casute cu [?]
generate_q <- function(mtx, m, n) {
  for (i in 1:n) {
    mtx[i, 1] <- '?'
    if (i <= m) {
      mtx[i, i] <- '?'
    }
  }
  
  if (m > n) {
    for (i in (n + 1):(m + 1)) {
      mtx[1, i] <- '?'
    }
  }
  mtx[n + 1, 1] <- '?'
  
  return (mtx)
}

# Generez doua variabile si repartitia comuna incompleta a acestora
frepcomgen <- function(m, n) {
  MAX = max(10, n, m)
  
  x <- sample((-MAX):MAX, m)
  y <- sample((-MAX):MAX, n)
  x <- x[order(x)]
  y <- y[order(y)]
  
  mtx <- matrix(nrow = n + 1, ncol = m + 1,
               dimnames = list(c(y, " "), c(x, " ")))
  
  mtx <- generate_matrix(mtx, m, n)
  mtx <- generate_q(mtx, m, n)
}

#----------------------------- Bonus b -------------------------------

# Numar casutele cu [?]
count_q <- function(mtx, k, m, pp) {
  if (pp == 0)  i = k
  else  j = k
  
  count <- 0
  for (l in 1:(m + 1)) {
    if (pp == 0)  j = l
    else  i = l
    
    if (mtx[i, j] == "?") count <- count + 1
  }
  
  return (count)
}

# Pun in casutele cu [?] valoarea potrivita
fix <- function(mtx, k, m, pp) {
  if (pp == 0)  i = k
  else  j = k
  
  h <- m + 1
  sum <- 0
  for (l in 1:m) {
    if (pp == 0)  j = l
    else  i = l
    
    if (mtx[i, j] == "?") h <- l
    else  sum <- sum + as.numeric(mtx[i, j])
  }
  
  if (h == m + 1) {
    if (pp == 0)  mtx[k, h] = sum
    else  mtx[h, k] = sum
  } else {
    if (pp == 0)  mtx[k, h] = as.numeric(mtx[k, m + 1]) - sum
    else  mtx[h, k] = as.numeric(mtx[m + 1, k]) - sum
  }
  
  return (mtx)
}

# Completez repartitia comuna din mtx
fcomplrepcom <- function(mtx, m, n) {
  for (i in 1:(n + 1)) 
    if (count_q(mtx, i, m, 0) == 1) {
      mtx <- fix(mtx, i, m, 0)
      mtx <- fcomplrepcom(mtx, m, n)
      return (mtx)
    }
  
  for (j in 1:(m + 1)) 
    if (count_q(mtx, j, n, 1) == 1) {
      mtx <- fix(mtx, j, n, 1)
      mtx <- fcomplrepcom(mtx, m, n)
      return (mtx)
    }
  
  return (mtx)
}

#----------------------------- Bonus c -------------------------------

MINIM <- -1000000
MAXIM <- 1000000

# Calculez media unei variabilei aleatoare
E <- function(mtx, val, m, n, pp) {
  if (pp == 0)  i = m + 1
  else  j = m + 1
  
  E <- 0
  for (l in 1:n) {
    if (pp == 0)  j = l
    else  i = l
    
    E <- E + as.numeric(val[l]) * as.numeric(mtx[i, j])
  }
  
  return (E)
}

# Calculez covalenta a doua variabile aleatoare
covalent <- function(mtx, m, n) {
  val_x <- colnames(mtx)
  val_y <- rownames(mtx)
  
  Ex <- E(mtx, val_x, n, m, 0)
  Ey <- E(mtx, val_y, m, n, 1)
  
  Exy <- 0
  for (i in 1:n){
    for (j in 1:m){
      Exy <- Exy + as.numeric(mtx[i,j]) *
        as.numeric(val_x[j]) * as.numeric(val_y[i])
    }
  }
  
  return (Exy - Ex * Ey)
}

# Calculez probabilitatea marginita pentru doua variabile aleatoare
probability <- function(mtx, m, n, lower_x = MINIM, upper_x = MAXIM, 
                        lower_y = MINIM, upper_y = MAXIM) {
  val_x <- as.numeric(colnames(mtx))
  val_y <- as.numeric(rownames(mtx))
  
  p <- 0
  for (i in  1:m){
    for (j in 1:n){
      if (val_x[i] > lower_x && val_x[i] < upper_x &&
          val_y[j] > lower_y && val_y[j] < upper_y)
        p <- p + as.numeric(mtx[i,j])
    }
  }
  return (as.numeric(p))
}

# Calculez probabilitatea conditionata pentru doua variabile aleatoare
cond_probability <- function(mtx, m, n, lower_x = MINIM, upper_x = MAXIM,
                             lower_cond_x = MINIM, upper_cond_x = MAXIM,
                             lower_y = MINIM, upper_y = MAXIM,
                             lower_cond_y = MINIM, upper_cond_y = MAXIM) {
  lower_x <- max(lower_x, lower_cond_x)
  lower_y <- max(lower_y, lower_cond_y)
  upper_x <- min(upper_x, upper_cond_x)
  upper_y <- min(upper_y, upper_cond_y)
  
  p_up <- probability(mtx, m, n, lower_x, upper_x, lower_y, upper_y)
  p_down <- probability(mtx, m, n, lower_cond_x, upper_cond_x, lower_cond_y, upper_cond_y)
  
  return (p_up / p_down)
}

# Subpunctul a
cov_a <- function(mtx, m, n) {
  return ((-3) * 5 * covalent(mtx, m, n))
}

# Subpunctul b
prob_b <- function(mtx, m, n) {
  return (cond_probability(mtx, m, n, lower_x = 0, upper_x = 3, lower_cond_y = 2))
}

# Subpunctul c
prob_c <- function(mtx, m, n) {
  return (probability(mtx, m, n, lower_x = 6, upper_y = 7))
}

#----------------------------- Bonus d -------------------------------

EPSILON <- 0.001

# Verific daca variabilele din mtx sunt independente
fverind <- function(mtx, m, n) {
  for (i in 1:n)
    for (j in 1:m)
      if (EPSILON < as.numeric(mtx[i, j]) -
          as.numeric(mtx[i, m + 1]) * as.numeric(mtx[n + 1, j]))
        return (0)
  
  return (1)
}

# Verific daca variabilele din mtx sunt necorelate
fvernecor <- function(mtx, m, n) {
  return (abs(covalent(mtx, m, n)) < EPSILON)
}

#----------------------------- Teste -------------------------------

n <- 2
m <- 3

mtx <- frepcomgen(m, n)
print("Repartitia comuna incompleta a variabilelor generate:")
print(mtx)

mtx <- fcomplrepcom(mtx, m, n)
print("Repartitia comuna completa a variabilelor generate:")
print(mtx)

print("Cov(5X,-3Y)")
print(cov_a(mtx, m, n))

print("P(0<X<3/Y>2)")
print(prob_b(mtx, m, n))

print("P(X>6,Y<7)")
print(prob_c(mtx, m, n))

if (fverind(mtx, m, n)) {
  message(cat("Variabilele X si Y sunt independente"))
} else {
  message(cat("Variabilele X si Y nu sunt independente"))
}

if (fvernecor(mtx, m, n)) {
  message(cat("Variabilele X si Y sunt necorelate"))
} else {
  message(cat("Variabilele X si Y sunt corelate"))
}

