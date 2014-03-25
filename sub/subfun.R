BasicInfoFun_Ind <- function(x, B) {
  n <- sum(x)
  f1 <- sum(x == 1)
  f2 <- sum(x == 2)
  D <- sum(x > 0)
  if (f2 > 0) {
    C <- 1 - f1 / n * ((n - 1) * f1 / ((n - 1) * f1 + 2 * f2))
    f0 <- (n - 1) / n * f1^2 / (2 * f2)
  } else if (f2 == 0 & f1 != 0) {
    C <- 1 - f1 / n * ((n - 1) * (f1 - 1) / ((n - 1) * (f1 - 1) + 2))
    f0 <- (n - 1) / n * f1 * (f1 - 1) / 2
  } else {
    C <- 1
    f0 <- (n - 1) / n * f1 * (f1 - 1) / 2
  }
  f0 <- ceiling(f0)
  S <- D + f0
  
  i <- 1:n
  tmp1 <- sum(sapply(i, function(i) i * (i-1) * sum(x == i)))
  tmp2 <- sum(sapply(i, function(i) i * sum(x == i)))
  gamma <- max((D / C) * (tmp1 / (tmp2 * (tmp2 - 1))) - 1 , 0)
  CV <- sqrt(gamma)    #  Estimated CV
  
  C <- round(C, 3)
  CV <- round(CV, 3)
  
  col1 <- c("Number of observed individuals", 
            "Number of observed species", 
            "Estimated speceis richness by Chao 1 (Chao, 1984)",
            "Number of singletons", 
            "Number of doubletons", 
            "Estimated sample coverage", 
            "Estimated CV", 
            "Bootstrap replications for s.e. estimate")
  col2 <- paste(c("n =", "D =", "Shat =", "f1 =", "f2 =", "C =", "CV =", "B ="),
                c(n, D, S, f1, f2, C, CV, B))
  
  out <- data.frame(Meaning=col1, Value = col2)
  return(out)
}

BasicInfoFun_Sam <- function(x, B) {
  t <- x[1]
  y <- x[-1]
  D <- sum(y > 0)
  Q1 <- sum(y == 1)
  Q2 <- sum(y == 2)
  U <- sum(y)
  if (Q2 > 0) {
    C <- 1 - Q1 / U * ((t - 1) * Q1 / ((t - 1) * Q1 + 2 * Q2))
    Q0 <- (t - 1) / t * Q1^2 / (2 * Q2)
  } else if (Q2 == 0 & Q1 != 0) {
    C <- 1 - Q1 / U * ((t - 1) * (Q1 - 1) / ((t - 1) * (Q1 - 1) + 2))
    Q0 <- (t - 1) / t * Q1 * (Q1 - 1) / 2
  } else {
    C <- 1
    Q0 <- (t - 1) / t * Q1 * (Q1 - 1) / 2
  }
  Q0 <- ceiling(Q0)
  S <- Q0 + D
  
  C <- round(C, 3)
  col1 <- c("Number of observed sampling units", 
            "Number of observed species", 
            "Estimated speceis richness by Chao 2 (Chao, 1987)",
            "Number of singletons", 
            "Number of doubletons", 
            "Estimated sample coverage", 
            "Bootstrap replications for s.e. estimate")
  col2 <- paste(c("T =", "D =", "Shat =", "Q1 =", "Q2 =", "C =", "B ="),
                c(t, D, S, Q1, Q2, C, B))
  out <- data.frame(Meaning=col1, Value = col2)
  return(out)
}

saveList2csv <- function(out, file) {
  for (i in seq_along(out)){
    write.table(names(out)[i], file=file, sep=",", dec=".", 
                quote=FALSE, col.names=FALSE, row.names=FALSE, append=TRUE)  #writes the name of the list elements
    write.table(out[[i]], file=file, sep=",", dec=".", quote=FALSE, 
                col.names=FALSE, row.names=TRUE, append=T)  #writes the data.frames
  }
}
