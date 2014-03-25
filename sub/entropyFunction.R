ObservedEstFunIn <-
  function(y, t) {
    y <- y[y > 0]
    U <- sum(y)
    pi <- (y / t) / (U / t)
    est <- -sum(pi * log(pi))
    return(est)
  }

ZhangHzstarEstFun <-
  function(x) {
    x <- x[x > 0]
    n <- sum(x)
    forward <- sum(x / n * (digamma(n) - digamma(x)))
    k <- 1 : (n-1)
    back <- ObservedEstFun(x) - 
      sum(sapply(k, function(k) 1 / (n * k) * sum(x * (1 - x / n)^k)))
    return(forward + back)
  }

ZahlJackEstFun <-
  function(x) { 
    x <- x[x > 0]
    n <- sum(x)
    sumjk <- -1 * sum((n - x) * x / (n - 1) * log(x / (n - 1))) - 
      sum((x[x > 1] - 1) * (x[x > 1]) / (n - 1) * log((x[x > 1] - 1) / (n - 1)))
    n * ObservedEstFun(x) - (n - 1) / n * sumjk
  }

A.C.Q0FunIn <-
  function(y, t) {
    Q1 <- sum(y == 1)
    Q2 <- sum(y == 2)
    U <- sum(y)
    if (Q2 > 0) {
      A <- 2 * Q2 / ((t - 1) * Q1 + 2 * Q2)
      C <- 1 - Q1 / U * ((t - 1) * Q1 / ((t - 1) * Q1 + 2 * Q2))
      Q0 <- (t - 1) / t * Q1^2 / (2 * Q2)
    } else if (Q2 == 0 & Q1 != 0) {
      A <- 2 / ((t - 1) * (Q1 - 1) + 2)
      C <- 1 - Q1 / U * ((t - 1) * (Q1 - 1) / ((t - 1) * (Q1 - 1) + 2))
      Q0 <- (t - 1) / t * Q1 * (Q1 - 1) / 2
    } else {
      A <- 1
      C <- 1
      Q0 <- (t - 1) / t * Q1 * (Q1 - 1) / 2
    }
    Q0 <- ceiling(Q0)
    return(c(A, C, Q0))
  }

entropy_Zhang <-
  function(data, B = 200, conf = 0.95) {
    est <- ZhangHzstarEstFun(data)
    se <- BootstrapFun(data, B, ZhangHzstarEstFun)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Zhang (2012) Hz*")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

GrassEstFun <-
  function(dat) { 
    dat <- round(dat)
    n <- sum(dat)
    p.hat <- dat/n
    a = 0
    for(i in 1:length(dat)){
      if(p.hat[i] > 0){
        integrand <- function(x) { x^(dat[i] - 1) / (1 + x) }
        a <- a + p.hat[i] * (digamma(n) - digamma(dat[i]) - (-1)^dat[i]
                             * (integrate(integrand, lower = 0, upper = 1))$value)
      }
    }
    return(a)
  }

ObservedEstFun <-
  function(x) { 
    #   entropy(x, method = "ML")
    x <- x[x > 0]
    n <- sum(x)
    pi <- x / n
    est <- -sum(pi * log(pi))
    return(est)
  }


entropy_ObservedIn <-
  function(data, B = 200, conf = 0.95) {
    t <- data[1]
    y <- data[-1]
    est <- ObservedEstFunIn(y, t)
    se <- BootstrapFunIn(y, t, B, ObservedEstFunIn)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Observed entropy")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out) 
  }

entropy_Observed <-
  function(data, B = 200, conf = 0.95) {
    est <- ObservedEstFun(data)
    se <- BootstrapFun(data, B, ObservedEstFun)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Observed entropy")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

entropy_Grassberger <-
  function(data, B = 200, conf = 0.95) {
    est <- GrassEstFun(data)
    se <- BootstrapFun(data, B, GrassEstFun)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Grassberger (2003)")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

entropy_Jackknife <-
  function(data, B = 200, conf = 0.95) {
    est <- ZahlJackEstFun(data)
    se <- BootstrapFun(data, B, ZahlJackEstFun)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Zahl (1977) Jackknife")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

entropy_ChaoShen <-
  function(data, B = 200, conf = 0.95) {
    est <- ChaoShenEstFun(data)
    se <- BootstrapFun(data, B, ChaoShenEstFun)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Chao_Shen (2003)")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

entropy_ChaoIn <-
  function(data, B = 200, conf = 0.95) {
    t <- data[1]
    y <- data[-1]
    est <- ChaoEntropyEstFunIn(y, t)
    se <- BootstrapFunIn(y, t, B, ChaoEntropyEstFunIn)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Chao entropy")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

entropy_Chao <-
  function(data, B = 200, conf = 0.95) {
    est <- ChaoEntropyEstFun(data)
    se <- BootstrapFun(data, B, ChaoEntropyEstFun)
    z <- qnorm(1 - (1 - conf)/2)
    CI <- c(max(est - z * se, 0), est + z * se)
    out <- matrix(c(est, se, CI), nrow = 1)
    rownames(out) <- c("Chao_entropy (2013)")
    colnames(out) <- c("Estimator", "Bootstrap s.e.",
                       paste(conf*100, "% Lower"), paste(conf*100, "% Upper"))
    return(out)
  }

ChaoShenEstFun <-
  function(x){ 
    #   entropy(x, method = "CS")
    x <- x[x > 0]
    n <- sum(x)
    f <- function(i) { sum(x == i) }
    f1 = f(1); f2 = f(2)
    Chat <- Candf0Fun(f1, f2, n)[1]
    adjpi <- Chat * x / n
    est <- - sum(adjpi * log(adjpi) / (1 - (1 - adjpi)^n))
    return(est)
  }

ChaoEntropyOnline <-
  function (data, datatype = c("abundance", "incidence"), 
            method = c("Chao", "ChaoShen", "Grassberger", "Jackknife",
                       "Zhang", "Observed"), se = TRUE, nboot = 200, 
            conf = 0.95) {
    if (is.matrix(data) == TRUE || is.data.frame(data) == TRUE) {
      data <- as.matrix(data)
      if (ncol(data) != 1 & nrow(data) != 1) 
        stop("Error: The data format is wrong.")
      if (ncol(data) == 1) {
        data <- data[, 1]
      }
      else {
        data <- data[1, ]
      }
    }
    if (is.numeric(conf) == FALSE || conf > 1 || conf < 0) {
      cat("Warning: \"conf\"(confidence level) must be a numerical value between 0 and 1, e.g. 0.95.", 
          "\n")
      cat("          We use \"conf\" = 0.95 to calculate!", 
          "\n\n")
      conf <- 0.95
    }
    if (se == TRUE) {
      B <- nboot
      if (nboot < 1) 
        nboot <- 1
      if (nboot == 1) 
        cat("Warning: When \"Number of bootstraps\" =", B, ", the bootstrap s.e. and confidence interval can't be calculated.", 
            "\n\n")
    }
    method <- match.arg(method, several.ok=T)
    datatype <- match.arg(datatype)
    if (datatype == "abundance") {
      if (sum(data) == 0) 
        stop("The data didn't have any information.")
      if (sum(data > 0) == 1) {
        cat("Warning: When the individual-based (abundance) data only have \"ONE\" species.", 
            "\n")
        cat("         ALL estimator are equal to 0,and the standard error will meaningless.", 
            "\n\n")
        out <- data.frame(rep(0, 6), rep(0, 6), rep(0, 6), rep(0, 6))
        colnames(out) <- c('Estimator', 'Bootstrap s.e.', "95 % Lower", '95 % Upper')
        rownames(out) <- c("Chao_entropy (2013)", "Chao_Shen (2003)", 
                           "Grassberger (2003)", "Zahl (1977) Jackknife", 
                           "Zhang (2012) Hz*", "Observed entropy")
      }
      else {
        out <- ChaoEntropy.IndOnline(data, method, nboot, conf, se)
      }
    }
    if (datatype == "incidence") {
      if (sum(data[1] < data[-1]) != 0) 
        stop("Error: Total number of sampling units should be greater than the species incidence frequency.")
      if (length(data) == 1) 
        stop("Error: The input format of first entry should be total number of sampling units, and followed by species incidence frequency. You only type the total number of sampling units.")
      if (sum(data[-1]) == 0) 
        stop("Error: The data didn't have enough information.")
      out <- ChaoEntropy.SamOnline(data, method, nboot, conf, se)
    }
    return(out)
  }

ChaoEntropyEstFun <-
  function(x) {
    x <- x[x > 0]
    n <- sum(x)
    f <- function(i) { sum(x == i) }
    A <- AFun(x)
    temp1 <- sum(x / n * (digamma(n) - digamma(x)))
    if(A == 1){
      temp2 <- 0
    } else {
      l <- 1:(n-1)
      temp2 <- f(1) / n * (1 - A)^(1-n) * (-log(A) - sum(1 / l * (1-A)^l))
    }
    est <- (temp1 + temp2)
    return(est)
  }

ChaoEntropy.SamOnline <-
  function(data, method = c("Chao", "Observed"),
           B = 200, conf = 0.95, se = TRUE) {
    if ("Chao" %in% method) {
      a <- entropy_ChaoIn(data, B, conf)
    } else {
      a <- NULL
    }
    if ("Observed" %in% method) {
      f <- entropy_ObservedIn(data, B, conf)
    } else {
      f <- NULL
    }
    out <- rbind(a, f)
    return(out)
  }

ChaoEntropyEstFunIn <-
  function(y, t) {
    y <- y[y > 0]
    U <- sum(y)
    Q1 <- sum(y == 1)
    tmp <- A.C.Q0FunIn(y, t)
    A <- tmp[1]
    temp1 <- sum(y / t * (digamma(t) - digamma(y)))
    if (A == 1) {
      temp2 <- 0
    } else {
      r <- 1 : (t-1)
      temp2 <- Q1 / t * (1 - A)^(1-t) * (-log(A) - sum(1 / r * (1 - A)^r))
    }
    est <- t / U * (temp1 + temp2) + log(sum(y) / t)
    return(est)
  }


ChaoEntropy.IndOnline <-
  function(data, 
           method = c("Chao", "ChaoShen", "Grassberger",
                      "Jackknife", "Zhang", "Observed"), 
           B = 200, conf = 0.95, se = TRUE) {
    if ("Chao" %in% method) {
      a <- entropy_Chao(data, B, conf)
    } else {
      a <- NULL
    }
    if ("ChaoShen" %in% method) {
      b <- entropy_ChaoShen(data, B, conf)
    } else {
      b <- NULL
    }
    if ("Grassberger" %in% method) {
      c <- entropy_Grassberger(data, B, conf)
    } else {
      c <- NULL
    }
    if ("Jackknife" %in% method) {
      d <- entropy_Jackknife(data, B, conf)
    } else {
      d <- NULL
    }
    if ("Zhang" %in% method) {
      e <- entropy_Zhang(data, B, conf)
    } else {
      e <- NULL
    }
    if ("Observed" %in% method) {
      f <- entropy_Observed(data, B, conf)
    } else {
      f <- NULL
    }
    out <- rbind(a, b, c, d, e, f)
    return(out)
  }

AFun <-
  function(x) {
    x <- x[x > 0]
    n <- sum(x)
    f <- function(i) { sum(x == i) }
    f1 <- f(1); f2 <- f(2)
    if (f2 > 0) {
      A <- 2 * f2 / ((n - 1) * f1 + 2 * f2)
    } else if (f2 == 0 & f1 != 0) {
      A <- 2 * (f2 + 1) / ((n - 1) * (f1 - 1)+ 2 * (f2 + 1))
    } else {
      A <- 1
    }
    return(A)
  }

Candf0Fun <-
  function(f1, f2, n) {
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
    return(c(C, f0))
  }

BootstrapFunIn <-
  function(y, t, B, FunName) {
    U <- sum(y)
    tmp <- A.C.Q0FunIn(y, t)
    Chat <- tmp[2]
    Q0 <- tmp[3]
    tau <- U / t * (1 - Chat) / sum(y / t * (1 - y / t)^t)
    pi <- y / t * (1 - tau * (1 - y / t)^t)
    pi.star <- c(pi, rep(U / t * (1 - Chat) / Q0, Q0))
    #   set.seed(456)
    y1 <- matrix(rbinom(length(pi.star) * B, t, pi.star), ncol = B)
    se <- sd(apply(y1, 2, function(y2) FunName(y2, t)), na.rm=TRUE)
    return(se)
  }

BootstrapFun <-
  function(x, B, FunName) {
    n <- sum(x)
    f <- function(i) { sum(x == i) }
    f1 = f(1); f2 = f(2)
    tmp <- Candf0Fun(f1, f2, n)
    Chat <- tmp[1] ; f0 <- tmp[2]
    lambda <- (1 - Chat) / sum(x / n * (1 - x / n)^n)
    pi <- x / n * (1 - lambda * (1 - x /n)^n)
    pi.star <- c(pi, rep((1 - Chat) / f0, f0))
    #   set.seed(1234)
    X <- rmultinom(B, n, pi.star)
    se <- sd(apply(X, 2, function(x) FunName(x)))
    return(se)
  }
