# Example code to work the examples from the paper, "The Intuition Behind Black-Litterman
# Portfolios" by He and Litterman, 1999.
# Jay Walters - September 18, 2014

# generalized math library
library(MASS)

# Functions

# reverse_optmize
#   This function executes the reverse optimization to compute the implied
#   returns consistent with the weights, covariance matrix and risk tolerance.
# Inputs
#   delta  - Risk tolerance from the equilibrium portfolio
#   weq    - Weights of the assets in the equilibrium portfolio
#   sigma  - Prior covariance matrix
# Outputs
#   er    - Implied returns
reverse_optimize <- function(delta, weq, sigma) {
  er <- t(delta * weq %*% sigma)
  return(er)
}

# black_litterman
#   This function performs the Black-Litterman blending of the prior
#   and the views into a new posterior estimate of the returns as
#   described in the paper by He and Litterman.
# Inputs
#   delta  - Risk tolerance from the equilibrium portfolio
#   weq    - Weights of the assets in the equilibrium portfolio
#   sigma  - Prior covariance matrix
#   tau    - Coefficiet of uncertainty in the prior estimate of the mean (pi)
#   P      - Pick matrix for the view(s)
#   Q      - Vector of view returns
#   Omega  - Matrix of variance of the views (diagonal)
# Outputs
#   Er     - Posterior estimate of the mean returns
#   w      - Unconstrained weights computed given the Posterior estimates
#            of the mean and covariance of returns.
#   posteriorSigma - Adjusted covariance matrix for uncertainty of estimate.
#   lambda - A measure of the impact of each view on the posterior estimates.
black_litterman <- function(delta, weq, sigma, tau, P, Q, Omega) {

  # return
  out <- list()

  # Reverse optimize and back out the equilibrium returns
  # This is formula (12) page 6
  pir <- reverse_optimize(delta, weq, sigma)
  out$pi <- pir

  # We use tau * sigma many places so just compute it once
  ts <- tau * sigma

  # check size of Q
  n <- nrow(Q)
  m <- ncol(Q)

  # if size is 0 we have special processing
  if (n > 0) {
    # Compute posterior estimate of the mean
    # This is a simplified version of formula (8) on page 4.
    er <- pir + ts %*% t(P) %*% solve(P %*% ts %*% t(P) + Omega) %*% (Q - P %*% pir)

    # Compute posterior estimate of the uncertainty in the mean
    # This is a simplified and combined version of formulas (9) and (15)
    posteriorSigma <- sigma + ts - ts %*% t(P) %*% solve(P %*% ts %*% t(P) + Omega) %*% P %*% ts

    # Compute posterior weights based on uncertainty in mean
    w <- t(er) %*% solve(delta * posteriorSigma)

    # Compute lambda value
    # We solve for lambda from formula (17) page 7, rather than formula (18)
    # just because it is less to type, and weve already computed w*.
    lambda <- t(ginv(P)) %*% (t(w)*(1+tau) - weq)
  } else {
    # Compute posterior estimate of the mean
    # This is a simplified version of formula (8) on page 4.
    er <- pir

    # Compute posterior estimate of the uncertainty in the mean
    # This is a simplified and combined version of formulas (9) and (15)
    posteriorSigma <- sigma + ts

    # Compute posterior weights based on uncertainty in mean
    w <- t(t(er) %*% t(solve(delta * posteriorSigma)))

    # Compute lambda value
    # We solve for lambda from formula (17) page 7, rather than formula (18)
    # just because it is less to type, and weve already computed w*.
    lambda <- 0
  }

  colnames(er) <- "Return"

  out$er <- er
  out$w <- w
  out$posteriorSigma <- posteriorSigma
  out$lambda <- lambda
  return(out)
}

# print_results
#    This function prints out the results of a Black-Litterman estimation in the
#    format used in the He and Litterman 1999 paper.
# Inputs
#
# Outputs
#
print_results <- function(tau, P, Q, Omega, results) {
  # Extract some labels we'll want for later
  views <- rownames(P)
  assets <- colnames(P)

  n = nrow(P)
  m = ncol(P)

  # Show results which wil be Lambda = 0 for view 3 proving Property 3.1
  col <- 100*t(P)
  col <- append(col, formatC(100*results$er, digits=1, format="f", width=4))
  col <- append(col, formatC(100*results$w, digits=1, format="f", width=4))
  mt <- matrix(col, m, n+2, byrow=FALSE)
  rownames(mt) <- assets
  colnames(mt) <- c(views, "  er", "  w")
  # T is Q, t/W and lambda
  col <- t(format(100*Q, justify="right", trim=FALSE, digits=2, format="f", width=4))
  col <- append(col, format(round(diag(Omega)/tau,digits=4), justify="right", trim=FALSE, digits=3, format="f", width=5))
  col <- append(col, format(round(t(results$lambda),digits=4), scientific=FALSE, justify="right", trim=FALSE, digits=3, format="f", width=5))
  mb <- matrix(col, 3, n, byrow=TRUE)
  rownames(mb) <- c("q  ","w/tau    ","Lambda")
  colnames(mb) <- views
  print(mt, justify="right", quote=FALSE)
  print(mb, justify="right", quote=FALSE)
}

# Scale x up to percentages
percent_formatter <- function(x) {
  xv <- x * 100
  lab <- sprintf('%3.0f%%', xv)
  return(lab)
}


# Distribution parameters
assets <- c("Australia","Canada","France","Germany","Japan","UK","USA")

weq <- c(0.016, 0.022, 0.052, 0.055, 0.116, 0.124, 0.615)
names(weq) <- assets

d <- c()
d <- append(d, c(1.000, 0.488, 0.478, 0.515, 0.439, 0.512, 0.491))
d <- append(d, c(0.488, 1.000, 0.664, 0.655, 0.310, 0.608, 0.779))
d <- append(d, c(0.478, 0.664, 1.000, 0.861, 0.355, 0.783, 0.668))
d <- append(d, c(0.515, 0.655, 0.861, 1.000, 0.354, 0.777, 0.653))
d <- append(d, c(0.439, 0.310, 0.355, 0.354, 1.000, 0.405, 0.306))
d <- append(d, c(0.512, 0.608, 0.783, 0.777, 0.405, 1.000, 0.652))
d <- append(d, c(0.491, 0.779, 0.668, 0.653, 0.306, 0.652, 1.000))
C <- matrix(d, 7, 7)
colnames(C) <- assets
rownames(C) <- assets

Sigma <- as.vector(c(0.160, 0.203, 0.248, 0.271, 0.210, 0.200, 0.187))
names(Sigma) <- assets

V <- diag(Sigma) %*% C %*% diag(Sigma)
colnames(V) <- assets
rownames(V) <- assets

mu <- as.vector( c(0.039, 0.069, 0.084, 0.090, 0.043, 0.068, 0.076) )
names(mu) <- assets

# Example specific parameters
delta <- 2.5
tau <- 0.05

# Compute the equilibrium returns (prior returns in case we want them)
pir <- reverse_optimize(delta, weq, V)

# Define the views

# Define view 1
# Germany will outperform the other European markets by 5%
# Market cap weight the P matrix
# Results should match Table 4, Page 21

views <- c("View 1")

P1 <- c(0, 0, -.295, 1.00, 0, -.705, 0 )
P <- matrix(P1, 1, 7)
colnames(P) <- assets
rownames(P) <- views

Q1 <- c(0.05)
Q <- matrix(Q1, 1, 1)
colnames(Q) <- views
rownames(Q) <- c("Returns")

Omega <- tau * P %*% V %*% t(P) * diag(1)

results <- black_litterman(delta, weq, V, tau, P, Q, Omega)
print_results(tau, P, Q, Omega, results)

# Define view 2
# Canadian Equities will outperform US Equities by 3%
# Results should match Table 5, Page 22

views <- c("View 1", "View 2")

P2 <- c(0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0)
P <- matrix(c(P1,P2), 2, 7, byrow=TRUE)
colnames(P) <- assets
rownames(P) <- views

Q2 <- c(0.03)
Q <- matrix(c(Q1, Q2), 2, 1)
rownames(Q) <- views
colnames(Q) <- c("Return")

Omega <- tau * P %*% V %*% t(P) * diag(2)

results <- black_litterman(delta, weq, V, tau, P, Q, Omega);
print_results(tau, P, Q, Omega, results)

# Make View 2 more Bullish
# Canadian Equities will now outperform US Equities by 4%
# Results should match Table 6, Page 23

Q2 <- c(0.04)
Q <- matrix(c(Q1, Q2), 2, 1)
rownames(Q) <- views
colnames(Q) <- c("Return")

results <- black_litterman(delta, weq, V, tau, P, Q, Omega);
print_results(tau, P, Q, Omega, results)

# Investor becomes more uncertain about view 1
# German Equities will outperfrom but now investor twice as uncertain
# Results should match Table 7, Page 24

Omega <- tau * P %*% V %*% t(P) * diag(2)
Omega[1,1] <- 2 * Omega[1,1]

results <- black_litterman(delta, weq, V, tau, P, Q, Omega);
print_results(tau, P, Q, Omega, results)

# Define view 3
# Canadian Equities will outperform JP equities by 4.12, this matches
# the implied posterior return from the first 2 views.
# Results should match Table 8, Page 25

views <- c("View 1", "View 2", "View 3")

P3 <- c(0.0, 1.0, 0.0, 0.0, -1.0, 0.0, 0.0)
P <- matrix(c(P1,P2,P3), 3, 7, byrow=TRUE)
colnames(P) <- assets
rownames(P) <- views

# Implied returns from the first two views making the third view have weight
# on view of 0, this is known as Property 3.1 by He & Litterman
postSigma <- V + results$posteriorSigma
erimp <- V %*% solve(results$posteriorSigma) %*% results$er

# Now set the return for this view to the implied return for 0 weight on view
Q3 <- c(erimp[2] - erimp[5])
Q <- matrix(c(Q1, Q2, Q3), 3, 1)
rownames(Q) <- views
colnames(Q) <- c("Return")

Omega <- tau * P %*% V %*% t(P) * diag(3)
Omega[1,1] <- 2 * Omega[1,1]

results <- black_litterman(delta, weq, V, tau, P, Q, Omega);
print_results(tau, P, Q, Omega, results)
