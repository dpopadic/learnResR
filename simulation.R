# SIMULATIONS IN R
# ----------------


# 1 SOLVING CALCULUS PROBLEMS
# ---------------------------
# Let's say we want to integrate x^2 over interval [0, 1]. Using monte-carlo simulation,
# one can throw darts at the curve to count the number falling below the curve. The challenge
# is to vectorise the simulation.

# non-vectorised:
mc <- function(n) {
  hits <- 0
  for (i in seq_len(n)) {
    u1 <- runif(1)
    u2 <- runif(1)
    if (u1^2 > u2)
      hits <- hits + 1
  }
  return(hits / n)
}

system.time(mc(n = 1000000))

# vectorised:
mc <- function(n) sum(runif(n)^2 > runif(n)) / n
system.time(mc(n = 1000000))
