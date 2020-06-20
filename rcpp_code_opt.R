# CODE OPTIMISATION USING C++ IN R
# -------------------------------------------------------------------------------------------------
library(Rcpp)

# some key points to keep in mind for r vs c++ ------------------------------
# - R is dynamically type language, while c++ is statically typed (eg. once x is defined as
# double in c++, it cannot be redefined as integer whereas it can in r). Therefore, you need
# to declare the variables types in a c++ function as well as the type of the returned object:
# double sum(double x, double y) {
#  body ..
# }



# simple c++ expressions in r basics ----------------------------------------
# evaluate whether rcpp is properly setup by evaluating a simple function
Rcpp::evalCpp("40 + 20")
Rcpp::evalCpp("exp(9)")
y <- Rcpp::evalCpp("sqrt(2)")
base::storage.mode(y)
# Note: In R, a 1-digit number (eg. 2) has class double/numeric whereas in c++ it's integer if
# it's 3 or double if it's 3.0. Explicit casting can be set:
y <- Rcpp::evalCpp("(double) 3")
base::storage.mode(y)
# in c++, the following result is an integer
y <- Rcpp::evalCpp("17/2")
base::storage.mode(y)
# one needs to cast it to return a double
y <- Rcpp::evalCpp("(double) 17/2")
base::storage.mode(y)
# get largest representable
Rcpp::evalCpp("std::numeric_limits<int>::max()")

# quick benchmarking in r
sum_loop <- function(x)
  sum(x)
all.equal(sum_loop(x), sum(x))
# compare the performance
microbenchmark::microbenchmark(sum_loop = sum_loop(x), 
                               r_sum = sum(x))


# custom user-defined functions -----------------------------------------------
# define a custom function
Rcpp::cppFunction("int ft(){
                  int x = 66;
                  return x;}")
# call the function
ft()

# compare 2 equivalent functions
# c++ 
Rcpp::cppFunction("double add_cpp(double x, double y){
                  double res = x + y;
                  return res;}")
# r
add_r <- function(x, y)
  return(x + y)

# compare performance
microbenchmark::microbenchmark(ev_cpp = add_cpp(x, y), 
                               ev_r = add_r(x, y))








