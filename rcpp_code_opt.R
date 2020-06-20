# CODE OPTIMISATION USING C++ IN R
# -------------------------------------------------------------------------------------------------
library(Rcpp)
# some key points to keep in mind for r vs c++ ------------------------------------
# - R is dynamically type language, while c++ is statically typed (eg. once x is defined as
# double in c++, it cannot be redefined as integer whereas it can in r). Therefore, you need
# to declare the variables types in a c++ function as well as the type of the returned object:
# double sum(double x, double y) {
#  body ..
# }
# - in c++, indexing starts at 0 (not 1)
# - vectorised functions in r are functions with for loops created in a compiled language like c++

# simple c++ expressions in r basics ----------------------------------------------
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


# custom user-defined functions --------------------------------------------------
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
Rcpp::cppFunction("double euclid_dist_cpp(double x, double y) {
                   return sqrt(x*x + y*y);}")
# r
add_r <- function(x, y)
  return(x + y)
euclid_dist_r <- function(x, y)
  return(x^2 + y^2)

# compare performance
microbenchmark::microbenchmark(ev_cpp = add_cpp(x, y), 
                               ev_r = add_r(x, y))
microbenchmark::microbenchmark(ev_cpp = euclid_dist_cpp(x, y), 
                               ev_r = euclid_dist_r(x, y))

# debugging in rcpp ---------------------------------------------------------------

# use Rprintf() to print output to the console. Similar to the sprintf() function 
# in r, and uses the format specifiers:
# - %d to specify an integer
# - %s to specify a string
# - use \\n to print next line

# print to the r console
Rcpp::cppFunction('int fun_int(){
                  int x = 42;
                  Rprintf("printing to console the number defined inside which is x=%d\\n", x);
                  return 10;}')
fun_int()

Rcpp::cppFunction('
  int fun_add(int x, int y) {
    int z = x + y;
    Rprintf("%d + %d = %d\\n", x, y, z) ;
    return z;}')
fun_add(3, 4)

# error handling
Rcpp::cppFunction('
  // adds x and y, but only if they are positive
  int fun_add_pos(int x, int y) {
      // if x is negative, stop
      if(x<0) stop("x is negative");
      
      // if y is negative, stop
      if(y<0) stop("y is negative");
     
      return x + y;
  }
')

fun_add_pos(3, 2)
fun_add_pos(3, -2)
# note that traditional tryCatch can be used on the R side to capture errors.


# using rcpp files ----------------------------------------------------------------
# source a function fun_t2 from a .cpp file
Rcpp::sourceCpp("data/cpp_code_1.cpp")
fun_t2(3)

# notes on cpp syntax:
# - comments either using \\ or for multiple lines: /* comment body */
# - c++ can treat parts as an r code in a c++ file which is run after the c++ code: 
# /***R
# # this is an r comment
# */

# control objects in c++
Rcpp::sourceCpp("data/cpp_code_2.cpp")
cust_info(2)
cust_info(-2)


# loops ---------------------------------------------------------------------------
# looping in c++
# 4 parts in c++ loops: initialization, continue condition, increment, body
# for loop example

Rcpp::cppFunction("int forl(int x){
  // sum all numbers up to x
  int res=0;
  for(int i=0; i<=x; i++){
    // exit loop early if x<0
    if(x<0) break;
    res=res+i;
  }
  return res;
}")

forl(3)
forl(-3)

# while/do while loop examples
Rcpp::cppFunction("double sqrt_approx(double val, double thresh) {
  double x = 1.0;
  double x_0 = x;
  bool crit = false;
  
  while(crit==false) {
    x_0 = x;
    x = (x + val / x) / 2.0;
    crit = fabs(x - x_0) < thresh;
  }
  
  return x ;
}")

sqrt_approx(2, 0.001)

Rcpp::cppFunction("double sqrt_approx(double val, double thresh) {
  double x = 1.0;
  double x_0 = x;
  bool crit = false;
  
  do{
    x_0 = x;
    x = (x + val / x) / 2.0;
    crit = fabs(x - x_0) < thresh;
  } while(!crit);
  return x ;
}")

sqrt_approx(2, 0.001)


# vectors ---------------------------------------------------------------------------
# rcpp has the following vector classes:
# - NumericVector (eg. c(1, 2, 3))
# - IntegerVector (eg. 1:3)
# - LogicalVector (eg. c(TRUE, FALSE, TRUE))
# - CharacterVector (eg. c("a", "b", "c"))
# - Lists

# some important methods and points on these classes:
# - x.size() number of elements in vector
# - x[i] to extract the element
# - x[x.size()-1] for last element in vector
# - declaration: NumericVector numbers(10): 
# - when you copy a vector in c++, you need to clone it
# first if you don't what the initial vector to change: 
# NumericVector the_copy = clone(the_original);
# - note the efficiency of rcpp vector classes: 
# 1) thin wrappers around r vectors
# 2) data is copied every time you change the vector size
# one can use Standard Template Library (STL) vectors instead
# where the vector is not copied again and again


# working with elements of a vector
Rcpp::cppFunction("double vec_1stplst(NumericVector x) {
    int n = x.size();
    // 1st element
    double x_1 = x[0];
    // last element
    double x_n = x[n-1];
    return x_1 + x_n;
}")

x <- c(1, 12, 12, 20)
vec_1stplst(x)

# sum of a vector
Rcpp::cppFunction("double sum_cpp(NumericVector x) {
  int n = x.size();
  double z = 0;
  for(int i = 0; i<n; i++) {
    z = z + x[i];
  }
  return z;
}")

sum_cpp(1:100)

# construct different types of vectors
Rcpp::cppFunction('List con_vec() {
  // create unnamed character vector
  CharacterVector cv = CharacterVector::create("a", "b", "c");
  // create a named integer vector
  IntegerVector iv = IntegerVector::create(_["first"] = 1, _["second"] = 2, _["third"] = 3);
  // create a named list
  List lv = List::create(_["cv"] = cv, _["iv"] = iv);
  return lv;
}')

con_vec()

# weighted mean example
Rcpp::cppFunction("double wgt_mean_cpp(NumericVector x, NumericVector w) {
  double wt = 0;
  double p = 0;
  int n = x.size();
  
  for(int i = 0; i < n; i++) {
  // note: x += 3 means x = x + 3
  // if there is an NA, return NA
  if(NumericVector::is_na(x[i]) || NumericVector::is_na(w[i])) {
      return NumericVector::get_na();
    }
    wt += w[i];
    p += x[i] * w[i];
  }
  return p / wt;
}")

wgt_mean_cpp(x = c(1, 2, 3), w = c(0.2, 0.6, 0.4))
wgt_mean_cpp(x = c(1, 2, NA), w = c(0.2, 0.6, 0.4))


# using c++ STL vector classes
Rcpp::cppFunction("std::vector<double> ret_pos(NumericVector x) {
  // identify positive values and return them
  int n = x.size();
  std::vector<double> x_pos(0);
  
  for(int i = 0; i < n; i++) {
    if(x[i] > 0) {
      x_pos.push_back(x[i]);
    }
  }
  return x_pos;
}")

y <- ret_pos(x = rnorm(1000))


# some utility function examples ---------------------------------------------------------

# last non-na observation carried forward
Rcpp::cppFunction("NumericVector na_locf_fwd(NumericVector x) {
  double z = NumericVector::get_na();
  int n = x.size();
  NumericVector res = clone(x);
  for(int i = 0; i < n; i++) {
    if(NumericVector::is_na(x[i])) {
      res[i] = z;
    } else {
      z = x[i];
    }
  } 
  return res ;
}")

x <- stats::rnorm(1000)
x[sample(1000, size = 10)] <- NA
y <- na_locf_fwd(x = x)


# mean of last observations carried forward
Rcpp::cppFunction("NumericVector na_locf_mean(NumericVector x) {
  double z = 0.0;
  double n_not_na = 0.0;
  NumericVector res = clone(x);
  
  int n = x.size();
  for(int i = 0; i < n; i++) {
    if(NumericVector::is_na(x[i])) {
      res[i] = z / n_not_na;
    } else {
      z += x[i];
      n_not_na++;
    }
  }  
  return res;
}")

y <- na_locf_mean(x = x)


# an econometric arma(p, q) model
Rcpp::cppFunction('NumericVector arma(
  int n, 
  double mu, 
  NumericVector phi, 
  NumericVector theta, 
  double sd) {
  int p = phi.size();
  int q = theta.size();   
  NumericVector x(n);
  
    //  noise vector
    NumericVector eps = rnorm(n, 0.0, sd);
    // start at the max of p and q + 1
    int start = std::max(p, q) + 1;
    for(int i = start; i < n; i++) {
      // mean + noise
      double val = mu + eps[i];
      
      // ma(q) part
      for(int j = 0; j < q; j++) {
        val += theta[j] * eps[i - j - 1];
      }
      
      // ar(p) part
      for(int j = 0; j < p; j++) {
        val += phi[j] * x[i - j - 1];
      }
      
      x[i] = val;
    }
  return x;
}')

# simulate an arma process
mod <- arma(n = 50, mu = 5, phi = c(1, -0.2), theta = c(1, -0.2), sd = 1)
df <- data.frame(x = 1:50, y = mod)
ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_line()



