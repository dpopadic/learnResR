#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void cust_info(int x){
  if(x > 0){
    Rprintf("x is positive");
  } else if(x == 0){
    Rprintf("x is zero");
  } else if(x < 0){
    Rprintf("x is negative");
  } else{
    Rprintf("x is not a number");
  }
}
