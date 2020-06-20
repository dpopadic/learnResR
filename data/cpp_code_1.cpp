#include <Rcpp.h>
using namespace Rcpp;

// Export the function to R
// [[Rcpp::export]]
int fun_t2(int x){
  return 2*x;
}

// Call r code
/*** R
# call sum(3, 4) in r
sum(3, 4)
*/
