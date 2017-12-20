#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix mySweep(const NumericMatrix B, int m) {
  
  NumericMatrix A = clone(B);
  int n = A.nrow();
  for (int k = 0; k < m; k++){
    for (int i = 0; i < n; i++){
      for (int j = 0; j < n; j++){
        if ((i != k) & (j != k))
          A(i,j) = A(i,j) - A(i,k)*A(k,j)/A(k,k);
      }
    }
    for (int i = 0; i < n; i++){
      if (i!=k)
        A(i,k) = A(i,k)/A(k,k);
    }
    
    for (int j = 0; j < n;j++){
      if (j != k)
        A(k,j) = A(k,j)/A(k,k);
      
    }
    A(k,k) = -1/A(k,k);
  }
  
  return A;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
*/
