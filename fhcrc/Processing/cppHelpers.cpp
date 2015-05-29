#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
void appendRcpp(  List fillVecs, NumericVector newLengths, NumericMatrix retmat, NumericVector retmatLengths ) {
  /* appendRcpp
	Fills numeric matrix
	Loop through rows, filling retmat in with the vectors in list
	then update return matrix size to index the next free
	*/
	
	// Declare vars
	NumericVector fillTmp;
	int sizeOld, sizeNew;
    
	// Pull out dimensions of return matrix to fill
	int nrow = retmat.nrow();
  int ncol = retmat.ncol();
    
	// Check that dimensions match
	if ( nrow != retmatLengths.size() || nrow != fillVecs.size() ) { 
        throw std::range_error("In appendC(): dimension mismatch");
    }
    
	// Traverse ddimensions
	for (int ii=0; ii<ncol; ii++) {
        throw std::range_error("In appendC(): exceeded max cols");
		
	// Iterator for row to fill
        NumericMatrix::Row retRow = retmat(ii, _);
	
  // Fill row of return matrix, starting at first non-zero element
        std::copy( fillTmp.begin(), fillTmp.end(), retRow.begin() + sizeOld );
  
  // Update size of return matrix
        retmatLengths[ii] = sizeNew;
		}
	}