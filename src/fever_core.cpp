// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

//-----------------------------------------------------------------------------
//' @title Filter events within a series of data
//' @description The function loops through a vector and filters events whose
//' values exceed a given threshold within a given window.
//' @param x The vector to be filtered.
//' @param threshold The threshold. Filter rule applied is x(i) > threshold.
//' @param window The search window.
//' @return A 2-column matrix with start and end indices.
//' @export
// [[Rcpp::export(fever)]]
arma::mat fever(arma::vec x, double threshold, int window) {
  //-----------------------------------------------------------------------------

  // get the size of the vector to allocate the result matrix
  int n = x.size();

  // init event idx
  int event_idx = 0;
  // init event status
  bool event = FALSE;

  // create object of type armadillo matrix with length n and columns 2
  // column 1 will contain the start point of an event
  // column 2 will contain the end point of an event
  // each row represents an event
  arma::mat event_mat(n, 2, arma::fill::zeros);
  event_mat.fill(NA_REAL);

  // loop through the vector
  for(int i = 0; i < n; i++) {

    // an event starts, if
    // if the value excceeds the treshold AND
    // the event status is FALSE
    if ((x(i) > threshold) && (event == FALSE)) {
      // event status changes (begin)
      event = TRUE;
      // fill the result matrix with the event start index
      event_mat(event_idx, 0) = i + 1;
      // next index
      i += 1;
    }

    // WHILE the end of the (vector - window) is not reached AND
    // event is still TRUE
    while ( (i < (n - window) ) && (event == TRUE) ) {

      // an event ends, if
      // the maximum value within (i:i+window) is below the treshold
      if (arma::max(x.subvec(i, i + window)) <= threshold) {
        // event status changes (end)
        event = FALSE;
        // fill the result matrix with the event end index
        event_mat(event_idx, 1) = i + 1;
        // next event ID
        event_idx += 1;
      }
      // next index
      i += 1;

    }

  }

  if (event == TRUE) {
    Rcpp::warning("Incomplete event detected.");
  }

  // finally, we resize the original matrix to
  event_mat.resize(std::max(event_idx,1), 2);

  // return 2 column matrix with event start and end points...
  return(event_mat);

}

