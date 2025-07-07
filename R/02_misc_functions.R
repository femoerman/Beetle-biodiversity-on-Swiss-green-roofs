#' Misc functions
#' functions that are helpers and can be useful in several contexts



#' create_vector_numbering_elements
#'
#' @param v the vector
#' @param start_value start value of numbering
#'
#' @return numeric vector containing numbering starting from start_value
#' @export
#'
#' @examples
create_vector_numbering_elements <- function(v, start_value) {
  res <- start_value:(length(v) + start_value-1)
  return(res)
}
