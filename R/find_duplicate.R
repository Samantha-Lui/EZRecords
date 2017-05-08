setGeneric("find_duplicate", function(obj, date, orn, sup, ...) {
    standardGeneric("find_duplicate")
})

#' Check if an order number has a duplicate in the database
#'
#' \emph{find_duplicate} checks if an order number has a duplicate in the database and returns the location of the duplicate if one exists.
#'
#' @param date A character string representing the date of the transaction.
#' @param orn A character string representing the order number of the transaction.
#' @param sup A character string representing the supplier of the transaction.
#' @return The location of the duplicate if one exists and character(0) otherwise.
#'
#' @rdname find_duplicate
#' @export
#' 
#' @examples \dontrun{find_duplicate(a_log, '2016-12-23', 'A123456', 'Amazon')}
setMethod("find_duplicate",
          signature('product_logs', 'character', 'character', 'character'),
          function(obj, date, orn, sup, ...){
              code <- gsub('[ ,\\-_@]', '', paste(date, substring(sup, 1,3), orn, sep=''))
              where <- which(order_track(obj) == code)
              return(where)}
)