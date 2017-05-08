setGeneric("order_track", function(obj, ...) {
    standardGeneric("order_track")
})

#' Create codes for the order numbers
#'
#' \emph{order_track} creates a new code for the order number for each transaction in the log.
#'
#' @return A vector of character strings, the new codes for the order number for each transaction in the log.
#'
#' @rdname order_track
#' @export
#' 
#' @examples \dontrun{order_track(a_log)}
setMethod("order_track",
          signature('product_logs'),
          function(obj, ...){
              sapply(obj@logs, function(t) gsub('[ ,\\-_@]', '', paste(t@date, substring(t@supplier_customer, 1,3), t@order_no, sep='')))
          })

#gsub('[ ,\\-_@]', '', paste(t@date, substring(t@supplier_customer, 1,3), t@order_no, sep=''))