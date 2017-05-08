#' PRODUCT LOGS
#'
#' \emph{product_logs} is an S4 class to represent a business transaction log.
#'
#' @slot logs A list of \emph{product_transac} objects
#'
#' @include product_transac.R
#' @name product_logs-class
#' @rdname product_logs-class
#' @examples
#' \dontrun{a_log <- new('product_logs', logs = list(product_transaction1, product_transaction2))}
#'
#' @export
#' @exportClass product_logs
setClass('product_logs',
         slot = c(logs = 'list') # list of 'product_transac'
)

validProduct_Logs <- function(object) {
    if(length(object@logs) == 0)
        return(TRUE)
    else{
        s <- TRUE
        for(i in 1:length(object@logs))
            s <- s & class(object@logs[[i]])=='product_transac'
        if(s == FALSE)
            paste('Items on the list should be a product_transac object')
        else
            return(TRUE)
    }
}
## assign the function as the validity method for the class
setValidity('product_logs', validProduct_Logs)