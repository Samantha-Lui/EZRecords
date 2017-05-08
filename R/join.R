setGeneric("join", function(obj, another, ...) {
    standardGeneric("join")
})

#' Append another log to a log
#'
#' \emph{join} appends another log to the calling object and returns a new \emph{product_logs} object with the combined logs. 
#'
#' @param another A product_logs object whose entries to be added to the main argument.
#' @return A new \emph{product_logs} object with the combined logs.
#'
#' @rdname join
#' @export
#' 
#' @examples \dontrun{join(log1, log2)}
setMethod("join",
          signature('product_logs', 'product_logs'),
          function(obj, another, ...){
              pl <- new('product_logs')
              pl@logs <- append(obj@logs, another@logs)
              return(pl)
          }
)