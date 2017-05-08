setGeneric("show_all_items_info", function(obj, ...) {
    standardGeneric("show_all_items_info")
})

#' Show information of all items in a log for summary
#'
#' \emph{show_all_items_info} extracts informations about each item including the date of transaction, type of transaction, category, and model inside the log for later summary.
#'
#' @return A data frame containing informations about each item inside the log.
#'
#' @rdname show_all_items_info
#' @export
#' 
#' @examples \dontrun{show_all_items_info(a_log)}
setMethod("show_all_items_info",
          signature('product_logs'),
          function(obj, ...){
              ais <- data.frame()
              n <- length(obj@logs)
              if(n == 0)
                  return(ais)
              for(i in 1:n){
                  date <- obj@logs[[i]]@date
                  transac <- obj@logs[[i]]@transac
                  order_no <- obj@logs[[i]]@order_no
                  ai <- display(obj@logs[[i]]@descrp)[, c('category', 'model', 'price', 'quantity', 'amount')]
                  df <- data.frame(date, 
                                   transac,
                                   order_no,
                                   ai,
                                   stringsAsFactors = FALSE)
                  ais <- rbind(ais, df)
              }
              ais2 <- spread(ais, transac, amount, 0)
              ais2$order_no <- NULL
              return(ais2)
          }
)