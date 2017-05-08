setGeneric("show_duplicate", function(obj, date, orn, sup, ...) {
    standardGeneric("show_duplicate")
})

#' Display the record with the same order number
#'
#' \emph{"show_duplicate"} displays the existing record of transaction in the log who has the same order number as the one entered by the user.
#'
#' @param date A character string representing the date of the transaction.
#' @param orn A character string representing the order number of the transaction.
#' @param sup A character string representing the supplier of the transaction.
#' @return The existing record of transaction in a data frame if there is a duplicate and an empty data frame otherwise.
#'
#' @rdname show_duplicate
#' @export
#' 
#' @examples \dontrun{show_duplicate(a_log, '2016-12-23', 'A123456', 'Amazon')}
setMethod("show_duplicate",
          signature('product_logs', 'character', 'character', 'character'),
          function(obj, date, orn, sup, ...){
              where <- find_duplicate(obj, date, orn, sup)
              if(identical(where, integer(0)))
                  return(list(character(0), data.frame()))
              else{
                  return(list('<font size=\'4\', color=\"#C65555\"><b>Order number already existed in the following record:</b></font>', print(obj)[where, ]))
              }
          }
)