#' Product Transaction
#'
#' \emph{product_transac} is an S4 class to represent a transaction record of an order in a business transaction log.
#'
#' @slot date A Date object describing the date of the transaction.
#' @slot transac A character string describing the nature of the transaction with values 'credit' and 'debit'.
#' @slot catgory A character string representing whether the order is a 'purchase' or a 'sale'.
#' @slot descrp An \emph{all_items} object representing all items in the order.
#' @slot supplier_customer A character string representing the supplier/customer of the transaction.
#' @slot order_no A character string representing the order number of the transaction.
#' @slot value A numeric value representing the value of the items in the order.
#' @slot shipment A numeric value representing the handling and shipment cost for the order.
#' @slot total A  numeric value representing the total amount in the transaction.
#' @slot time_stamp A POSIXt object describing the time when the transaction record is created.
#'
#' @include all_items.R
#' @name product_transac-class
#' @rdname product_transac-class
#'
#' @examples
#' \dontrun{a_transaction <- new('product_transac', date = Sys.Date(), transac = 'credit',
#'
#' category = 'sale', descrp = shopping_cart, supplier_customer = 'Amazon', order_no = 'A12345',
#'
#' value = 100, tax = 9.75, shipment = 12.99, total = 122.74, time_stamp = Sys.time())}
#'
#' @export
#' @exportClass product_transac
setClass('product_transac',
         slots = c(date = 'Date',
                  transac = 'character',
                  category = 'character',
                  descrp = 'all_items',
                  supplier_customer = 'character',
                  order_no = 'character',
                  value = 'numeric',
                  tax = 'numeric',
                  shipment = 'numeric',
                  total = 'numeric',
                  time_stamp = "POSIXt"
         )
)

validProductTransac <- function(object) {
    if(identical(object@transac, character(0)) | identical(object@supplier_customer, character(0)) | identical(object@order_no, character(0)) |
       identical(object@category, character(0)) | identical(object@descrp, new('all_items')) |
       identical(object@value, numeric(0)) | identical(object@tax, numeric(0)) | identical(object@shipment, numeric(0) | identical(object@total, numeric(0))))
        return(TRUE)
    else{
        if(! object@transac %in% c('credit', 'debit'))
            paste('Transaction should be either \'credit\' or \'debit\'')
        else
            return(TRUE)
    }
}
## assign the function as the validity method for the class
setValidity('product_transac', validProductTransac)