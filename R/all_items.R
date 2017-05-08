#' All Items in an order
#'
#' \emph{all_items} is an S4 class to represent all of the items in an order from a business owner's perspective.
#'
#' @slot items A list of \emph{single_item} objects.
#'
#' @include single_item.R
#' @name all_items-class
#' @rdname all_items-class
#' @examples
#' \dontrun{shopping_cart <- new('all_items', items = list(items, item2))}
#'
#' @export
#' @exportClass all_items
setClass('all_items',
         slots = c(items = 'list') #list of 'single_item'
)

validAllItems <- function(object) {
    if(length(object@items) == 0)
        return(TRUE)
    else{
        s <- TRUE
        for(i in 1:length(object@items))
            s <- s & class(object@items[[i]])=='single_item'
        if(s == FALSE)
            paste('Items on the list should be a single_item object')
        else
            return(TRUE)
    }
}
## assign the function as the validity method for the class
setValidity('all_items', validAllItems)