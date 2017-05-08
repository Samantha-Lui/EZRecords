#' Amount
#'
#' \emph{amount} calculates the value of the object and returns the result.
#'
#' @param obj An object.
#'
#' @rdname amount
#' @export
setGeneric("amount", function(obj, ...) {
    standardGeneric("amount")
})


#' @rdname amount
#' @return The value of a single_item.
#' @examples \dontrun{amount(an_item) #Calculates the value of a single_item: amount = quantity * unit price * discount(an_item).}
setMethod('amount',
          signature('single_item'),
          function(obj, ...){
              return(sum(obj@quant * obj@price * discount(obj)))
          }
)


#' @rdname amount
#' @include single_item.R
#' @return The values of all items in an order.
#' @examples \dontrun{amount(shopping_cart) #Calculates sum of the values of all items in shopping_cart: sum(amount(each of the items)).}
setMethod('amount',
          signature('all_items'),
          function(obj, ...){
              if(length(obj@items)==0)
                  return(0)
              return(sum(sapply(obj@items, amount)))
          }
)

