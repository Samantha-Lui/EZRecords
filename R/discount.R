#' Discount of an item
#'
#' \emph{discount} returns the remaining fraction a value after it is discounted.
#'
#' @param obj An object.
#' @param item An item to be added.
#'
#' @rdname discount
#' @export
setGeneric("discount", function(obj, ...) {
    standardGeneric("discount")
})


#' @rdname discount
#' @return The remaining fraction a value in decimal after the single item is discounted.
setMethod('discount',
          signature('single_item'),
          function(obj, ...){
              if(obj@discount=='None')
                  return(1)
              d <- (100-as.numeric(gsub('%', '', obj@discount)))/100
              return(d)
          }
)