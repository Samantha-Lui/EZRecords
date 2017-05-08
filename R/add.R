#' Add an element
#'
#' \emph{add} appends an item to a structure within the object.
#'
#' @param obj An object.
#' @param item An item to be added.
#'
#' @rdname add
#' @export
setGeneric("add", function(obj, item, ...) {
    standardGeneric("add")
})

#' @rdname add
#' @include single_item.R
#' @examples \dontrun{add(shopping_cart, an_item) #A single_item is added to the first position of the list within the all_items object.}
setMethod('add',
          signature('all_items','single_item'),
          function(obj, item, ...){
              obj@items <- c(item, obj@items)
              obj
          })

#' @rdname add
#' @include product_transac.R
#' @examples \dontrun{add(transac_log, a_trnsac) #A product_transac is added to the first position of the list within the product_logs object.}
setMethod('add',
          signature('product_logs', 'product_transac'),
          function(obj, item, ...){
              obj@logs <- c(item, obj@logs)
              obj
          }
)