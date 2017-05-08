#' Check if the Object Is Empty
#'
#' \emph{is_empty} checks if the object has only trivial slots and returns a boolean value for the result.
#'
#' @param obj An object
#'
#' @rdname is_empty
#' @export
setGeneric("is_empty", function(obj, ...) {
    standardGeneric("is_empty")
})

#' @rdname is_empty
#' @return A boolean value describing whether the object has any nontrivial slots.
setMethod('is_empty',
          signature('single_item'),
          function(obj){
              return(identical(obj, new('single_item')))
          }
)

#' @rdname is_empty
#' @return A boolean value describing whether the object has any nontrivial slots.
setMethod('is_empty',
          signature('all_items'),
          function(obj){
              return(identical(obj, new('all_items')))
          }
)

#' @rdname is_empty
#' @return A boolean value describing whether the object has any nontrivial slots.
setMethod('is_empty',
          signature('product_transac'),
          function(obj){
              return(identical(obj, new('product_transac')))
          }
)

#' @rdname is_empty
#' @return A boolean value describing whether the object has any nontrivial slots.
setMethod('is_empty',
          signature('product_logs'),
          function(obj){
              return(identical(obj, new('product_logs')))
          }
)