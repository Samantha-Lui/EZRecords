#' Remove the most last element
#'
#' \emph{remove_last} removes the most recently added item from a structure within the object.
#'
#' @param obj An object.
#'
#' @rdname remove_last
#' @export
setGeneric("remove_last", function(obj, ...) {
    standardGeneric("remove_last")
})


#' @rdname remove_last
setMethod('remove_last',
          signature('all_items'),
          function(obj, ...){
              if(length(obj@items) == 0)
                  return(obj)
              obj@items[[1]] <- NULL
              return(obj)
          }
)