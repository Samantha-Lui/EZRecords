#' Display Content
#'
#' Similar to \emph{print}, but \emph{display} shows the content of an object in a more viewer-friendly format while \emph{print} provides a compact form of the content for bookkeeping.
#'
#' @param obj An object
#'
#' @rdname display
#' @export
setGeneric("display", function(obj, ...) {
    standardGeneric("display")
})


#' @rdname display
#' @return A data frame describing the object. # for \emph{all_items} objects
setMethod('display',
          signature('all_items'),
          function(obj, ...){
              df <- data.frame()
              if(length(obj@items) == 0)
                  return(df)
              for(i in 1:length(obj@items)){
                  df1 <- data.frame(category = obj@items[[i]]@category,
                                    #description = print(obj@items[[i]]),
                                    description = obj@items[[i]]@descrp,
                                    model = obj@items[[i]]@model,
                                    price = obj@items[[i]]@price,
                                    quantity = obj@items[[i]]@quant,
                                    discount = obj@items[[i]]@discount,
                                    amount = obj@items[[i]]@amount,
                                    sample = obj@items[[i]]@sample,
                                    prod_sampling = obj@items[[i]]@sampling)
                  df <- rbind(df, df1)
              }
              return(df)
          })