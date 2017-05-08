#' Print Objects
#'
#' Rather than printing out its argument, \emph{print} siliently returns a representation of its argument in various forms such as character string and data frame.
#'
#' @param obj An object
#'
#' @rdname print
#' @export
setGeneric("print", function(obj, ...) {
    standardGeneric("print")
})


#' @rdname print
#' @return A character string describing the object. # for \emph{single_item} objects
setMethod("print",
          signature('single_item'),
          function(obj, ...) {
              d <- paste(obj@category, obj@model, sep = ':')
              d <- paste(d, obj@price, sep = ' @$')
              d <- paste(d, obj@quant, sep = ' x ')
              if(obj@discount!='None')
                  d <- paste(d, ' with ', obj@discount, 'off', sep = '')
              if(obj@sample!='No')
                  d <- paste(d, '(SAMPLE)', sep = ' ')
              if(obj@sampling!='No')
                  d <- paste(d, '(PRODUCTION SAMPLING)', sep = ' ')
              d
          }
)

#' @rdname print
#' @return A character string describing the object. # for \emph{all_items} objects
setMethod('print',
          signature('all_items'),
          function(obj, ...){
              if(length(obj@items)==0)
                  return('None')
              pt <- print(obj@items[[1]])
              if(length(obj@items)==1)
                  return(pt)
              for(i in 2:length(obj@items))
                  pt <- paste(pt, print(obj@items[[i]]), sep=', ')
              return(pt)
          }
)


#' @rdname print
#' @return A data frame describing the object. # for \emph{product_logs} objects
setMethod('print',
          signature('product_transac'),
          function(obj, ...){
              df <- data.frame(date = obj@date,
                               transac = obj@transac,
                               category = obj@category,
                               descrp = print(obj@descrp),
                               supplier_customer = obj@supplier_customer,
                               order_no = obj@order_no,
                               value = obj@value,
                               tax = obj@tax,
                               shipment = obj@shipment,
                               total = obj@total,
                               time_stamp = obj@time_stamp
              )
              df
          }
)


#' @rdname print
#' @return A data frame describing the object. # for \emph{product_logs} objects
setMethod('print',
          signature('product_logs'),
          function(obj, ...){
              df <- data.frame()
              if(length(obj@logs)==0)
                  return(df)
              for(i in 1:length(obj@logs))
                  df <- rbind(df, print(obj@logs[[i]]))
              df
          }
)
