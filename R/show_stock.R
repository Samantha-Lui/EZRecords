#' Show the stock of products
#'
#' \emph{show_stock} extracts the pertinent items' information for each item from each transaction record of a business log and returns the result for further calculation of the stock status of product. 
#'
#' @param obj An object.
#'
#' @rdname show_stock
#' @export
setGeneric("show_stock", function(obj, ...) {
    standardGeneric("show_stock")
})



#' @rdname show_stock
#' @include all_items.R
#' @include product_transac.R
#' @return The result of the extraction. 
#' @examples \dontrun{show_stock(business_logs)}
setMethod("show_stock",
          signature('product_logs'),
          function(obj, ...){
              stock <- data.frame()
              if(length(obj@logs)==0)
                  return(stock)
              for(nlogs in 1:length(obj@logs)){
                  for(nitems in 1:length(obj@logs[[nlogs]]@descrp@items)){
                      df <- data.frame(date=obj@logs[[nlogs]]@date,
                                       transac=obj@logs[[nlogs]]@transac,
                                       category=obj@logs[[nlogs]]@descrp@items[[nitems]]@category,
                                       model=obj@logs[[nlogs]]@descrp@items[[nitems]]@model,
                                       descrp=obj@logs[[nlogs]]@descrp@items[[nitems]]@descrp,
                                       quant=obj@logs[[nlogs]]@descrp@items[[nitems]]@quant,
                                       remark=ifelse(obj@logs[[nlogs]]@descrp@items[[nitems]]@sample=='Yes', 'SAMPLE',
                                                     ifelse(obj@logs[[nlogs]]@descrp@items[[nitems]]@sampling=='Yes', 'PRODUCTION SAMPLING',
                                                            '')),
                                       stringsAsFactors = FALSE)
                      stock <- rbind(stock, df, stringsAsFactors = FALSE)
                  }
              }
              stock$credit <- ifelse(stock$transac=='debit', stock$quant, 0)
              stock$debit <- ifelse(stock$transac=='credit', stock$quant, 0)
              stock$transac <- NULL
              stock$quant <- NULL
              stock <- stock[, c('category', 'model', 'descrp', 'date', 'credit', 'debit', 'remark')]
              return(stock)
          })