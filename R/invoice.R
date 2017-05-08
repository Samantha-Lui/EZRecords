#' Invoice for an order
#'
#' An \emph{invoice} object handles order number generation and keeps the order numbers in a tracking system.
#'
#' @slot fname A character string indicating the name of the file in which thetracking system is stored.
#' @slot part1 A character string describing the first part of the invoice number pattern.
#' @slot part2 A character string describing the second part of the invoice number pattern.
#' @slot count_begins_at A character string indicating the number at which the count begins and the number of placeholders for the count.
#'
#' @name invoice-class
#' @rdname invoice-class
#' @examples 
#' \dontrun{
#' vt <- new('invoice', fname = 'invoice_test', part1 = 'abc', part2 = 'xyz', count_begins_at = '3146')
#'}
#' 
#' @export
#' @exportClass invoice
setClass('invoice',
         slots = c(fname = 'character',
                   part1 = 'character',
                   part2 = 'character',
                   count_begins_at = 'character'))


validInvoice <- function(object) {
    if(identical(object@fname, character(0)) |  identical(object@par1, character(0)) | 
       identical(object@part2, character(0)) |  identical(object@count_begins_at, character(0)))
        return(TRUE)
    else{
        s <- as.numeric(object@count_begins_at)
        if(is.na(s))
            paste('count_begins_at should be a character string consists of digits only')
        else
            return(TRUE)
    }
}
## assign the function as the validity method for the class
setValidity('all_items', validAllItems)

setGeneric("invoice", function(obj, ...) {
    standardGeneric("invoice")
})

#' Invoice generator for orders
#'
#' The \emph{invoice} method generates an order number for the transaction when a sale is made. An order number generated has a pattern consists of four parts: A customized first part, followed by the 2-digit represention of the year when the order number is generated, follow by a customized second part, and completed by the (possibly encryted) count of order number issued. *At the beginnig of the following years, the count will be reset to the \emph{count_begins_at} assigned at the construction of the object, with the 2-digit year modified.
#'
#' @param obj The invoice object.
#' @param is_sampling A boolean value indicating whether the transaction is in fact a sampling for the production or not.
#' @return A character string representation of the order number if it is indeed a sale and 'N/A' otherwise. 
#'
#' @examples \dontrun{invoice(vt, FALSE)}
#'
#' @rdname invoice
#' @export
setMethod('invoice',
          signature = c('invoice'),
          function(obj, is_sampling){
              if(is_sampling)
                  return('N/A')
              inv <- readRDS(paste(obj@fname, '.Rds', sep=''))
              v <- ''
              if(nrow(inv) == 0){ #brand new start
                  y <- substring(Sys.Date(), 3, 4)
                  v <- paste(obj@part1, y, obj@part2, obj@count_begins_at, sep='')
                  inv <- data.frame(v=v, t=Sys.time(), stringsAsFactors = FALSE)
                  saveRDS(inv, file=paste(obj@fname, '.Rds', sep=''))
                  return(v)
              }
              else{
                  np1 <- nchar(obj@part1)
                  yp <- as.numeric(substring(inv$v[nrow(inv)], np1+1, np1+2))
                  yc <- as.numeric(substring(Sys.Date(), 3, 4))
                  v <- inv$v[nrow(inv)]
                  if(yp < yc) # A new year; start the count over with the new year included in the invoice number.
                      v <- paste(obj@part1, substring(Sys.Date(), 3, 4), obj@part2, obj@count_begins_at, sep='')
                  else{
                      np2 <- nchar(obj@part2)
                      l <- np1 + 2 + np2
                      count <- as.numeric(substring(v, l+1, nchar(v)))
                      count <- count + 1
                      count <- formatC(count,
                                       width = nchar(obj@count_begins_at),
                                       format = 'd',
                                       flag = '0')
                      v <- paste(obj@part1, substring(Sys.Date(), 3, 4), obj@part2, count, sep='')
                  }
                  inv <- rbind(inv, data.frame(v=v, t=Sys.time()), stringsAsFactors = FALSE)
                  saveRDS(inv, file=paste(obj@fname, '.Rds', sep=''))
                  return(v)
              }
          })

setGeneric("invoice.demo", function(obj, ...) {
  standardGeneric("invoice.demo")
})

#' Demo for invoice generator for orders
#'
#' The \emph{invoice.demo} method generates an order number for the transaction when a sale is made. An order number generated has a pattern consists of four parts: A customized first part, followed by the 2-digit represention of the year when the order number is generated, follow by a customized second part, and completed by the (possibly encryted) count of order number issued. *At the beginnig of the following years, the count will be reset to the \emph{count_begins_at} assigned at the construction of the object, with the 2-digit year modified.
#'
#' @param obj The invoice object.
#' @param is_sampling A boolean value indicating whether the transaction is in fact a sampling for the production or not.
#' @param inv_df A data frame containing a log of the invoice generated. 
#' @return A list containing a character string representation of the order number if it is indeed a sale and 'N/A' otherwise and an undated log of generated invoices. 
#'
#' @examples \dontrun{invoice.demo(vt, FALSE)}
#'
#' @rdname invoice.demo
#' @export
setMethod('invoice.demo',
          signature = c('invoice'),
          function(obj, is_sampling, inv){
            if(is_sampling)
              return('N/A')
            
            #inv <- readRDS(paste(obj@fname, '.Rds', sep=''))
            v <- ''
            if(nrow(inv) == 0){ #brand new start
              y <- substring(Sys.Date(), 3, 4)
              v <- paste(obj@part1, y, obj@part2, obj@count_begins_at, sep='')
              inv <- data.frame(v=v, t=Sys.time(), stringsAsFactors = FALSE)
              #saveRDS(inv, file=paste(obj@fname, '.Rds', sep=''))
              return(list(v, inv))
            }
            else{
              np1 <- nchar(obj@part1)
              yp <- as.numeric(substring(inv$v[nrow(inv)], np1+1, np1+2))
              yc <- as.numeric(substring(Sys.Date(), 3, 4))
              v <- inv$v[nrow(inv)]
              if(yp < yc) # A new year; start the count over with the new year included in the invoice number.
                v <- paste(obj@part1, substring(Sys.Date(), 3, 4), obj@part2, obj@count_begins_at, sep='')
              else{
                np2 <- nchar(obj@part2)
                l <- np1 + 2 + np2
                count <- as.numeric(substring(v, l+1, nchar(v)))
                count <- count + 1
                count <- formatC(count,
                                 width = nchar(obj@count_begins_at),
                                 format = 'd',
                                 flag = '0')
                v <- paste(obj@part1, substring(Sys.Date(), 3, 4), obj@part2, count, sep='')
              }
              inv <- rbind(inv, data.frame(v=v, t=Sys.time()), stringsAsFactors = FALSE)
              #saveRDS(inv, file=paste(obj@fname, '.Rds', sep=''))
              return(list(v, inv))
            }
          })