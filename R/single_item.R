#' Single Item in an order
#'
#' \emph{single_item} is an S4 class to represent an item in an order from a business owner's perspective.
#'
#' @slot category A character string describing the category of the item.
#' @slot descrp A character string describing the item.
#' @slot model A character string representing the model of the item.
#' @slot quant A numeric value representing the quantity of the item in the order.
#' @slot price A numeric value representing the unit price of the item.
#' @slot discount A character string representing the discount in percentage with the '\%' symbol on the item.
#' @slot amount A numeric value representing the amount due for the item at the given unit price, quantity, and discount.
#' @slot sample A character string describing whether the item is a sample for a customer or not with values 'Yes' and 'No'.
#' @slot category A character string describing whether the item is a sampling item for the production or not with values 'Yes' and 'No'.
#'
#' @name single_item-class
#' @rdname single_item-class
#' @examples 
#' \dontrun{
#' 
#' an_item <- new('single_item', category = 'some category', descrp = 'some description', 
#' 
#' model = 'some model', quant = 1, price = 10, discount = '10%', amount = 9, sample = 'No', 
#' 
#' sampling = 'No')
#' 
#'}
#' 
#'
#' @export
#' @exportClass single_item
setClass('single_item',
         slots = c(category = 'character',
                  descrp = 'character',
                  model = 'character',
                  quant = 'numeric',
                  price = 'numeric',
                  discount = 'character',
                  amount = 'numeric',
                  sample = 'character',
                  sampling = 'character')
         )

validSingleItem <- function(object) {
    if(identical(object@category, character(0)) | identical(object@descrp, character(0)) | identical(object@model, character(0)) | 
       identical(object@discount, character(0)) | identical(object@sample, character(0)) | identical(object@sampling, character(0)) | 
       identical(object@quant, numeric(0)) | identical(object@price, numeric(0)) | identical(object@amount, numeric(0)))
        return(TRUE)
    else{
        if(as.integer(object@quant) != object@quant)
            paste("Quantity should be an integer")
        else{
            if(object@quant < 1)
                paste("Quantity should be a positive integer")
            else{
                if(object@discount != 'None' & !grepl('[0-9]?[0-9]%', object@discount))
                    paste("If there is a discount, it should be indicated by the number of percentage followed by a % symbol (without space)")
                else{
                    if(object@sample == 'Yes' & object@sampling == 'Yes')
                        paste("An item cannot be a sample for customer and a production sampling at the same time")
                    else
                        return(TRUE)
                    }
                }
        }
    }

}
## assign the function as the validity method for the class
setValidity('single_item', validSingleItem)