% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/product_transac.R
\docType{class}
\name{product_transac-class}
\alias{product_transac-class}
\title{Product Transaction}
\description{
\emph{product_transac} is an S4 class to represent a transaction record of an order in a business transaction log.
}
\section{Slots}{

\describe{
\item{\code{date}}{A Date object describing the date of the transaction.}

\item{\code{transac}}{A character string describing the nature of the transaction with values 'credit' and 'debit'.}

\item{\code{catgory}}{A character string representing whether the order is a 'purchase' or a 'sale'.}

\item{\code{descrp}}{An \emph{all_items} object representing all items in the order.}

\item{\code{supplier_customer}}{A character string representing the supplier/customer of the transaction.}

\item{\code{order_no}}{A character string representing the order number of the transaction.}

\item{\code{value}}{A numeric value representing the value of the items in the order.}

\item{\code{shipment}}{A numeric value representing the handling and shipment cost for the order.}

\item{\code{total}}{A  numeric value representing the total amount in the transaction.}

\item{\code{time_stamp}}{A POSIXt object describing the time when the transaction record is created.}
}}

\examples{
\dontrun{a_transaction <- new('product_transac', date = Sys.Date(), transac = 'credit',

category = 'sale', descrp = shopping_cart, supplier_customer = 'Amazon', order_no = 'A12345',

value = 100, tax = 9.75, shipment = 12.99, total = 122.74, time_stamp = Sys.time())}

}
