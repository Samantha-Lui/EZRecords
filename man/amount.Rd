% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amount.R
\docType{methods}
\name{amount}
\alias{amount}
\alias{amount,single_item-method}
\alias{amount,all_items-method}
\title{Amount}
\usage{
amount(obj, ...)

\S4method{amount}{single_item}(obj, ...)

\S4method{amount}{all_items}(obj, ...)
}
\arguments{
\item{obj}{An object.}
}
\value{
The value of a single_item.

The values of all items in an order.
}
\description{
\emph{amount} calculates the value of the object and returns the result.
}
\examples{
\dontrun{amount(an_item) #Calculates the value of a single_item: amount = quantity * unit price * discount(an_item).}
\dontrun{amount(shopping_cart) #Calculates sum of the values of all items in shopping_cart: sum(amount(each of the items)).}
}
