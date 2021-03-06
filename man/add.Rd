% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.R
\docType{methods}
\name{add}
\alias{add}
\alias{add,all_items,single_item-method}
\alias{add,product_logs,product_transac-method}
\title{Add an element}
\usage{
add(obj, item, ...)

\S4method{add}{all_items,single_item}(obj, item, ...)

\S4method{add}{product_logs,product_transac}(obj, item, ...)
}
\arguments{
\item{obj}{An object.}

\item{item}{An item to be added.}
}
\description{
\emph{add} appends an item to a structure within the object.
}
\examples{
\dontrun{add(shopping_cart, an_item) #A single_item is added to the first position of the list within the all_items object.}
\dontrun{add(transac_log, a_trnsac) #A product_transac is added to the first position of the list within the product_logs object.}
}
