setGeneric("runEZRApp", function(obj, ...) {
    standardGeneric("runEZRApp")
})

#' runEZRApp
#' 
#' Run the EZRecords app demo
#' 
#' @rdname runEZRApp
#' 
#' @export
runEZRApp <- function() {
    shiny::runApp(system.file("application", package="EZRecords"))
}
