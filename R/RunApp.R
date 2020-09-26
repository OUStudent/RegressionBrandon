#' Runs the Shiny App for Two Knot PieceWise Regression
#'
#' This function calls the shiny app.
#'
#' @export
RunApp = function() {
  shiny::runApp( system.file("shinyApp", package = "AppliedRegressionBrandon"))
}
