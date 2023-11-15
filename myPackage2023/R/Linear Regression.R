#' Fit a linear regression model with multiple independent variables
#'
#' @param formula Formula specifying the linear regression model.
#' @param data Data frame containing the variables.
#' @return Fitted linear model.
#'
#' @examples
#' fit <- fit_linear_model(y ~ x1 + x2, data = mydata)
#'
#' @export
fit_linear_model <- function(formula, data) {
  lm_model <- lm(formula, data = data)
  return(lm_model)
}

#' Predict using a linear regression model
#'
#' @param model Fitted linear model.
#' @param newdata New data for prediction.
#' @return Predicted values.
#'
#' @examples
#' predictions <- predict_linear_model(model, newdata = newdata)
#'
#' @export
predict_linear_model <- function(model, newdata) {
  predictions <- predict(model, newdata = newdata)
  return(predictions)
}

#' Get a summary of the linear regression model
#'
#' @param model Fitted linear model.
#' @return Model summary.
#'
#' @examples
#' summary <- model_summary(model)
#'
#' @export
model_summary <- function(model) {
  summary_stats <- summary(model)
  return(summary_stats)
}

#' Test the significance of the linear regression model
#'
#' @param model Fitted linear model.
#' @return Model significance.
#'
#' @examples
#' significance <- model_significance(model)
#'
#' @export
model_significance <- function(model) {
  anova_table <- anova(model)
  return(anova_table)
}
