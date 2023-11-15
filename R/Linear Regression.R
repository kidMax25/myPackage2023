#' This Package Fits a linear model with the response and predictor vars
#' Dependencies: broom, plotly, car, lmtest, dplyr
options(quiet = T, warn = -1)
# if(!require(broom)) {
#   install.packages("broom")

#' @title Build Regression Model
#' @param formula linear regression formula
#' @param data Data frame object
#' @return linear model.
#' @examples
#' fit <- myModel(y ~ x1 + x2, data = mydata)
#' @export
myModel <- function(formula, data) {
  lm_model <- lm(formula, data = data)
  return(lm_model)
}

#' @title Get the model estimates in an easier view
#' @param model Linear Model
#' @return as tibble
#' @examples
#' myModel.Results(fit)
#'
#' @export
myModel.Results <- function(model){
broom::tidy(model)
}

#' Predict using a linear regression model
#'
#' @param model Fitted linear model.
#' @param newdata data for prediction.
#' @return Predicted values.
#'
#' @examples
#' predictions <- myModel.Predict(model, newdata = newdata)
#'
#' @export
myModel.Predict <- function(model, newdata) {
  predictions <- predict(model, newdata = newdata)
  return(predictions)
}

#' @title Tidier Predictions

#' @param x list file ran from myModel.Predict to a tidy form
#' @return class tible of the predictions in tidy form
#' @examples
#' tidyPred <- tibble.Predict(predictions)
#' @export
tibble.Predict <- function(x){
  x <- as.list(x)
  broom::tidy(x)
}

#' Get a summary of the linear regression model
#'
#' @param model Fitted linear model.
#' @return Model summary.
#'
#' @examples
#' summary <- myModel.Summary(model)
#'
#' @export
myModel.Summary <- function(model) {
  summary_stats <- summary(model)
  return(broom::tidy(summary_stats))
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
myModel.Significance <- function(model) {
  anova_table <- anova(model)
  return(anova_table)
}

#' @title Model Report
#' @param model fitted model
#' @return Reported Values for the model
#' @export
myModel.Report <-function(model){
valVIF <- car::vif(model)
hetTest <- lmtest::bptest(model)
nomTest <- shapiro.test(model$residuals)

report <- paste(
    "Multicollinearity value is", round(valVIF[[1]],4), ",",
    "The Heteroscedasticity by Breusch-Pagan test yields",round(hetTest$statistic[[1]],4),",", "with p.value", round(hetTest$p.value[[1]],4),",",
    "Normality of the Residuals by Shapiro.Wilk is ", round(nomTest$statistic[[1]], 4),"with p.value", round(nomTest$p.value,4)
  )
return(report[[1]])
}

#' @title Model Diagnosed
#' @param model fitted model
#' @return plotly plots
#' @examples
#' myModel.Diagnose(model = fit)
#'
#' @export
myModel.Diagnose <-function(model){
  require(plotly)
    # Extract fitted values from lm() object
    Fitted.Values <-  fitted(model)

    # Extract residuals from lm() object
    Residuals <-  resid(model)

    # Extract standardized residuals from lm() object
    Standardized.Residuals <- MASS::stdres(model)

    # Extract fitted values for lm() object
    Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x

    # Square root of abs(residuals)
    Root.Residuals <- sqrt(abs(Standardized.Residuals))

    # Calculate Leverage
    Leverage <- lm.influence(model)$hat

    # Create data frame
    # Will be used as input to plot_ly

    diagnose <- data.frame(Fitted.Values,
                         Residuals,
                         Standardized.Residuals,
                         Theoretical.Quantiles,
                         Root.Residuals,
                         Leverage)

    # Plot using Plotly

    # Fitted vs Residuals
    # For scatter plot smoother
    LOESS1 <- loess.smooth(Fitted.Values, Residuals)

    plt1 <- diagnose %>%
      plot_ly(x = Fitted.Values, y = Residuals,
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

      add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "lines+markers", name = "Smooth",
                line = list(width = 2)) %>%

      layout(title = "Residuals vs Fitted Values")

    # QQ Pot
    plt2 <- diagnose %>%
      plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals,
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

      add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "lines+markers", name = "",
                line = list(width = 2)) %>%

      layout(title = "Q-Q Plot")

    # Scale Location
    # For scatter plot smoother
    LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)

    plt3 <- diagnose %>%
      plot_ly(x = Fitted.Values, y = Root.Residuals,
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

      add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "lines+markers", name = "Smooth",
                line = list(width = 2)) %>%

      layout(title = "Scale Location")

    # Residuals vs Leverage
    # For scatter plot smoother
    LOESS3 <- loess.smooth(Leverage, Residuals)

    plt4 <- diagnose %>%
      plot_ly(x = Leverage, y = Residuals,
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>%

      add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "lines+markers", name = "Smooth",
                line = list(width = 2)) %>%

      layout(title = "Leverage vs Residuals")

    plt = list(plt1, plt2, plt3, plt4)
    return(plt)

}

#' @title Model Suggestion
#' @param model fitted model
#' @return Suggested Changes
#' @examples
#' mylnear.Suggest(model = fit)
#'
#' @export
mylnear.Suggest <- function(model){
mymodel <- broom::tidy(model)
less_significant <- mymodel %>%
  dplyr::filter(p.value>0.05) %>%
  dplyr::pull(term)
print(paste("The following Variables are less significant in the model", less_significant[],",","consider changing entry method with `base::lm`"))
}

#' @title Data Description and Distribution
#' @param data dataframe
#' @return Descriptive tables and Distribution
#' @examples
#' mylnear.Describe(data = mydata)
#' @export
mylnear.Describe <- function(data){
  data %>% summary(.)
}
