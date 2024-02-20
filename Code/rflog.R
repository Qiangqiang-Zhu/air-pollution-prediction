#### Prediction Intervals for Random forests

rflog <- function(formula = NULL, train_data = NULL, test_data = NULL,
                  alpha = 0.05, params_ranger = NULL) {


  response_name <- as.character(terms(as.formula(formula), data = train_data)[[2]])

  params_ranger[c("oob.error", "keep.inbag", "quantreg")] <- list(TRUE, TRUE, TRUE)

  trainRF <- do.call(ranger::ranger, c(list(formula = formula), list(data = train_data), params_ranger))

  testPred <- predict(trainRF, test_data)$predictions
  oob_error <- train_data[, response_name] - trainRF$predictions
  oob_sigma <- sqrt(var(oob_error))

  quantreg_testPred <- predict(trainRF, test_data, type = "quantiles", quantiles = c(alpha/2, 1 - alpha/2))

  quantreg_interval <- data.frame(quantreg_testPred$predictions)

  colnames(quantreg_interval) <- c("lower", "upper")


  result <- list(interval = quantreg_interval, sigma = oob_sigma, testPred = testPred)

  return(result)
}
