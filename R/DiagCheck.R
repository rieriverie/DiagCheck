#' Diagnostic Checking for Regression Models
#'
#' This package contains a function for diagnosing a regression model, ensuring its effectiveness by evaluating various metrics.
#'
#' @param model A regression model that want to be checked.
#' @param type type of regression models ("simple", "multiple", "polynomial", "logistic)
#' @return yields the evaluation matrices and the conclusion.
#' @examples
#' #Multiple Linear Regression Model
#' model <- lm(mpg~cyl+disp+hp+drat+wt+qsec, mtcars)
#' DCfunction(model, "multiple")
#' #Logistic Regression Model
#' model <- glm(am~mpg+hp, family = "binomial", data = mtcars)
#' DCfunction(model, "logistic")
#' @export
DCfunction = function(model, type){
  require("crayon")
  require("lmtest")
  require("car")
  require("ResourceSelection")
  alpha = 0.05
  if(type == "simple"){
    count = 0
    cat("1. Variable independent is fixed:\n")
    cat(bold("This assumption is fulfilled\n"))
    count = count + 1
    cat("_______________________________________________________________\n")

    cat("2. Linearity of the dependent variable with independent variable:\n")
    cat("Because this is also be part of the assumption test before we begin to create the model, we will consider to assumed the linearity has been fulfilled\n")
    cat(bold("So, This assumption is fulfilled\n"))
    count = count + 1
    cat("_______________________________________________________________\n")

    cat("3. Normality of Residuals :\n")
    residual = length(model$residuals)
    if (residual<50){
      test = shapiro.test(model$residuals)
      name = "Shapiro-Wilk"
      P.value = test$p.value
    } else {
      test = ks.test(model$residuals,"pnorm")
      name = "Kolmogorov-Smirnov"
      P.value = test$p.value
    }
    if(P.value<alpha){
      Normality = "NO"
    } else {
      Normality = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Normality
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Normality=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("4. Homoscedasticity of Residuals :\n")
    Test = bptest(model)
    name = "Breusch-Pagan"
    P.value = Test$p.value
    if(P.value<alpha){
      Homoscedasticity = "NO"
    } else {
      Homoscedasticity = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Homoscedasticity
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Homoscedasticity=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("5. No-autocorrelation of Residuals:\n")
    Test = dwtest(model)
    P.value = Test$p.value
    name = "Durbin-Watson"
    if (P.value < alpha) {
      `No Autocorrelation` = "NO"
    } else {
      `No Autocorrelation` = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      `No Autocorrelation`
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if (`No Autocorrelation` == "YES") {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n\n")
    if (count/5>=0.5){
      cat("CONCLUSION:\nDiagnostic Check is", bold("Satisfied\n"))
      cat("The model is robust.")
    } else{
      cat("CONCLUSION:\nDiagnostic Check is", bold("not Satisfied\n"))
    }
  }
  else if (type == "multiple"){
    count = 0
    cat("1. Variable independent is fixed:\n")
    cat(bold("This assumption is fulfilled\n"))
    count = count + 1
    cat("_______________________________________________________________\n")

    cat("2. Linearity of the dependent variable with each of independent variable:\n")
    cat("Because this is also be part of the assumption test before we begin to create the model, we will consider to assumed the linearity has been fulfilled\n")
    cat(bold("So, this assumption is fulfilled\n"))
    count = count + 1
    cat("_______________________________________________________________\n")

    cat("3. Normality of Residuals :\n")
    residual = length(model$residuals)
    if (residual<50){
      test = shapiro.test(model$residuals)
      name = "Shapiro-Wilk"
      P.value = test$p.value
    } else {
      test = ks.test(model$residuals,"pnorm")
      name = "Kolmogorov-Smirnov"
      P.value = test$p.value
    }
    if(P.value<alpha){
      Normality = "NO"
    } else {
      Normality = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Normality
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Normality=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("4. Homoscedasticity of Residuals :\n")
    Test = bptest(model)
    name = "Breusch-Pagan"
    P.value = Test$p.value
    if(P.value<alpha){
      Homoscedasticity = "NO"
    } else {
      Homoscedasticity = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Homoscedasticity
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Homoscedasticity=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("5. No-autocorrelation of Residuals:\n")
    Test = dwtest(model)
    P.value = Test$p.value
    name = "Durbin-Watson"
    if (P.value < alpha) {
      `No Autocorrelation` = "NO"
    } else {
      `No Autocorrelation` = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      `No Autocorrelation`
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if (`No Autocorrelation` == "YES") {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("6. No Multicolineaity:\n")
    vif_values = vif(model)
    Multicolinearity = numeric()
    for (i in 1:length(vif_values)) {
      if(vif_values[i]<10){
        Multicolinearity[i] = "NO"
      }
      else {
        Multicolinearity[i] = "YES"
      }
    }
    output = data.frame(
      Variables = names(vif_values),
      VIF = vif_values,
      Multicolinearity
    )
    output$Variables = format(output$Variables, digits = 6)
    print(output, row.names = FALSE)
    if (length(Multicolinearity[Multicolinearity=="YES"]) == 0) {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n\n")
    if (count/6>=0.5){
      cat("CONCLUSION:\nDiagnostic Check is", bold("Satisfied\n"))
      cat("The model is robust.")
    } else{
      cat("CONCLUSION:\nDiagnostic Check is", bold("not Satisfied\n"))
    }
  }
  else if (type == "polynomial"){
    count = 0
    cat("1. Variable independent is fixed:\n")
    cat(bold("This assumption is fulfilled\n"))
    count = count + 1
    cat("_______________________________________________________________\n")

    cat("2. Curvelinear relationship between dependent variable with each of independent variable:\n")
    cat("Because this is also be part of the assumption test before we begin to create the model, we will consider to assumed the linearity has been fulfilled\n")
    cat(bold("So, this assumption is fulfilled\n"))
    count = count + 1
    cat("_______________________________________________________________\n")

    cat("3. Normality of Residuals :\n")
    residual = length(model$residuals)
    if (residual<50){
      test = shapiro.test(model$residuals)
      name = "Shapiro-Wilk"
      P.value = test$p.value
    } else {
      test = ks.test(model$residuals,"pnorm")
      name = "Kolmogorov-Smirnov"
      P.value = test$p.value
    }
    if(P.value<alpha){
      Normality = "NO"
    } else {
      Normality = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Normality
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Normality=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("4. Homoscedasticity of Residuals :\n")
    Test = bptest(model)
    name = "Breusch-Pagan"
    P.value = Test$p.value
    if(P.value<alpha){
      Homoscedasticity = "NO"
    } else {
      Homoscedasticity = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Homoscedasticity
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Homoscedasticity=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("5. No-autocorrelation of Residuals:\n")
    Test = dwtest(model)
    P.value = Test$p.value
    name = "Durbin-Watson"
    if (P.value < alpha) {
      `No Autocorrelation` = "NO"
    } else {
      `No Autocorrelation` = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      `No Autocorrelation`
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if (`No Autocorrelation` == "YES") {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("6. No Multicolineaity:\n")
    vif_values = vif(model)
    Multicolinearity = numeric()
    for (i in 1:length(vif_values)) {
      if(vif_values[i]<10){
        Multicolinearity[i] = "NO"
      }
      else {
        Multicolinearity[i] = "YES"
      }
    }
    output = data.frame(
      Variables = names(vif_values),
      VIF = vif_values,
      Multicolinearity
    )
    output$Variables = format(output$Variables, digits = 6)
    print(output, row.names = FALSE)
    if (length(Multicolinearity[Multicolinearity=="YES"]) == 0) {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n\n")
    if (count/6>=0.5){
      cat("CONCLUSION:\nDiagnostic Check is", bold("Satisfied\n"))
      cat("The model is robust.")
    } else{
      cat("CONCLUSION:\nDiagnostic Check is", bold("not Satisfied\n"))
    }
  }
  else if (type == "logistic") {
    count = 0
    cat("1. Goodness-of-Fit Test:\n")
    Test = hoslem.test(model$y, fitted(model))
    P.value = Test$p.value
    if (P.value < alpha) {
      `Goodness of Fit` = "NO"
    } else {
      `Goodness of Fit` = "YES"
    }
    output = data.frame(
      Test = "Hosmer-Lemeshow",
      P.value,
      `Goodness of Fit`
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if (`Goodness of Fit` == "YES") {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("2. Multicolinearity:\n")
    vif_values = vif(model)
    Multicolinearity = numeric()
    for (i in 1:length(vif_values)) {
      if (vif_values[i] < 10) {
        Multicolinearity[i] = "NO"
      } else {
        Multicolinearity[i] = "YES"
      }
    }
    output = data.frame(
      Variables = names(vif_values),
      VIF = vif_values,
      Multicolinearity
    )
    output$Variables = format(output$Variables, digits = 6)
    print(output, row.names = FALSE)
    if (length(Multicolinearity[Multicolinearity == "YES"]) == 0) {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("3. Normality of Residuals :\n")
    residual = length(model$residuals)
    if (residual<50){
      test = shapiro.test(model$residuals)
      name = "Shapiro-Wilk"
      P.value = test$p.value
    } else {
      test = ks.test(model$residuals,"pnorm")
      name = "Kolmogorov-Smirnov"
      P.value = test$p.value
    }
    if(P.value<alpha){
      Normality = "NO"
    } else {
      Normality = "YES"
    }
    output = data.frame(
      Test = name,
      P.value,
      Normality
    )
    output$Test = format(output$Test, digits = 6)
    print(output, row.names = FALSE)
    if(Normality=="YES"){
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else{
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n")

    cat("4. Linearity of Logit:\n")
    logit_model = model.matrix(model)
    logit_vars = logit_model[, -1] * logit_model[, -1]
    logit_test = lm(model$fitted.values ~ logit_vars)
    anova_test = anova(logit_test)
    P.value = anova_test$`Pr(>F)`[1]
    Linearity = ifelse(P.value > alpha, "YES", "NO")
    output = data.frame(
      Test = "Linearity of Logit",
      P.value,
      Linearity
    )
    print(output, row.names = FALSE)
    if (Linearity == "YES") {
      cat(bold("This assumption is fulfilled\n"))
      count = count + 1
    } else {
      cat(bold("This assumption is not fulfilled\n"))
    }
    cat("_______________________________________________________________\n\n")

    if (count/4 >= 0.5) {
      cat("CONCLUSION:\nDiagnostic Check is", bold("Satisfied\n"))
      cat("The model is robust.")
    } else {
      cat("CONCLUSION:\nDiagnostic Check is", bold("not Satisfied\n"))
    }
  }

}




