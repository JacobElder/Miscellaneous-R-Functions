# This function is for comparing multiple MLM model fit indices at once. It will accept
# your model you want to compare, your model name that will go in the table (enter in
# quotations, such as "Model 1"), and the dataframe. If the dataframe parameter is left
# empty, the function will create a new dataframe and your model fit indices will
# be appended as the first row. You must have REML = FALSE for lmer for model fit
# indices to be provided and incorporated into this function.

modelSelectionTable <- function(model, modelName, modelDf) {
  if(missing(modelDf)) {
    if (all(names(summary(model)$AIC) != c("AIC", "BIC", "logLik", "deviance", "def.resid"))) {
      print("Run model with REML = FALSE and try again.")
    } else {
    modelDf <- data.frame(matrix(ncol = 6, nrow = 0))
    x <- c("Inventory","AIC", "BIC", "logLik", "deviance", "def.resid")
    colnames(modelDf) <- x
    modelDf <- rbind(modelDf, data.frame(Model = modelName, 
    AIC=summary(model)$AICtab[1], BIC=summary(model)$AICtab[2],
    logLik=summary(model)$AICtab[3], deviance=summary(model)$AICtab[4],
    def.resid=summary(model)$AICtab[5])) 
    }
  } else if (all(names(summary(model)$AIC) != c("AIC", "BIC", "logLik", "deviance", "def.resid"))) {
    print("Run model with REML = FALSE and try again.")
    } else {
    if (all(colnames(modelDf) == c("Model","AIC", "BIC", "logLik", "deviance", "def.resid")) & ncol(modelDf) == 6) {
      modelDf <- rbind(modelDf, data.frame(Model = modelName, 
      AIC=summary(model)$AICtab[1], BIC=summary(model)$AICtab[2],
      logLik=summary(model)$AICtab[3], deviance=summary(model)$AICtab[4],
      def.resid=summary(model)$AICtab[5]))
    } else {
      print("Dataframe needs to have consistent format. Leave second parameter empty and try again.")
    }
    }
  modelDf <- modelDf[order(modelDf$AIC, modelDf$BIC, modelDf$logLik, modelDf$deviance, modelDf$def.resid),]
  return(modelDf)
}