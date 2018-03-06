#' @export
testOneBMN <-
function(X,
         lambda,
         alpha.global = 0.05,
         multiTest = TRUE,
         alpha.multi = 0.10) {
  tryCatch(
    BMN.onesample.wrapper(X, lambda, 
                          alpha.global = alpha.global,
                          multiTest = multiTest,
                          alpha.multi = alpha.multi),
    error = function(e) {
      print(paste("one multinomial or binomial class has 1 or 0 observations"))
      
      list(reject = NA,
           statistic = NA,
           pvalue = -99)
    }
  )
}
