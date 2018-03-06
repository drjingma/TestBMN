#' @export
testTwoBMN <-
  function(dat,
           lambda,
           alpha.global = 0.05,
           multiTest = TRUE,
           alpha.multi = 0.10) {
    tryCatch(
      BMN.twosample.wrapper(dat[[1]], dat[[2]], lambda[[1]], lambda[[2]], 
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
