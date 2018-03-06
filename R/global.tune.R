#' @export
global.tune <-
  function(X,
           Lambda,
           gamma = 0.25) {
    tryCatch(
      global.tune.wrapper(X, Lambda, gamma = gamma),
      error = function(e) {
        print(paste("one multinomial or binomial class has 1 or 0 observations"))
        
        list(EBIC = matrix(NA, nrow(Lambda), ncol(Lambda)))
      }
    )
  }
