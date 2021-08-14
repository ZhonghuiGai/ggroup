#' A function for non-parametric multi-group test using dunn.test package
#'
#' @param data a data frame with grouping information
#' @param method adjusts the p-value for multiple comparisons
#'
#' @return a data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' dunnTEST(data = data, method = "bh)
dunnTEST <- function(data, method = "bh"){
  name <- colnames(data)
  stopifnot("group" %in% name)
  results <- NULL
  data <- as.data.frame(data)
  name <- name[!name %in% "group"]
  for(i in name){
    dunn <- with(data,
                 dunn.test::dunn.test(data[, i], group,
                                      method = method,
                                      kw = TRUE, table = FALSE,
                                      list = TRUE, altp = TRUE))
    dunn <- data.frame(dunn[-which(names(dunn)=="chi2")])[, c(4,2,3)]
    rownames(dunn) <- dunn$comparisons
    dunn <- dunn[, -1]
    num <- length(rownames(dunn))
    dunn <- t(dunn)
    # dunn$variable <- rep(name[1], num)
    result <- matrix(NA, nrow = 1, ncol = 2*num, byrow = TRUE)
    colnames(result) <- c(paste(rownames(dunn)[1], colnames(dunn), sep = "."),
                          paste(rownames(dunn)[2], colnames(dunn), sep = "."))
    rownames(result) <- i
    result[1, 1:num] <- dunn[1,]
    result[1, (num+1):(2*num)] <- dunn[2, ]
    results <- rbind(results, result)
  }
  if (FALSE) {
    write.table(results, file = "dunn.txt", sep = "\t", quote = FALSE)
  }
  return(results)
}






