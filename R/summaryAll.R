#' Summarizes data
#'
#' @param data A data frame containing the group collum for grouping info
#'
#' @return a data frame containing count, max, mean, standard deviation,
#' standard error of mean, 95% confidence interval
#' @export
#'
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' summaryAll(data)
summaryAll <- function(data){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }
  n = aggregate(.~group, data = data, length)
  n.melt <- reshape2::melt(data = n,
                           id.var = "group", value.name = "n")
  N = aggregate(.~group, data = data, function(x) sum(!is.na(x)))
  N.melt <- reshape2::melt(data = N,
                           id.var = "group", value.name = "N")
  max <- aggregate(.~group, data = data, max)
  max.melt <- reshape2::melt(data = max,
                             id.var = "group", value.name = "max")
  sd <- aggregate(.~group, data = data, sd)
  sd.melt <- reshape2::melt(data = sd,
                            id.var = "group", value.name = "sd")
  mean <- aggregate(.~group, data = data, mean)
  mean.melt <- reshape2::melt(data = mean,
                              id.var = "group", value.name = "mean")
  df <- Reduce(function(x, y) merge(x, y, by = c("group", "variable")),
               list(n.melt, N.melt, max.melt, sd.melt, mean.melt))
  df$se <- df$sd/sqrt(df$N)
  ciMult <- qt(0.95/2 + 0.5, df$N - 1)
  df$ci <- df$se * ciMult
  class(df) <- c("summaryAll", "data.frame")
  return(df)
}
