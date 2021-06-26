#' Draw barplot based on ggplot2, using data containg grouping info.
#' Only can draw one valuable. Function 1.
#'
#' @param data A dataframe that containing the grouping info and multiple numaric valiabes.
#' @param variable The collum that will be used to draw the barplot.
#' @param comparisons a list that indicated which group to be compared.
#' @param test The method that used for significant test, one of "t.test" and "wilcox.test".
#' The default method is "wilcox.test".
#' @param jitter A boolean value indicates whether add jitter points to the plot.
#' The default value is TRUE.
#' @param y_position A vector shows the values to be used in positions of the results
#' significant tests.
#' @param x.text.angle A numaric number indicated the angle of axix.test.x.
#'
#' @return The ggbarplot
#' @export
#' @author Zhonghui Gai
#' @examples
#' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggbarplot(data = data, variable = "Petal.Width",
#' comparisons = list(c("setosa", "versicolor"),
#'  c("versicolor", "virginica"),
#'   c("setosa", "virginica"))) +  ggsci::scale_fill_aaas()
ggbarplot <- function(data, variable,
                      comparisons = NULL,
                      test = "wilcox.test",
                      jitter = TRUE,
                      y_position = NULL,
                      x.text.angle = 30){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }
  p <- ggplot(data = data, aes(x = group, y = get(variable))) +
    stat_summary(geom = "bar", fun = mean, aes(fill = group)) +
    stat_summary(geom = "errorbar", fun.data = mean_sdl, #fun.data = mean_se
                 width = 0.2) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(NA, 1.25*max(data[, variable]))) +
    xlab(NULL) + theme_bw() + ylab(variable) +
    theme(axis.text = element_text(size = 12, face = "bold", colour = "black"),
          axis.title = element_text(size = 14, face = "bold"),
          legend.position = 0)
  if (!is.null(comparisons)) {
    if (is.null(y_position)) {
      temp <- function(){
        formula <- as.formula(paste0(variable, "~", "group"))
        m <- aggregate(formula, data = data, max)
        n <- length(comparisons)
        ind <- sapply(1:n, function(x){m$group %in% comparisons[[x]]})
        sig.position <- sapply(1:n, function(x){max(m[ind[, x], ][, 2])})
        mul <- c(1, 1, 1.1, 1.15, 1.2, 1.25)
        sig.position <- sig.position*mul[1:n]
      }
      y_position <- temp()
    }
     p <- p + ggsignif::geom_signif(comparisons = comparisons,
                                   test = test,
                                   y_position = y_position,
                                   size = 0.35,
                                   textsize = 4,
                                   map_signif_level = T,
                                   tip_length = 0.01,
                                   margin_top = 0.1)
  }
  if (jitter) {
    p <- p +
      geom_jitter(width = 0.3, fill = "white",
                  color = "gray60", shape = 21, size = 1)
  }
  if (x.text.angle != 0) {
   p <- p +
     theme(axis.text.x = element_text(angle = x.text.angle, vjust = 1, hjust = 1))
  }
  return(p)
}




