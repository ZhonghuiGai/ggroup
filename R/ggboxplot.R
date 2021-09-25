#' Draw boxplot based on ggplot2, using data containing grouping info.
#' Only can draw one valuable. Function 1.
#'
#' @param data A dataframe that containing the grouping info and multiple numeric valiabes.
#' @param variable The column that will be used to draw the bar plot.
#' @param comparisons a list that indicated which group to be compared.
#' @param test The method that used for significant test, one of "t.test" and "wilcox.test".
#' The default method is "wilcox.test".
#' @param jitter A boolean value indicates whether add jitters points to the plot.
#' The default value is TRUE.
#' @param y_position A vector shows the values to be used in positions of the results
#' significant tests.
#' @param x.text.angle A numeric number indicated the angle of axix.test.x.
#' @param map_signif_level A logic value
#' @param tip_length the length of tip, default is 0
#'
#' @return The ggbarplot
#' @export
#' @author Zhonghui Gai
#' @examples
#' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggboxplot(data = data, variable = "Petal.Width",
#' comparisons = list(c("setosa", "versicolor"),
#'  c("versicolor", "virginica"),
#'   c("setosa", "virginica"))) +  ggsci::scale_fill_aaas()
ggboxplot <- function(data, variable,
                      comparisons = NULL,
                      test = "wilcox.test",
                      jitter = TRUE,
                      y_position = NULL,
                      x.text.angle = 30,
                      map_signif_level = TRUE,
                      tip_length = 0){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }

  p <- ggplot(data = data, aes(x = group, y = get(variable))) +
    stat_boxplot(geom = 'errorbar', width = 0.25,
                 position=position_dodge(width=1))+
    geom_boxplot(aes(fill = group),
                 position=position_dodge(width=1), width = 0.5,
                 outlier.size = 0.25) +
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

    if (map_signif_level) {
      map_signif_level <- function(p) sprintf("p = %.3f", p)
    }

    p <- p + ggsignif::geom_signif(comparisons = comparisons,
                                   test = test,
                                   y_position = y_position,
                                   size = 1,
                                   textsize = 5,
                                   family = "sans",
                                   fontface = "bold.italic",
                                   tip_length = tip_length,
                                   color = "#3c9eff",
                                   map_signif_level = map_signif_level,
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
