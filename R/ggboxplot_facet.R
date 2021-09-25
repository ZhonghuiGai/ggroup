#' Draw box plot based on ggplot2, using data containing grouping info.
#' Can draw multiple variables
#'
#' @param data A data frame that containing the grouping info and multiple numeric variables.
#' @param comparisons a list that indicated which group to be compared.
#' @param test he method that used for significant test, one of "t.test" and "wilcox.test".
#' The default method is "wilcox.test".
#' @param x.text.angle A numeric number indicated the angle of axix.test.x.
#' @param map_signif_level A logic value
#' @param tip_length the length of tip, default is 0
#' @param textsize_sifnif the text size of signifcant
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggboxplot_facet(data, map_signif_level = T, comparisons = list(c("setosa", "versicolor"),
#' c("versicolor", "virginica"),
#' c("setosa", "virginica"))) + ggsci::scale_fill_aaas()
ggboxplot_facet <- function(data, comparisons = NULL,
                            test = "wilcox.test",
                            x.text.angle = 45,
                            map_signif_level = TRUE,
                            tip_length = 0,
                            textsize_sifnif = 4){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }
  if (map_signif_level) {
    map_signif_level <- function(p) sprintf("p = %.3f", p)
  } else {
    map_signif_level <- TRUE
  }
  data <- reshape2::melt(data = data, id.var = "group")
  data$variable <- as.factor(data$variable)
  library(ggplot2)
  p <- ggplot(data = data, aes(x = group, y = value, fill = group)) +
    stat_boxplot(geom = "errorbar", position = position_dodge(width = 0.5), width = 0.25) +
    geom_boxplot(position = position_dodge(width = 0.5), outlier.size = 0.3) +
    ggsignif::geom_signif(comparisons = comparisons,
                textsize = textsize_sifnif, test = test, step_increase = 0.1,
                map_signif_level = map_signif_level,
                tip_length = tip_length, color = "#3c9eff", fontface = "bold.italic")+
    facet_wrap(. ~ variable, nrow = 1, strip.position = "top", scales = "free") +
    xlab(NULL) + theme_bw() +
    theme(axis.text = element_text(size = 12, face = "bold", colour = "black"),
          axis.title = element_text(size = 14, face = "bold"),
          legend.position = 0,
          axis.text.x = element_text(angle = x.text.angle, vjust = 1, hjust = 1),
          strip.switch.pad.wrap = unit(2, "cm"),
          strip.placement = "out",
          panel.background = element_rect(colour = "black", fill = NA),
          panel.border = element_rect(colour = "black", fill = NA),
          strip.background = element_rect(colour = "white", fill = "#91D1C2"),
          strip.text = element_text(size = 12, face = "bold"))
  return(p)
}
