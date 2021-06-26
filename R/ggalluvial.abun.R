#' Draw a alluvim plot using ggplot2 and ggalluvial package
#'
#' @param data a data.frame abtained from summaryAll function
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' phylum <- read.csv("phylum.csv")
#' data <- phylum[, 1:7]
#' results <- data.frame(group = data$group, data[, rev(colnames(data)[-1])])
#' results <- summaryAll(results)
#' results$mean <- 100*results$mean
#' ggalluvial.abun(results) + theme(legend.position = "top") +
#' ggsci::scale_fill_aaas()
ggalluvial.abun <- function(data){
  if(!inherits(data, "summaryAll")){
    stop("The input data must be the result of the summaryAll function!")
  }
  p <- ggplot2::ggplot(data = data, # the stratum and alluvium are grouped by variable info.
                       aes(x = group, y = mean,
                           stratum = variable, alluvium = variable))
  p <- p + # make chongji layer first and then add the stratum layer
    ggalluvial::geom_alluvium(aes(fill = variable), alpha = 0.4, width = 0.6,
                              color = "gray99", size = 0.1) +
    ggalluvial::geom_stratum(aes(fill = variable), width = 0.6,
                             color = "gray99", size = 0.1) +
    ylab("Relative Abundance (%)") + xlab(NULL)

  p <- p +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(color = "gray60",
                                          fill = "transparent", size = 1),
          axis.text = element_text(size = 14, face = "bold", color = "black"),
          axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
          axis.title.y = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_blank(),
          legend.position = "left",
          panel.border = element_rect(colour = "black", fill = "transparent"))

  p <- p +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0))+
    scale_x_discrete(expand = c(0.06, 0.06))
  return(p)
}
