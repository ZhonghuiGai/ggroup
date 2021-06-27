#' Boxplot based on ggplot2 of two groups and multiple numeric variables
#'
#' @param data a data frame containing grouping information which only have two groups
#' @param violin boolean value to indicate whether to show violin plot
#' @param test the method used to compare, one of wilcox.test or t.test
#' @param nrow the number of rows
#'
#' @author ZhonghuiGai
#' @return a ggplot2 plot
#' @export
#'
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' data <- data[1:100, ]  |> droplevels()
#' ggboxplot_2grp(data)
ggboxplot_2grp <- function(data, violin = TRUE,
                           test = "wilcox.test",
                           nrow = 1){
  name <- levels(data$group) |> as.character()
  data.melt <- reshape2::melt(data = data, id.vars = "group")
  split.data <- split(data.melt, data.melt$variable)
  # calculate the adjusted p values
  if (test == "wilcox.test") {
    p.adjust <- sapply(split.data, function(x){
      pval <- wilcox.test(x[x$group == name[1], "value"],
                          x[x$group == name[2], "value"])$p.value
      pval <- p.adjust(pval, method = "BH")
      paste0("p.adj = ", round(pval, digits = 3))
    })
  }else if (test == "t.test") {
    p.adjust <- sapply(split.data, function(x){
      pval <- t.test(x[x$group == name[1], "value"],
                          x[x$group == name[2], "value"])$p.value
      pval <- p.adjust(pval, method = "BH")
      paste0("p.adj = ", round(pval, digits = 3))
    })
  }
  # add p value to the variables
  if(length(name) == 2){
    data.melt$variable <- as.factor(data.melt$variable)
    levels(data.melt$variable) <- paste0(levels(data.melt$variable), "\n", p.adjust)
  }
  # base plot
  if (violin) {box.width = 0.15}else{box.width = 0.5}
  p <- ggplot(data = data.melt, aes(x = group, y = value, fill = group)) +
    stat_boxplot(geom = "errorbar", width = box.width/2,
                 position = position_dodge(width = 1)) +
    geom_boxplot(aes(fill=group), position = position_dodge(width = 1),
                 width = box.width, outlier.size = 0.3) +
    xlab(NULL) + ylab(NULL) +
    facet_wrap(~ variable, scales = "free", nrow = nrow,
               shrink = F, strip.position = "top")
  if (violin) {
    p <- p + geom_violin(position=position_dodge(width = 1), alpha = 0.1)
  }
  p <- p + theme(panel.grid = element_line(color = 'gray90', size = 0.1),
          panel.background = element_rect(color = 'black',
                                          fill = 'transparent', size = 1),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold",
                                      angle = 0, hjust = 0.5),
          legend.position = 0,
          panel.border = element_rect(colour = "black", fill = "transparent")) +
    theme(strip.text = element_text(face="bold", size = rel(1)),
          strip.background = element_rect(fill = "#d6fff655", colour = "black",
                                          size = 1))
  return(p)
}
