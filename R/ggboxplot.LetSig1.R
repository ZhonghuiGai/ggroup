#' Similar function of ggboxplot.LetSig, only for one variable
#'
#' @param data A dataframe containing grouping info and only one numeric variables.
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggboxplot.LetSig1(data = data[, 4:5]) + theme(legend.position = 0)
ggboxplot.LetSig1 <- function(data,
                              facet = FALSE){
  ncol <- ncol(data)
  data.melt <- reshape2::melt(data, id.vars = "group")
  variable <- subset(colnames(data), colnames(data) != "group")
  # step1 make a matrix to store the significant results
  test <- matrix(NA, nrow = nlevels(data$group), ncol = (ncol -1))
  colnames(test) <- variable
  rownames(test) <- levels(data$group)
  for (i in variable) {
    fit1 <- aov(as.formula(sprintf("%s ~ group",i)), data = data)
    tuk1 <- multcomp::glht(fit1,linfct = multcomp::mcp(group="Tukey"))
    res1 <- multcomp::cld(tuk1,alpah=0.05)
    test[, i] <- res1$mcletters$Letters
  }
  test <- reshape2::melt(test)
  colnames(test) <- c("group","variable","value")
  # step2 make a function to calculate the max value

  # calculate the max value by group
  test.max <-aggregate(.~group, data, max)
  test.max.melt1 <- reshape2::melt(test.max, id.vars = "group")
  for (i in 2:ncol(test.max)) {
    test.max[,i] <- test.max[,i] + max(test.max[,i])*0.1
  }
  test.max.melt2 <- reshape2::melt(test.max, id.vars = "group")
  test.max.mix <- merge(test.max.melt1,test.max.melt2,
                        by = c("variable","group"))
  test.data <- merge(test, test.max.mix,
                     by = c("variable","group"))
  # step3 make boxplot
  p <- ggplot2::ggplot(data = data.melt, aes(x = group, y = value)) +
    stat_boxplot(geom = 'errorbar', width = 0.25,
                 position=position_dodge(width=1))+
    geom_boxplot(aes(fill = group),
                 position=position_dodge(width=1), width = 0.5,
                 outlier.size = 0.25) + xlab(NULL) + ylab(NULL)
  if (facet) {
    p <- p + facet_wrap(.~variable, nrow = 1, scales = "free")
  }
  # add Letter significant information
  p <- p +
    geom_text(data = test.data, aes(x = group, y = value.y, label = value),
              size = 5, color = "#ea4f93", fontface = "bold")
  # modify the theme
  p <- p +
    theme(panel.grid = element_line(color = 'gray90', size = 0.1),
          panel.background = element_rect(color = 'gray60',
                                          fill = 'transparent',
                                          size = 1),
          axis.text = element_text(size = 12, face = "bold", color = "black"),
          axis.text.x = element_text(colour = "black", size = 12, face = "bold",
                                     angle = 270, hjust = 0, vjust = 0.5),
          axis.title.y = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_blank(),
          legend.position = "right") +
    # geom_violin(position=position_dodge(width=1), alpha=0.1)+
    theme(strip.text = element_text(face="bold", size=rel(1)),
          strip.background = element_rect(fill="#d6fff655",colour="gray60",
                                          size=1))
  return(p)
}
