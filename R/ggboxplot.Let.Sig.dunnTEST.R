#' A function for multiple group ggplot2 based boxplot using dunn.test
#'
#' @param data the data frame containing the grouping information
#' @param adjust adjust the p.value
#' @param set.levels boolean value for facet title
#' @param letterMark a boolean value to show LETTERS or not
#' @param unites the new levels for facet title text, usually containing two lines
#'
#' @return a ggplot boxplot
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggboxplot.Let.Sig.dunTEST(data = data, adjust = FALSE,
#' set.levels = TRUE, letterMark = TRUE,
#' unites = c("Sepal Length\n(g/L)", "Sepal Width\n(g/L)",
#'            "Petal Length\n(g/L)", "Petal Width\n(g/L)"))
ggboxplot.Let.Sig.dunTEST <- function(data, adjust = FALSE,
                                          set.levels = TRUE, letterMark = TRUE,
                                          unites = c("IS\n(g/L)", "PCS\n(g/L)",
                                                     "TMAO\n(g/L)", "3AA\n(g/L)")){
  stopifnot("group" %in% colnames(data))
  stopifnot(is.factor(data$group))
  ncol <- ncol(data)
  data.melt <- reshape2::melt(data, id.vars = "group")
  if(set.levels){
    levels(data.melt$variable) <- unites
  }
  # step 1 non-parametric dunntest
  variable <- colnames(data)[!colnames(data) %in% "group"] # delete the group variable
  n.grp <- nlevels(data$group)
  dunn <- ggroup::dunnTEST(data = data, method = "bh")
  ncol <- ncol(dunn)
  if(adjust){
    dunn <- dunn[, ((ncol/2)+1):ncol]
    colnames(dunn) <- gsub("altP.adjusted.", "", colnames(dunn))
    colnames(dunn) <- gsub(" ", "", colnames(dunn))
  }else{
    dunn <- dunn[, 1:(ncol/2)]
    colnames(dunn) <- gsub("altP.", "", colnames(dunn))
    colnames(dunn) <- gsub(" ", "", colnames(dunn))
  }
  # step 2 turn pairwise p.value to LETTER
  stopifnot(all(rownames(dunn) == variable))
  result <- c()
  for(i in 1:length(variable)){
    temp <- multcompView::multcompLetters(dunn[i, ])
    result <- cbind(result, temp$Letters)
  }
  colnames(result) <- variable
  test <- reshape2::melt(result)
  colnames(test) <- c("group","variable","value")
  if(set.levels){
    levels(test$variable) <- unites
  }
  # step 3 conculate the max value of each group
  test.max <- aggregate(.~group, data = data, max)
  test.max.melt1 <- reshape2::melt(test.max, id.vars = "group")
  if(set.levels){
    levels(test.max.melt1$variable) <- unites
  }
  for (i in 2:ncol(test.max)) {
    test.max[,i] <- test.max[,i] + max(test.max[,i])*0.1
  }
  # step 4 caculate the positions of LETTERS
  test.max.melt2 <- reshape2::melt(test.max, id.vars = "group")
  if(set.levels){
    levels(test.max.melt2$variable) <- unites
  }

  test.max.mix <- merge(test.max.melt1, test.max.melt2,
                        by = c("variable","group"))
  test.data <- merge(test, test.max.mix,
                     by = c("variable","group"))
  # step 5 make boxplot
  p <- ggplot2::ggplot(data = data.melt, aes(x = group, y = value)) +
    stat_boxplot(geom = 'errorbar', width = 0.25, position=position_dodge(width=1))+
    geom_boxplot(aes(fill = group),
                 position=position_dodge(width=1), width = 0.5, outlier.size = 0.5) +
    facet_wrap(.~variable, nrow = 1, scales = "free")+
    xlab(NULL) + ylab(NULL)
  # add the LETTER mark
  if (letterMark) {
    p <- p +
      geom_text(data = test.data, aes(x = group, y = value.y, label = value),
                size = 5, color = "#ea4f93", fontface = "bold")
  }
  p <- p +
    theme(panel.grid = element_line(color = 'gray90', size = 0.1),
          panel.background = element_rect(color = 'gray60',fill = 'transparent', size = 1),
          plot.title = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 14, face = "bold", color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.y = element_text(size = 14, face = "bold"),
          legend.text = element_blank(),
          legend.title = element_blank(),
          axis.title.x = element_text(size = 14, face = "bold", angle = 0, hjust = 0.5),
          legend.position = 0) +
    theme(strip.text = element_text(face="bold", size=rel(1.1)),
          strip.background = element_rect(fill = "#d6fff655", colour = "black",
                                          size = 1))
  return(p)
}

