#' Draw a point&line plot using multiple variable according the grouping info.
#' Function 2.
#'
#' @param data A dataframe containing the grouping info and more than one numeric
#' variables. The structure of data likes iris.
#' @param type The type of plot, one of "simple" and "facet", the default type
#' is "simple".
#' @param fun.data The function used to draw the errorbar, one of "mean_sdl" and
#' "mean_se"
#' @param size the size of point
#'
#' @return a point plot
#' @export
#' @author Zhonghui Gai
#' @examples
#' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggpointplot(data = data, type = "facet", fun.data = mean_sdl) +
#' ggsci::scale_color_aaas() +
#' theme(legend.position = c(0.5, 0.1),
#'       legend.direction = "horizontal")

ggpointplot <- function(data,
                        type = "simple",
                        fun.data = mean_sdl,
                        size = 4){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }
  data.melt <- reshape2::melt(data = data, id.var = "group")
  # step1 The base plot ----
  p <- ggplot(data.melt, aes(x = variable, y = value,
                                   colour = group,
                                   group = group,
                                   shape = group)) +
    xlab(NULL) + ylab(NULL) + theme_bw() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 8, face = "bold"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          axis.text = element_text(size = 12, face = "bold",
                                   colour = "black"),
          panel.grid = element_line(colour = "grey92", size = 0.1),
          axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  # ----
  if (type == "simple") {
    p <- p + stat_summary(geom = "errorbar",
                          fun.data = fun.data, width = 0.1, show.legend = FALSE) +
      stat_summary(geom = "point", fun = mean, aes(fill = group), size = size) +
      stat_summary(geom = "line", fun = mean, aes(colour = group), size = 1, show.legend = FALSE)
  }
  if (type == "facet") {
   p <- p +
     stat_summary(geom = "errorbar", fun.data = fun.data,
                  width = 0.25, show.legend = FALSE,
                  position=position_dodge(width=0.5), show.legend= FALSE) +
     stat_summary(fun = mean, geom = "point", size = 1,
                  position = position_dodge(width = 0.5), size = size) +
     facet_wrap(. ~variable, nrow = 1, scales = "free_x") +
     theme_minimal() +
     theme(strip.text.x.top = element_text(angle = 0),
           legend.position = "bottom",
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.ticks.y = element_line(colour = "#4a4e4d"),
           axis.text = element_text(colour = "black", face = "bold"),
           panel.background = element_rect(fill = "white",
                                           colour = "white"),
           panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           panel.spacing = unit(0, "lines"),
           panel.grid.major.x = element_line(size = 0.1, colour = "gray95"),
           panel.border = element_rect(size = 0.2, colour = "black",
                                       fill = "transparent"),
           strip.background = element_rect(fill="#d6fff655",colour="gray60",
                                           size=0.2),
           strip.text = element_text(face = "bold", size = 8, hjust = 0),
           legend.text = element_text(face = "bold", size = 6),
           legend.title = element_blank(),
           legend.key.size = unit(0, "lines"),
           legend.background = element_rect(fill = "white",
                                            color = alpha("transparent", 0)))
  }
  return(p)
}
