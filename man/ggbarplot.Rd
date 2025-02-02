% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggbarplot.R
\name{ggbarplot}
\alias{ggbarplot}
\title{Draw barplot based on ggplot2, using data containg grouping info.
Only can draw one valuable. Function 1.}
\usage{
ggbarplot(
  data,
  variable,
  comparisons = NULL,
  test = "wilcox.test",
  jitter = TRUE,
  y_position = NULL,
  x.text.angle = 30
)
}
\arguments{
\item{data}{A dataframe that containing the grouping info and multiple numaric valiabes.}

\item{variable}{The collum that will be used to draw the barplot.}

\item{comparisons}{a list that indicated which group to be compared.}

\item{test}{The method that used for significant test, one of "t.test" and "wilcox.test".
The default method is "wilcox.test".}

\item{jitter}{A boolean value indicates whether add jitter points to the plot.
The default value is TRUE.}

\item{y_position}{A vector shows the values to be used in positions of the results
significant tests.}

\item{x.text.angle}{A numaric number indicated the angle of axix.test.x.}
}
\value{
The ggbarplot
}
\description{
Draw barplot based on ggplot2, using data containg grouping info.
Only can draw one valuable. Function 1.
}
\examples{
library(ggplot2)
data <- iris
colnames(data)[5] <- "group"
ggbarplot(data = data, variable = "Petal.Width",
comparisons = list(c("setosa", "versicolor"),
 c("versicolor", "virginica"),
  c("setosa", "virginica"))) +  ggsci::scale_fill_aaas()
}
\author{
Zhonghui Gai
}
