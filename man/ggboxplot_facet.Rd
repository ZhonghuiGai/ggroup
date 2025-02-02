% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggboxplot_facet.R
\name{ggboxplot_facet}
\alias{ggboxplot_facet}
\title{Draw box plot based on ggplot2, using data containing grouping info.
Can draw multiple variables}
\usage{
ggboxplot_facet(
  data,
  comparisons = NULL,
  test = "wilcox.test",
  x.text.angle = 45,
  map_signif_level = TRUE,
  tip_length = 0,
  textsize_sifnif = 4
)
}
\arguments{
\item{data}{A data frame that containing the grouping info and multiple numeric variables.}

\item{comparisons}{a list that indicated which group to be compared.}

\item{test}{he method that used for significant test, one of "t.test" and "wilcox.test".
The default method is "wilcox.test".}

\item{x.text.angle}{A numeric number indicated the angle of axix.test.x.}

\item{map_signif_level}{A logic value}

\item{tip_length}{the length of tip, default is 0}

\item{textsize_sifnif}{the text size of signifcant}
}
\value{
a ggplot2 object
}
\description{
Draw box plot based on ggplot2, using data containing grouping info.
Can draw multiple variables
}
\examples{
data <- iris
colnames(data)[5] <- "group"
ggboxplot_facet(data, map_signif_level = T, comparisons = list(c("setosa", "versicolor"),
c("versicolor", "virginica"),
c("setosa", "virginica"))) + ggsci::scale_fill_aaas()
}
\author{
Zhonghui Gai
}
