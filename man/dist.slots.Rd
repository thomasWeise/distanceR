% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indexing.R
\name{dist.slots}
\alias{dist.slots}
\title{Get the Number of Slots in a Distance Matrix}
\usage{
dist.slots(n)
}
\arguments{
\item{n}{the number of objects to compare}
}
\value{
the required number of slots in a distance matrix
}
\description{
This function receives the number \code{n} of objects to compare
  and computes how many different distances will be stored in a distance
  matrix, i.e., what the number of slots of a \code{\link[stats]{dist}}
  object will be.
}