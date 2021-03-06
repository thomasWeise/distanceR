% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranker.R
\name{rank.dist}
\alias{rank.dist}
\title{Rank a Vector of Distance Values}
\usage{
rank.dist(X, na.value = 2)
}
\arguments{
\item{X}{the array of distance values}

\item{na.value}{the value to be used for NA distances. since all "normal"
distances will be normalized into \code{[0, 1]}, a value bigger than that
would make sense, say \code{2} or \code{+Inf}.}
}
\value{
an array with normalized distance ranks
}
\description{
This function computes the ranks of a vector of distance values.
  The resulting array will contain normalized ranks in \code{[0, 1]}.
  \code{NA} distances are allowed and will receive ranks \code{> 1}. If the
  smallest non-\code{NA} distance in \code{X} is \code{0}, all such distances
  will receive rank \code{0}. The largest non-\code{NA} distance in \code{X}
  will receive rank \code{1}. If the smallest non-\code{NA} distance in
  \code{X} is larger than zero, it will be assigned a value in \code{(0,1)},
  namely its raw rank divided by the maximum rank.
}
