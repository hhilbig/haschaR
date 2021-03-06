% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzy_string_match.R
\name{fuzzy_string_match}
\alias{fuzzy_string_match}
\title{Fuzzy string merging function}
\usage{
fuzzy_string_match(string_vec1, string_vec2, method = "jw")
}
\arguments{
\item{string_vec1}{vector of strings}

\item{method}{distance calculation method, based on methods in stringdist}
}
\value{
DF with original string, match and distance to matchs
}
\description{
Fuzzy string merging function
}
