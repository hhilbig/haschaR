% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_feols_single.R
\name{tidy_feols_single}
\alias{tidy_feols_single}
\title{feols output summarize}
\usage{
tidy_feols_single(
  model,
  add_glance = T,
  add_dv_stats = T,
  add_conf_90 = T,
  add_first_rhs_stats = T
)
}
\arguments{
\item{model}{feols model}

\item{add_glance}{if T, glance output will be added to each line (note that not all default glance output will be returned)}

\item{add_dv_stats}{if T, DV mean, sd, min, max will be added to each line}

\item{add_conf_90}{if T, 90\% CIs will be added (note that I use normal and not t distribution)}

\item{add_first_rhs_stats}{if T, firsth RHS variable mean, sd, min, max will be added to each line. if the first RHS variable is always one, stats for the second RHS variable will be added instead.}
}
\description{
feols output summarize
}
