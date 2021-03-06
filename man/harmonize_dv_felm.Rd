% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonize_dv_felm.R
\name{harmonize_dv_felm}
\alias{harmonize_dv_felm}
\title{Harmonized variable names for stargazer output for felm models}
\usage{
harmonize_dv_felm(model_list, term_replace = "", term_keep = "")
}
\arguments{
\item{model_list}{list of felm models to be passed to stargazer}

\item{term_replace}{name of explanatory variable to be replaced}

\item{term_keep}{replacement name for the variable to be replaced}
}
\value{
This returns the list of models, with harmonized variable names
}
\description{
Harmonized variable names for stargazer output for felm models
}
