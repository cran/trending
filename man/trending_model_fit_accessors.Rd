% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trending_model_fit_accessors.R
\name{trending_model_fit_accessors}
\alias{trending_model_fit_accessors}
\alias{get_model}
\alias{get_model.trending_model_fit}
\alias{get_data}
\alias{get_data.trending_model_fit}
\title{Accessors for trending_model_fit objects}
\usage{
get_model(x, ...)

\method{get_model}{trending_model_fit}(x, ...)

get_data(x, ...)

\method{get_data}{trending_model_fit}(x, ...)
}
\arguments{
\item{x}{Object of class \code{\link{trending_model}} (i.e. the output of functions
\code{\link[=lm_model]{lm_model()}}, \code{\link[=glm_model]{glm_model()}}, \code{\link[=glm_nb_model]{glm_nb_model()}}, or \code{\link[=brms_model]{brms_model()}}).}

\item{...}{Not currently used}
}
\description{
These functions can be used to access information stored in
\code{trending_model_fit} objects. See details.
}
\details{
The following accessors are available:
\itemize{
\item \code{get_model()}: get the fitted model stored in the object
}
}
