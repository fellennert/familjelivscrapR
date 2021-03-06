% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_main.R
\name{get_main_sections}
\alias{get_main_sections}
\title{Get main sections}
\usage{
get_main_sections(mainpage = "http://gamla.familjeliv.se/Forum-0-0/")
}
\arguments{
\item{mainpage}{The gamla.familjeliv.se forum page; default value suffices.}
}
\value{
A tibble with two columns: 'name' and 'suffix'. The former indicates
the section's name, the latter its link's suffix.
}
\description{
Returns the main sections' names and suffixes
}
\examples{
get_main_sections()

}
