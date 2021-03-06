% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbp_api.R
\name{cbp_api}
\alias{cbp_api}
\title{Core function to retrieve data from cBioPortal API.}
\usage{
cbp_api(
  url_path,
  method = c("get", "post"),
  token = get_cbioportal_token(),
  body = NULL,
  extra_box = FALSE
)
}
\arguments{
\item{url_path}{The url path for API call}

\item{method}{Which API method to use. Default is GET.}

\item{token}{Authentication token, if needed. Default is \code{get_cbioportal_token()}}

\item{body}{Arguments passed to API call (e.g. sample ID or gene IDs)}

\item{extra_box}{Some functions require an additional list() wrapping around body idk why}
}
\value{
A parsed API response
}
\description{
Core function to retrieve data from cBioPortal API.
}
\examples{
get_cbioportal_db("public")
cbp_api(url_path = "genes/TP53")

}
