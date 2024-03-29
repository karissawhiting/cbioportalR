% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by_studies.R
\name{available_samples}
\alias{available_samples}
\title{Get All Sample IDs in a Study}
\usage{
available_samples(study_id = NULL, sample_list_id = NULL, base_url = NULL)
}
\arguments{
\item{study_id}{A character string indicating which study ID should be searched.
Only 1 study ID allowed.}

\item{sample_list_id}{A character string indicating which sample list ID should be searched.
Only 1 sample list ID allowed.}

\item{base_url}{The database URL to query
If \code{NULL} will default to URL set with \verb{set_cbioportal_db(<your_db>)}}
}
\value{
A dataframe of sample_ids in a given study
}
\description{
Pulls all available sample IDs for a given study ID or sample list ID.
Either a study ID or sample list ID must be passed. If both \code{sample_list} and \code{study_id} are not \code{NULL},
\code{sample_list} ID will be searched and \code{study_id} will be ignored.
}
\examples{
\dontrun{
set_cbioportal_db("public")
available_samples(study_id = "acc_tcga")
available_samples(sample_list_id = "acc_tcga_cna")
}

}
