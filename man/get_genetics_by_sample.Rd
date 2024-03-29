% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genomics_by_sample.R
\name{get_genetics_by_sample}
\alias{get_genetics_by_sample}
\title{Get All Genomic Information By Sample IDs}
\usage{
get_genetics_by_sample(
  sample_id = NULL,
  study_id = NULL,
  sample_study_pairs = NULL,
  genes = NULL,
  panel = NULL,
  add_hugo = TRUE,
  base_url = NULL,
  return_segments = FALSE
)
}
\arguments{
\item{sample_id}{a vector of sample IDs (character)}

\item{study_id}{A string indicating the study ID from which to pull data. If no study ID, will
guess the study ID based on your URL and inform. Only 1 study ID can be passed. If mutations/cna from
more than 1 study needed, see \code{sample_study_pairs}}

\item{sample_study_pairs}{A dataframe with columns: \code{sample_id}, \code{study_id} and \code{molecular_profile_id} (optional). Variations in capitalization of column names are accepted.
This can be used in place of \code{sample_id}, \code{study_id}, \code{molecular_profile_id} arguments above if you
need to pull samples from several different studies at once. If passed this will take overwrite \code{sample_id}, \code{study_id}, \code{molecular_profile_id} if also passed.}

\item{genes}{A vector of Entrez ids or Hugo symbols. If Hugo symbols are supplied, they will be converted to entrez ids using the \code{get_entrez_id()} function.
If \code{panel} and \code{genes} are both supplied, genes from both arguments will be returned. If both are NULL (default), it will return gene results for all available genomic data for that sample.}

\item{panel}{One or more panel IDs to query (e.g. 'IMPACT468').
If \code{panel} and \code{genes} are both supplied, genes from both arguments will be returned. If both are NULL (default), it will return gene results for all available genomic data for that sample.}

\item{add_hugo}{Logical indicating whether \code{HugoGeneSymbol} should be added to your resulting data frame, if not already present in raw API results.
Argument is \code{TRUE} by default. If \code{FALSE}, results will be returned as is (i.e. any existing Hugo Symbol columns in raw results will not be removed).}

\item{base_url}{The database URL to query
If \code{NULL} will default to URL set with \verb{set_cbioportal_db(<your_db>)}}

\item{return_segments}{Default is \code{FALSE} where copy number segmentation data won't be returned in addition to the mutation, cna and structural variant data.
\code{TRUE} will return any available segmentation data with results.}
}
\value{
A list of mutations, cna and structural variants (including fusions), if available. Will also return copy number segmentation data if \code{return_segments = TRUE}.
}
\description{
Get All Genomic Information By Sample IDs
}
\examples{
\dontrun{
get_genetics_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
 study_id = "acc_tcga",
 return_segments = TRUE)
}
}
