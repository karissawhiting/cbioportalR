# cbioportalR 0.2.1

* Update `get_genomics_by_sample()` and related functions to pull all gene data available for selected sample by default instead of 
pulling data for IMPACT genes only by default. This will change default results when user does not specify genes, and could return more results than previously as they will include results for non IMPACT gene mutations, CNA or fusions, if available.
* Allow users to pass Hugo Symbols to all `get_genomics_by_sample()` and related functions (previously had to specify by Entrez Gene ID or `NULL`) (#33)


# cbioportalR 0.2.0

* Update internals of most functions with the exception of `cbp_api()`
* Genomic data is all pulled by the common internal functions `.get_data_by_study()` and `.get_data_by_sample()` with wrappers for each data type
* Updated function names to specify if user is pulling by study or sample ID
* Authentication schema is now different. Package level environment variable now set instead of global for base URL specification
* Updates to all documentation
* Added extensive error messaging using {cli}
* Added unit tests
* Fixed fusion functionality. User can now pull fusion data by study ID or sample ID

# cbioportalR 0.1.0

* First release - basic API wrapping functionality only
