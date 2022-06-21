# cbioportalR 1.0.0

This is the first release submitted to CRAN, and includes all updates (listed below under versions 0.2.0 and 0.2.0) made since last release (0.1.0). 

Package has been overhauled since first release (0.1.0), and code will not be backwards compatible with that version.

**Overview of Main Updates:**

* Authentication schema has been updated. Package-level environmental variable can now be set that specifies login credentials for that session
* Functionality to pull fusions has been added
* Users can pull data by study ID, sample ID, and patient ID
* Update `get_genomics_by_sample()` and related functions to pull all gene data available for select samples instead of pulling data for IMPACT genes only by default 
* Allow users to pass Hugo Symbols or Panel IDs to all `get_genomics_by_sample()` and related functions. 

# cbioportalR 0.2.1

* Update `get_genomics_by_sample()` and related functions to pull all gene data available for selected sample by default instead of
pulling data for IMPACT genes only by default. This will change default results when user does not specify genes, and could return more results than previously as they will include results for non IMPACT gene mutations, CNA or fusions, if available.
* Allow users to pass Hugo Symbols to all `get_genomics_by_sample()` and related functions (previously had to specify by Entrez Gene ID or `NULL`) (#33)
* Allow users to pass gene panels to all `get_genomics_by_sample()` and related functions with new `panel` argument. Previously could only specify `genes` by specified sets of gene IDs. (#15)
* Update functions that take sample ID-study ID or patient ID-study ID pairs to be less stringent in what data frame columns names they accept. Previously these functions only accepted snake case (e.g. `sample_id`, `study_id`), but now accept any capitalization/delimiters (e.g. `sampleID`, `sample id`) (#16)


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
