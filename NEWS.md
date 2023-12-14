# cbioportalR 1.1.0

* Added `available_sample_lists()` function which returns all available sample list IDs for a given study ID
* Added `sample_list_id` argument to `available_samples()` which returns all samples IDs in specific sample list within a study (#53).
* Fixed {cli} errors to make package compatible with {cli} ≥ v3.4.1.
* Added CNA segmentation retrieval endpoint accessible via `get_segments_by_sample()` and `get_segments_by_study()`). You can use `get_genomics_by_*(return_segments = TRUE)` as well to access this data.

# cbioportalR 1.0.1

This version makes the package compatible with [cBioPortal v5.0](https://github.com/cBioPortal/cbioportal/releases/tag/v5.0.0). Main updates include:

* Fusion API endpoint URLs updated to use `structural-variant` instead of `fusion` in specification.
* `get_structural_variants_by_sample()` and `get_structural_variants_by_study()` added as aliases for `get_fusions_by_sample()` and `get_fusions_by_study()`. These functions return the same results as their fusion counterparts and both names will be supported.
* When functions include the `data_type` argument, `structural_variant` is now available as option. This will return the same results as `fusion`.
* Output of `get_genetics_by_sample()` and `get_genetics_by_study()` functions are now named lists with names: `mutation`, `cna`, and `structural_variant` (changed from `fusion`) to be consistent with cBioPortal v5.0 naming conventions. 
* Tests and examples are no longer run on CRAN to protect from future changes to internal database. They are still run in Github Actions and examples are run on website.
* Some examples adjusted to use a new cBioPortal profile name, as needed (e.g. `prad_msk_2019_structural_variants` ->  `prad_msk_2019_structural_variants`)
* bug fix in `add_hugo` argument of `get_*_by_sample()` functions.

# cbioportalR 1.0.0

This is the first release submitted to CRAN, and includes all updates (listed below under versions 0.2.0 and 0.2.1) made since last Github release (0.1.0). Package has been overhauled since first release (0.1.0), and code will not be backwards compatible with that version.

### Summary of Updates (since 0.1.0)

* Authentication schema has been updated. Package-level environmental variable can now be set that specifies login credentials for that session
* Functionality to pull fusion data has been added
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
* Authentication schema was updated. Package level environment variable now set instead of global variable for base URL specification
* Updates to all documentation
* Added extensive error messaging using {cli}
* Added unit tests
* Fixed fusion functionality. User can now pull fusion data by study ID or sample ID

# cbioportalR 0.1.0

* First release - basic API wrapping functionality only
