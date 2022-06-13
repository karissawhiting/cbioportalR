# cbioportalR 0.2.1


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
