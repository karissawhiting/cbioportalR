
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/karissawhiting/cbioportalR/workflows/R-CMD-check/badge.svg)](https://github.com/karissawhiting/cbioportalR/actions)
[![Codecov test
coverage](https://codecov.io/gh/karissawhiting/cbioportalR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/karissawhiting/cbioportalR?branch=master)

<!-- badges: end -->

# cbioportalR <a href='https://karissawhiting.github.io/cbioportalR/'><img src='man/figures/logo.png' align="right" height="120" /></a>

{cbioportalR} allows you to access
[cBioPortal’s](https://www.cbioportal.org/) genomic and clinical data
sets directly through R. The package wraps cBioPortal’s API endpoints so
R users can easily leverage the [existing
API](https://www.cbioportal.org/api/swagger-ui.html) to access genomic
data on mutations, copy number alterations and fusions as well as data
on tumor mutational burden (TMB), microsatellite instability status
(MSI) and select clinical data points (depending on the study).

This package was created to work with both the public [cBioPortal
website](https://www.cbioportal.org/), as well as private institutional
cBioPortal instances (e.g. MSKCC, GENIE) with appropriate credentials
and [authentication](#authentication).

For more information on cBioPortal, see the following publications:

-   [Gao et al. Sci. Signal.
    2013](https://pubmed.ncbi.nlm.nih.gov/23550210/)
-   [Cerami et al. Cancer Discov.
    2012](https://cancerdiscovery.aacrjournals.org/content/2/5/401.abstract)

For full documentation on the cBioPortal API, please see the following
links:

-   [cBioPortal API and API Clients
    documentation](https://docs.cbioportal.org/6.-web-api-and-clients/api-and-api-clients)
-   [Full reference documentation for
    API](https://www.cbioportal.org/api/swagger-ui.html#/)

*Note: If you are a MSK researcher working on IMPACT data, you should
connect to MSK’s cBioPortal instance to get the most up to date IMPACT
data, and you must follow [MSK-IMPACT publication
guidelines](https://cmo.mskcc.org/cmo/initiatives/msk-impact/) when
using this data*

## Installation

You can install {cbioportalR} with:

``` r
remotes::install_github("karissawhiting/cbioportalR")
```

``` r
library(cbioportalR)
```

## Authentication

If you are using the public domain <https://www.cbioportal.org/>, you
don’t need a token to start pulling data. If you are using a private
instance of cBioPortal (like MSKCC’s institutional database), you will
need to acquire a token and save it to your `.Renviron` file (or
wherever you store credentials). Simply log in to your institution’s
cBioPortal site, acquire a token (Usually through the ‘Web API’ tab) and
save it in your `.Renviron` file. This will save the token as an
environmental variable so you don’t have to hard code the secret key in
your scripts.

*Tip: The following {usethis} function can easily find and open the
`.Renviron` for you:*

``` r
usethis::edit_r_environ()
```

Paste the token you were given (using the format below) in the .Renviron
file and save the file changes. *After saving you should restart your R
session to ensure the token is saved and recognized.*

``` r
CBIOPORTAL_TOKEN = 'YOUR_TOKEN'
```

You can test that your token was saved using:

``` r
get_cbioportal_token()
```

For every new R session, you need to set your database URL. The
`set_cbioportal_db()` function will set an environmental variable for
your session that tells the package which database to point to for all
API calls. You can set it to point to the public database with
`db = 'www.cbioportal.org'` or `db = 'public'`. If using a private
database you will pass your institutions cBioPortal URL as `db`. This
function will both set your URL and check the connection.

``` r
set_cbioportal_db(db = "public")
#> ✓ You are successfully connected!
#> ✓ base_url for this R session is now set to "www.cbioportal.org/api"
```

You are now set up for the remainder of your session! API calls depend
on your internet connection and possibly a VPN connection so you can use
the following to check your connection at any time throughout your
session:

``` r
test_cbioportal_db()
#> ✓ You are successfully connected!
```

## cBioPortal Data Model

There are many ways to identify and pull data (e.g. by study ID, by
sample ID, by molecular profile ID). Having an understanding of how data
is organized in cBioPortal will help you determine which functions you
need. The figure below outlines the general data schema for cBioPortal
and which functions access which levels of the schema:

<table>
<tbody>
<tr>
<td>
<img src="man/figures/svg-cbp-diagram.svg" width="90%">
</td>
<td>
<table style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color: #333333; font-size: small; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: 500px; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;">
  
  <thead style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
    <tr>
      <th style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: center;" rowspan="1" colspan="1"><strong>Level</strong></th>
      <th style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" rowspan="1" colspan="1"><strong>Functions</strong></th>
    </tr>
  </thead>
  <tbody style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr><td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; background-color: rgba(138,43,226,0.6); color: #FFFFFF; font-weight: bold;">Database</td>
<td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;">`available_studies()`, `available_profiles()`, `available_gene_panels()`, `get_genes()`</td></tr>
    <tr><td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; background-color: rgba(93,109,221,0.6); color: #FFFFFF; font-weight: bold;">Studies</td>
<td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;">`available_profiles()`, `get_study_info()`, `available_clinical_attributes()`, `get_genetics_by_study()`, `get_mutations_by_study()`, `get_cna_by_study()`, `get_fusions_by_study()`, `get_clinical_by_study()`, `available_samples()`</td></tr>
    <tr><td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; background-color: rgba(48,167,198,0.6); color: #000000; font-weight: bold;">Molecular Profiles</td>
<td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;">`get_genetics_by_study()`, `get_mutations_by_study()`, `get_cna_by_study()`, `get_fusions_by_study()`</td></tr>
    <tr><td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: center; background-color: rgba(244,164,96,0.6); color: #000000; font-weight: bold;">Samples</td>
<td style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;">`get_genetics_by_sample()`, `get_mutations_by_study()`, `get_cna_by_sample()`, `get_fusions_by_sample()`, `get_clinical_by_sample()`, `get_panel_by_sample()`, `get_samples_by_patient()`</td></tr>
  </tbody>
  
  
</table>
</td>
</tr>
</tbody>
</table>

## Examples

To see available studies in your database you can use:

``` r
available_studies() %>% 
  head(n = 10)
#> # A tibble: 10 × 13
#>    studyId name  description publicStudy pmid  citation groups status importDate
#>    <chr>   <chr> <chr>       <lgl>       <chr> <chr>    <chr>   <int> <chr>     
#>  1 prad_m… Pros… "Genome-wi… TRUE        2502… Hierony… "PUBL…      0 2021-04-2…
#>  2 pan_or… Chin… "Landscape… TRUE        <NA>  <NA>     ""          0 2021-09-2…
#>  3 crc_ni… Disp… "Targeted … TRUE        <NA>  <NA>     ""          0 2021-10-2…
#>  4 acc_tc… Adre… "TCGA Adre… TRUE        <NA>  <NA>     "PUBL…      0 2021-10-2…
#>  5 acyc_s… Aden… "Whole exo… TRUE        2377… Stephen… "ACYC…      0 2021-10-2…
#>  6 all_st… Acut… "Whole-gen… TRUE        2777… Zhang e… "PUBL…      0 2021-10-2…
#>  7 all_ph… Pedi… "Whole gen… TRUE        <NA>  <NA>     "NCI-…      0 2021-10-2…
#>  8 all_st… Acut… "Comprehen… TRUE        2573… Anderss… "PUBL…      0 2021-11-0…
#>  9 acbc_m… Aden… "Whole exo… TRUE        2609… Martelo… "ACYC…      0 2021-11-0…
#> 10 acyc_j… Aden… "Whole-gen… TRUE        2686… Rettig … "ACYC…      0 2021-11-0…
#> # … with 4 more variables: allSampleCount <int>, readPermission <lgl>,
#> #   cancerTypeId <chr>, referenceGenome <chr>
```

To view study metadata on a particular study you can use:

``` r
get_study_info("acc_tcga") %>% 
  t()
#>                             [,1]                                                                                                                                                                                             
#> name                        "Adrenocortical Carcinoma (TCGA, Firehose Legacy)"                                                                                                                                               
#> description                 "TCGA Adrenocortical Carcinoma. Source data from <A HREF=\"http://gdac.broadinstitute.org/runs/stddata__2016_01_28/data/ACC/20160128/\">GDAC Firehose</A>. Previously known as TCGA Provisional."
#> publicStudy                 "TRUE"                                                                                                                                                                                           
#> groups                      "PUBLIC"                                                                                                                                                                                         
#> status                      "0"                                                                                                                                                                                              
#> importDate                  "2021-10-29 00:00:00"                                                                                                                                                                            
#> allSampleCount              "92"                                                                                                                                                                                             
#> sequencedSampleCount        "90"                                                                                                                                                                                             
#> cnaSampleCount              "90"                                                                                                                                                                                             
#> mrnaRnaSeqSampleCount       "0"                                                                                                                                                                                              
#> mrnaRnaSeqV2SampleCount     "79"                                                                                                                                                                                             
#> mrnaMicroarraySampleCount   "0"                                                                                                                                                                                              
#> miRnaSampleCount            "0"                                                                                                                                                                                              
#> methylationHm27SampleCount  "0"                                                                                                                                                                                              
#> rppaSampleCount             "46"                                                                                                                                                                                             
#> massSpectrometrySampleCount "0"                                                                                                                                                                                              
#> completeSampleCount         "75"                                                                                                                                                                                             
#> readPermission              "TRUE"                                                                                                                                                                                           
#> studyId                     "acc_tcga"                                                                                                                                                                                       
#> cancerTypeId                "acc"                                                                                                                                                                                            
#> cancerType.name             "Adrenocortical Carcinoma"                                                                                                                                                                       
#> cancerType.dedicatedColor   "Purple"                                                                                                                                                                                         
#> cancerType.shortName        "ACC"                                                                                                                                                                                            
#> cancerType.parent           "adrenal_gland"                                                                                                                                                                                  
#> cancerType.cancerTypeId     "acc"                                                                                                                                                                                            
#> referenceGenome             "hg19"
```

To pull all genomic data for a particular study you can use:

``` r
# As a result you will get a list of dataframes of 1) mutation + fusion and 2) cna.
df <- get_genetics_by_study(study_id = "acc_tcga")
#> ℹ Returning all data for the "acc_tcga_mutations" molecular profile in the "acc_tcga" study
#> ℹ Returning all data for the "acc_tcga_gistic" molecular profile in the "acc_tcga" study
#> ! No "fusion" data returned. Error:  No molecular profile for `data_type = fusion` found in "acc_tcga".  See
#> `available_profiles('acc_tcga')`
```

As you can see, there is no fusion data available for this study, only
mutations and cna. Alternatively, you can use
`get_mutations_by_study(study_id = "acc_tcga")` and
`get_cna_by_study(study_id = "acc_tcga")`

``` r
df$mut %>% 
  head()
#> # A tibble: 6 × 32
#>   uniqueSampleKey           uniquePatientKey molecularProfil… sampleId patientId
#>   <chr>                     <chr>            <chr>            <chr>    <chr>    
#> 1 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 2 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 3 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 4 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 5 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 6 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> # … with 27 more variables: entrezGeneId <int>, studyId <chr>, center <chr>,
#> #   mutationStatus <chr>, validationStatus <chr>, tumorAltCount <int>,
#> #   tumorRefCount <int>, normalAltCount <int>, normalRefCount <int>,
#> #   startPosition <int>, endPosition <int>, referenceAllele <chr>,
#> #   proteinChange <chr>, mutationType <chr>, functionalImpactScore <chr>,
#> #   fisValue <dbl>, linkXvar <chr>, linkPdb <chr>, linkMsa <chr>,
#> #   ncbiBuild <chr>, variantType <chr>, keyword <chr>, chr <chr>, …
```

You can also pull data by specific sample IDs but the API requires a bit
more information from you (unlike pulling by study ID which returns
everything available for that study). This can be useful when working
within a very large database or working across samples housed in
multiple different studies. When querying by `sample_id` you must also
specify the corresponding `study_id` in which the samples are housed and
the specific genes for which you wish to return results. When these
pieces of information are not provided, {cbioportalR} makes an informed
guess based on your connection and will throw an informative message to
clarify exactly what is being queried. In the example below, the
function defaults to the public version of the IMPACT database
(`study_id = "msk_impact_2017"`).

``` r
samples <- available_samples(study_id = "acc_tcga") %>%
  pull(sampleId) %>%
  head(n = 10)

mutations <- get_mutations_by_sample(sample_id =  samples)
#> The following parameters were used in query:
#> Study ID: "msk_impact_2017"
#> Molecular Profile ID: "msk_impact_2017_mutations"
#> Genes: "all IMPACT genes (see `gnomeR::impact_gene_info`)"

# no results returned because these samples are not in this study
length(mutations) == 0
#> [1] TRUE
```

No results were returned because the samples are not stored in this
study. When we specify the correct study (`study_id = "acc_tcga"`), we
get accurate results. You can check which samples are stored in a study
using `available_samples(study_id = "acc_tcga")`.

``` r
mutations <- get_mutations_by_sample(sample_id =  samples,
                                    study_id = "acc_tcga")
#> The following parameters were used in query:
#> Study ID: "acc_tcga"
#> Molecular Profile ID: "acc_tcga_mutations"
#> Genes: "all IMPACT genes (see `gnomeR::impact_gene_info`)"

mutations %>%
  head()
#> # A tibble: 6 × 32
#>   uniqueSampleKey           uniquePatientKey molecularProfil… sampleId patientId
#>   <chr>                     <chr>            <chr>            <chr>    <chr>    
#> 1 VENHQS1PUi1BNUoxLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 2 VENHQS1PUi1BNUoyLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 3 VENHQS1PUi1BNUoyLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 4 VENHQS1PUi1BNUoyLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 5 VENHQS1PUi1BNUoyLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> 6 VENHQS1PUi1BNUoyLTAxOmFj… VENHQS1PUi1BNUo… acc_tcga_mutati… TCGA-OR… TCGA-OR-…
#> # … with 27 more variables: entrezGeneId <int>, studyId <chr>, center <chr>,
#> #   mutationStatus <chr>, validationStatus <chr>, tumorAltCount <int>,
#> #   tumorRefCount <int>, normalAltCount <int>, normalRefCount <int>,
#> #   startPosition <int>, endPosition <int>, referenceAllele <chr>,
#> #   proteinChange <chr>, mutationType <chr>, functionalImpactScore <chr>,
#> #   fisValue <dbl>, linkXvar <chr>, linkPdb <chr>, linkMsa <chr>,
#> #   ncbiBuild <chr>, variantType <chr>, keyword <chr>, chr <chr>, …
```

Lastly, you can also pull clinical data by study_id or sample ID. To see
what data is available, you can use:

``` r
available_clinical_attributes(study_id = "acc_tcga") %>%
  head()
#> # A tibble: 6 × 7
#>   displayName    description datatype patientAttribute priority clinicalAttribu…
#>   <chr>          <chr>       <chr>    <lgl>            <chr>    <chr>           
#> 1 Diagnosis Age  Age at whi… NUMBER   TRUE             1        AGE             
#> 2 Neoplasm Dise… The extent… STRING   TRUE             1        AJCC_PATHOLOGIC…
#> 3 American Join… The versio… STRING   TRUE             1        AJCC_STAGING_ED…
#> 4 Atypical Mito… Atypical M… STRING   TRUE             1        ATYPICAL_MITOTI…
#> 5 Cancer Type    Cancer type STRING   FALSE            1        CANCER_TYPE     
#> 6 Cancer Type D… Cancer typ… STRING   FALSE            1        CANCER_TYPE_DET…
#> # … with 1 more variable: studyId <chr>
```

``` r
get_clinical_by_study("acc_tcga")
#> ! No `clinical_attribute` passed. Defaulting to returning all clinical attributes in "acc_tcga" study
#> # A tibble: 1,558 × 7
#>    uniqueSampleKey  uniquePatientKey sampleId patientId studyId clinicalAttribu…
#>    <chr>            <chr>            <chr>    <chr>     <chr>   <chr>           
#>  1 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… CANCER_TYPE     
#>  2 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… CANCER_TYPE_DET…
#>  3 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… DAYS_TO_COLLECT…
#>  4 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… FRACTION_GENOME…
#>  5 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… IS_FFPE         
#>  6 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… MUTATION_COUNT  
#>  7 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… OCT_EMBEDDED    
#>  8 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… ONCOTREE_CODE   
#>  9 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… OTHER_SAMPLE_ID 
#> 10 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… PATHOLOGY_REPOR…
#> # … with 1,548 more rows, and 1 more variable: value <chr>
```

``` r
get_clinical_by_sample(sample_id = samples, study_id = "acc_tcga") %>%
  head(10)
#> ! No `clinical_attribute` passed. Defaulting to returning
#> all clinical attributes in "acc_tcga" study
#> # A tibble: 10 × 7
#>    uniqueSampleKey  uniquePatientKey sampleId patientId studyId clinicalAttribu…
#>    <chr>            <chr>            <chr>    <chr>     <chr>   <chr>           
#>  1 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… CANCER_TYPE     
#>  2 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… CANCER_TYPE_DET…
#>  3 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… DAYS_TO_COLLECT…
#>  4 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… FRACTION_GENOME…
#>  5 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… IS_FFPE         
#>  6 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… MUTATION_COUNT  
#>  7 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… OCT_EMBEDDED    
#>  8 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… ONCOTREE_CODE   
#>  9 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… OTHER_SAMPLE_ID 
#> 10 VENHQS1PUi1BNUo… VENHQS1PUi1BNUo… TCGA-OR… TCGA-OR-… acc_tc… PATHOLOGY_REPOR…
#> # … with 1 more variable: value <chr>
```

All functions that pull by study IDs are limited to pulling data from
one study at a time. If you need to pull specific samples from multiple
studies, you likely want to pull by sample ID (instead of study ID) and
supply the function with a dataframe of `sample_study_pairs` that
specify where the function should look for each study. For more
information see the [Overview of Workflow
Vignette](https://karissawhiting.github.io/cbioportalR/articles/overview-of-workflow.html).
