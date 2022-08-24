
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/karissawhiting/cbioportalR/workflows/R-CMD-check/badge.svg)](https://github.com/karissawhiting/cbioportalR/actions)
[![Codecov test
coverage](https://codecov.io/gh/karissawhiting/cbioportalR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/karissawhiting/cbioportalR?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/cbioportalR)](https://CRAN.R-project.org/package=cbioportalR)
<!-- badges: end -->

<br>

## cbioportalR <a href='https://www.karissawhiting.com/cbioportalR/'><img src='man/figures/logo.png' align="right" height="180" style="align:right; height:180px;"></a>

{cbioportalR} allows you to access
[cBioPortal’s](https://www.cbioportal.org/) genomic and clinical data
sets directly through R. The package wraps cBioPortal’s API endpoints so
R users can easily leverage the [existing
API](https://www.cbioportal.org/api/swagger-ui/index.html) to access
genomic data on mutations, copy number alterations and fusions as well
as data on tumor mutational burden (TMB), microsatellite instability
status (MSI) and select clinical data points (depending on the study).

This package was created to work with both the public [cBioPortal
website](https://www.cbioportal.org/), as well as private institutional
cBioPortal instances (e.g. MSKCC, GENIE) with appropriate credentials
and [authentication](#authentication).

This package is compatible with cBioPortal v5.0, but is subject to
change as [cBioPortal updates are
released](https://github.com/cBioPortal/cbioportal/releases). For more
information on cBioPortal, see the following publications:

- [Gao et al. Sci. Signal.
  2013](https://pubmed.ncbi.nlm.nih.gov/23550210/)
- [Cerami et al. Cancer Discov.
  2012](https://aacrjournals.org/cancerdiscovery/article/2/5/401/3246/The-cBio-Cancer-Genomics-Portal-An-Open-Platform)

For full documentation on the cBioPortal API, please see the following
links:

- [cBioPortal API and API Clients
  documentation](https://docs.cbioportal.org/web-api-and-clients/)
- [Full reference documentation for
  API](https://www.cbioportal.org/api/swagger-ui/index.html)

*Note: If you are a MSK researcher working on IMPACT data, you should
connect to MSK’s cBioPortal instance to get the most up to date IMPACT
data, and you must follow the MSK-IMPACT publication guidelines when
using this data*

## Installation

You can install {cbioportalR} with the following code:

``` r
install.packages("cbioportalR")
```

Install the development version of {cbioportalR} with:

``` r
remotes::install_github("karissawhiting/cbioportalR")
```

Load the package:

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
#> ✔ You are successfully connected!
#> ✔ base_url for this R session is now set to "www.cbioportal.org/api"
```

You are now set up for the remainder of your session! API calls depend
on your internet connection and possibly a VPN connection so you can use
the following to check your connection at any time throughout your
session:

``` r
test_cbioportal_db()
#> ✔ You are successfully connected!
```

## cBioPortal Data Model

There are many ways to identify and pull data (e.g. by study ID, by
sample ID, by molecular profile ID). Having an understanding of how data
is organized in cBioPortal will help you determine which functions you
need. The figure below outlines the general data schema for cBioPortal
and which functions access which levels of the schema:

<img src="man/figures/readme-cbp-diagram.png" width="100%" />

## Examples

To see available studies in your database you can use:

``` r
available_studies() %>% 
  head(n = 10)
#> # A tibble: 10 × 13
#>    studyId   name  descr…¹ publi…² groups status impor…³ allSa…⁴ readP…⁵ cance…⁶
#>    <chr>     <chr> <chr>   <lgl>   <chr>   <int> <chr>     <int> <lgl>   <chr>  
#>  1 acc_tcga  Adre… "TCGA … TRUE    "PUBL…      0 2022-0…      92 TRUE    acc    
#>  2 bcc_unig… Basa… "Whole… TRUE    "PUBL…      0 2022-0…     293 TRUE    bcc    
#>  3 ampca_bc… Ampu… "Exome… TRUE    "PUBL…      0 2022-0…     160 TRUE    ampca  
#>  4 blca_dfa… Blad… "Whole… TRUE    "PUBL…      0 2022-0…      50 TRUE    blca   
#>  5 blca_msk… Blad… "Compr… TRUE    "PUBL…      0 2022-0…      97 TRUE    blca   
#>  6 blca_bgi  Blad… "Whole… TRUE    "PUBL…      0 2022-0…      99 TRUE    blca   
#>  7 blca_msk… Blad… "Genom… TRUE    "PUBL…      0 2022-0…     109 TRUE    blca   
#>  8 all_stju… Hypo… "Whole… TRUE    ""          0 2022-0…      44 TRUE    myeloid
#>  9 acyc_fmi… Aden… "Targe… TRUE    "ACYC…      0 2022-0…      28 TRUE    acyc   
#> 10 acyc_san… Aden… "Whole… TRUE    "ACYC…      0 2022-0…      24 TRUE    acyc   
#> # … with 3 more variables: referenceGenome <chr>, pmid <chr>, citation <chr>,
#> #   and abbreviated variable names ¹​description, ²​publicStudy, ³​importDate,
#> #   ⁴​allSampleCount, ⁵​readPermission, ⁶​cancerTypeId
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
#> importDate                  "2022-03-04 17:47:56"                                                                                                                                                                            
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
df <- get_genetics_by_study(study_id = "acc_tcga")
#> ℹ Returning all data for the "acc_tcga_mutations" molecular profile in the "acc_tcga" study
#> ℹ Returning all data for the "acc_tcga_gistic" molecular profile in the "acc_tcga" study
#> ! No "fusion" data returned. Error:  No molecular profile for `` data_type = `fusion` `` found in "acc_tcga".  See `` available_profiles('`acc_tcga`') ``
```

As a result, you will get a list of data frames with mutation and CNA
data respectively. The function will also try to pull fusion (structural
variant) data, but there is no fusion data available for this study, as
indicated by the function message.

``` r
df$mutation %>% 
  head()
#> # A tibble: 6 × 33
#>   hugoG…¹ entre…² uniqu…³ uniqu…⁴ molec…⁵ sampl…⁶ patie…⁷ studyId center mutat…⁸
#>   <chr>     <int> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  <chr>  
#> 1 KRT8       3856 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… broad… Somatic
#> 2 LCE1B    353132 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… hgsc.… Somatic
#> 3 SLC9C2   284525 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… broad… Somatic
#> 4 DNAH14   127602 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… broad… Somatic
#> 5 OPN4      94233 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… hgsc.… Somatic
#> 6 DNAJC4     3338 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… hgsc.… Somatic
#> # … with 23 more variables: validationStatus <chr>, tumorAltCount <int>,
#> #   tumorRefCount <int>, normalAltCount <int>, normalRefCount <int>,
#> #   startPosition <int>, endPosition <int>, referenceAllele <chr>,
#> #   proteinChange <chr>, mutationType <chr>, functionalImpactScore <chr>,
#> #   fisValue <dbl>, linkXvar <chr>, linkPdb <chr>, linkMsa <chr>,
#> #   ncbiBuild <chr>, variantType <chr>, keyword <chr>, chr <chr>,
#> #   variantAllele <chr>, refseqMrnaId <chr>, proteinPosStart <int>, …

df$cna %>% 
  head()
#> # A tibble: 6 × 9
#>   hugoGeneSymbol entre…¹ uniqu…² uniqu…³ molec…⁴ sampl…⁵ patie…⁶ studyId alter…⁷
#>   <chr>            <int> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>     <int>
#> 1 MEOX1             4222 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc…       2
#> 2 NUFIP2           57532 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc…       2
#> 3 OSBPL7          114881 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc…       2
#> 4 TP53I13          90313 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc…       2
#> 5 TAOK1            57551 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc…       2
#> 6 SPOP              8405 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc…       2
#> # … with abbreviated variable names ¹​entrezGeneId, ²​uniqueSampleKey,
#> #   ³​uniquePatientKey, ⁴​molecularProfileId, ⁵​sampleId, ⁶​patientId, ⁷​alteration
```

You can also pull data by specific sample IDs but the API requires a bit
more information from you (unlike pulling by study ID which returns
everything available for that study). This can be useful when working
within a very large database or working across samples housed in
multiple different studies. When querying by `sample_id` you must also
specify the corresponding `study_id` in which the samples are housed.
When these pieces of information are not provided, {cbioportalR} makes
an informed guess based on your connection and will throw an informative
message to clarify exactly what is being queried. In the example below,
the function defaults to the public version of the IMPACT database
(`study_id = "msk_impact_2017"`).

``` r
samples <- available_samples(study_id = "acc_tcga") %>%
  pull(sampleId) %>%
  head(n = 10)

mutations <- get_mutations_by_sample(sample_id =  samples)
#> The following parameters were used in query:
#> Study ID: "msk_impact_2017"
#> Molecular Profile ID: "msk_impact_2017_mutations"
#> Genes: "All available genes"

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
#> Genes: "All available genes"

mutations %>%
  head()
#> # A tibble: 6 × 33
#>   hugoG…¹ entre…² uniqu…³ uniqu…⁴ molec…⁵ sampl…⁶ patie…⁷ studyId center mutat…⁸
#>   <chr>     <int> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  <chr>  
#> 1 KRT8       3856 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… broad… Somatic
#> 2 LCE1B    353132 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… hgsc.… Somatic
#> 3 SLC9C2   284525 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… broad… Somatic
#> 4 DNAH14   127602 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… broad… Somatic
#> 5 OPN4      94233 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… hgsc.… Somatic
#> 6 DNAJC4     3338 VENHQS… VENHQS… acc_tc… TCGA-O… TCGA-O… acc_tc… hgsc.… Somatic
#> # … with 23 more variables: validationStatus <chr>, tumorAltCount <int>,
#> #   tumorRefCount <int>, normalAltCount <int>, normalRefCount <int>,
#> #   startPosition <int>, endPosition <int>, referenceAllele <chr>,
#> #   proteinChange <chr>, mutationType <chr>, functionalImpactScore <chr>,
#> #   fisValue <dbl>, linkXvar <chr>, linkPdb <chr>, linkMsa <chr>,
#> #   ncbiBuild <chr>, variantType <chr>, keyword <chr>, chr <chr>,
#> #   variantAllele <chr>, refseqMrnaId <chr>, proteinPosStart <int>, …
```

Lastly, you can also pull clinical data or sample metadata (e.g. tumor
sample site) by study ID, sample ID or patient ID. To see what data is
available, you can use:

``` r
available_clinical_attributes(study_id = "acc_tcga") %>%
  head()
#> # A tibble: 6 × 7
#>   displayName                    descr…¹ datat…² patie…³ prior…⁴ clini…⁵ studyId
#>   <chr>                          <chr>   <chr>   <lgl>   <chr>   <chr>   <chr>  
#> 1 Diagnosis Age                  Age at… NUMBER  TRUE    1       AGE     acc_tc…
#> 2 Neoplasm Disease Stage Americ… The ex… STRING  TRUE    1       AJCC_P… acc_tc…
#> 3 American Joint Committee on C… The ve… STRING  TRUE    1       AJCC_S… acc_tc…
#> 4 Atypical Mitotic Figures       Atypic… STRING  TRUE    1       ATYPIC… acc_tc…
#> 5 Cancer Type                    Cancer… STRING  FALSE   1       CANCER… acc_tc…
#> 6 Cancer Type Detailed           Cancer… STRING  FALSE   1       CANCER… acc_tc…
#> # … with abbreviated variable names ¹​description, ²​datatype, ³​patientAttribute,
#> #   ⁴​priority, ⁵​clinicalAttributeId
```

``` r
get_clinical_by_study("acc_tcga")
#> ! Sample Level Clinical Data: No `clinical_attribute` passed. Defaulting to returning all clinical attributes in "acc_tcga" study
#> ! Patient Level Clinical Data: No `clinical_attribute` passed. Defaulting to returning all clinical attributes in "acc_tcga" study
#> # A tibble: 6,292 × 6
#>    uniquePatientKey             patientId    studyId  clinicalAt…¹ value dataL…²
#>    <chr>                        <chr>        <chr>    <chr>        <chr> <chr>  
#>  1 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga AGE          58    PATIENT
#>  2 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga AJCC_PATHOL… Stag… PATIENT
#>  3 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga ATYPICAL_MI… Atyp… PATIENT
#>  4 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga CAPSULAR_IN… Inva… PATIENT
#>  5 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga CLIN_M_STAGE M0    PATIENT
#>  6 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga CT_SCAN_PRE… [Unk… PATIENT
#>  7 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga CYTOPLASM_P… Cyto… PATIENT
#>  8 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga DAYS_TO_INI… 0     PATIENT
#>  9 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga DFS_MONTHS   24.77 PATIENT
#> 10 VENHQS1PUi1BNUoxOmFjY190Y2dh TCGA-OR-A5J1 acc_tcga DFS_STATUS   1:Re… PATIENT
#> # … with 6,282 more rows, and abbreviated variable names ¹​clinicalAttributeId,
#> #   ²​dataLevel
```

``` r
get_clinical_by_sample(sample_id = samples, study_id = "acc_tcga") %>%
  head(10)
#> ! No `clinical_attribute` passed. Defaulting to returning
#> all clinical attributes in "acc_tcga" study
#> # A tibble: 10 × 7
#>    uniqueSampleKey                 uniqu…¹ sampl…² patie…³ studyId clini…⁴ value
#>    <chr>                           <chr>   <chr>   <chr>   <chr>   <chr>   <chr>
#>  1 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… CANCER… Adre…
#>  2 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… CANCER… Adre…
#>  3 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… DAYS_T… 4691 
#>  4 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… FRACTI… 0.05…
#>  5 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… IS_FFPE NO   
#>  6 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… MUTATI… 39   
#>  7 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… OCT_EM… TRUE 
#>  8 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… ONCOTR… ACC  
#>  9 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… OTHER_… E403…
#> 10 VENHQS1PUi1BNUoxLTAxOmFjY190Y2… VENHQS… TCGA-O… TCGA-O… acc_tc… PATHOL… TCGA…
#> # … with abbreviated variable names ¹​uniquePatientKey, ²​sampleId, ³​patientId,
#> #   ⁴​clinicalAttributeId
```

``` r
patients <- available_patients(study_id = "acc_tcga") %>%
  pull(patientId) %>%
  head(n = 10)

get_clinical_by_patient(patient_id = patients, study_id = "acc_tcga", 
                        clinical_attribute = "AGE") %>%
  head(10)
```

All functions that pull by study IDs are limited to pulling data from
one study at a time. If you need to pull specific samples from multiple
studies, you likely want to pull by sample ID (instead of study ID) and
supply the function with a dataframe of `sample_study_pairs` that
specify where the function should look for each study. For more
information see the [Overview of Workflow
Vignette](https://www.karissawhiting.com/cbioportalR/articles/overview-of-workflow.html).

## Contributing

Please note that {cbioportalR} is released with a [Contributor Code of
Conduct](https://www.karissawhiting.com/cbioportalR/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

Thank you to contributors!

[@arorarshi](https://github.com/arorarshi),
[@AxelitoMartin](https://github.com/AxelitoMartin),
[@edrill](https://github.com/edrill),
[@jalavery](https://github.com/jalavery),
[@ddsjoberg](https://github.com/ddsjoberg)
[@karomanchuk](https://github.com/karomanchuk)

Thank you [Isaak Liptzin](https://awenfilms.net/) for the hex sticker!
