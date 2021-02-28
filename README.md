
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbioportalR

{cbioportalR} allows you to access cBioPortal’s genomic and clinical
data directly through R. The package wraps cBioPortal’s existing API
endpoints in R so R users can easily leverage cBioPortal’s API. Using
these functions, you can access genomic data on mutations, copy number
alterations and fusions as well as data on tumor mutational burden
(TMB), microsatellite instability status (MSI) and select clinical data
points (depending on the study).

This package was created to work with both [the public cBioPortal
website](https://www.cbioportal.org/), as well as MSK’s private
institutional cbioportal database. To connect to a private database, you
must first get an access token and supply the specific API url at the
beginning of your session (details below).

For more information on cBioPortal, see the following publications:

  - [Gao et al. Sci.
    Signal. 2013](https://pubmed.ncbi.nlm.nih.gov/23550210/)
  - [Cerami et al. Cancer
    Discov. 2012](https://cancerdiscovery.aacrjournals.org/content/2/5/401.abstract)

For full documentation on the cBioPortal API, please see the following
links:

  - [cBioPortal API and API Clients
    documentation](https://docs.cbioportal.org/6.-web-api-and-clients/api-and-api-clients)
  - [full reference documentation for
    API](https://www.cbioportal.org/api/swagger-ui.html#/)

## Installation

You can install the development version of {cbioportalR} with:

``` r
remotes::install_github("karissawhiting/cbioportalR")
```

## Setting up the API

If you are using the public domain <https://www.cbioportal.org/>, you do
not need a token to access data. If you are using a private instance of
cbioportal (like MSK’s institutional database), you will need to acquire
a token and save it to your `.Renviron` file (or wherever you store
credentials).

Simply log in to your institution’s cbioportal website, acquire a token
(Usually through the ‘Web API’ tab) and save it in your `.Renviron`
file. This will save the token as an environmental variable so you do
not have to hard code the secret key in your scripts.

*Tip: The following {usethis} function can easily find and open the
`.Renviron` for you:*

``` r
usethis::edit_r_environ()
```

Paste the token you were given (using the format below) in the .Renviron
file and save the file changes. You may want to save and restart your R
session to ensure the token is saved and recognized.

``` r
CBIOPORTAL_TOKEN = 'YOUR_TOKEN'
```

You can test that your token was saved using:

``` r
library(cbioportalR)
get_cbioportal_token()
```

To reiterate, if you are planning to retrieve data using public
cBioPortal, you do not need a token. If you need to access data on an
institutional cBioPortal page you must get a token first.

*Note: If you are a MSK researcher working on IMPACT, you should connect
to MSK’s cBioPortal instance to get the most up to date IMPACT data, and
you must follow MSK-IMPACT publication guidelines when using the data*

## Setting the database

For every new R session, you need to set your database URL. The
`get_cbioportal_db()` function will set an environmental variable for
your session that tells the package which database to point to for all
API calls.

You can set it to point to the public database with this shortcut:

``` r
library(cbioportalR)
get_cbioportal_db("public")
```

or you can set it to a specific institution database with:

``` r
get_cbioportal_db("<<your institution's url>>/api")
```

## Retrieving data

Once you’ve set your preferred db connection, you can pull data via
study ID or sample ID.

To see available studies (this depends on what cBioPortal database you
are connected to), you can use:

``` r
get_studies() %>% head(n = 10)
#> # A tibble: 10 x 13
#>    name  shortName description publicStudy pmid  citation groups status
#>    <chr> <chr>     <chr>       <lgl>       <chr> <chr>    <chr>   <int>
#>  1 Oral… Head & n… Comprehens… TRUE        2361… Pickeri… ""          0
#>  2 Hepa… HCC (Ins… Whole-exom… TRUE        2582… Schulze… "PUBL…      0
#>  3 Uvea… UM (QIMR) Whole-geno… TRUE        2668… Johanss… "PUBL…      0
#>  4 Neur… NBL (AMC) Whole geno… TRUE        2236… Molenaa… "PUBL…      0
#>  5 Naso… NPC (Sin… Whole exom… TRUE        2495… Lin et … "PUBL…      0
#>  6 Neur… NBL (Col… Whole-geno… TRUE        2646… Peifer … ""          0
#>  7 Myel… MDS (Tok… Whole exom… TRUE        2190… Yoshida… ""          0
#>  8 Insu… Panet (S… Whole exom… TRUE        2432… Cao et … ""          0
#>  9 Pleu… PLMESO (… Whole-exom… TRUE        2548… Guo et … ""          0
#> 10 Pilo… PAST (Na… Whole-geno… TRUE        2381… Jones e… "PUBL…      0
#> # … with 5 more variables: importDate <chr>, allSampleCount <int>,
#> #   studyId <chr>, cancerTypeId <chr>, referenceGenome <chr>
```

To pull mutation data for a particular study ID you can use:

``` r

# As a result you will get a list of dataframes of 1) mutation + fusion and 2) cna.
df <- get_genetics(study_id = "nbl_amc_2012",
                   mutations = TRUE, 
                   cna = FALSE, 
                   fusions = TRUE)

mutations <- df$mut 
df %>% head()
#> $mut
#> # A tibble: 562 x 31
#>    uniqueSampleKey uniquePatientKey molecularProfil… Tumor_Sample_Ba… patientId
#>    <chr>           <chr>            <chr>            <chr>            <chr>    
#>  1 TjU5NVQ6bmJsX2… TjU5NTpuYmxfYW1… nbl_amc_2012_mu… N595T            N595     
#>  2 TjYwOFQ6bmJsX2… TjYwODpuYmxfYW1… nbl_amc_2012_mu… N608T            N608     
#>  3 TjcxOFQ6bmJsX2… TjcxODpuYmxfYW1… nbl_amc_2012_mu… N718T            N718     
#>  4 TjU3MlQ6bmJsX2… TjU3MjpuYmxfYW1… nbl_amc_2012_mu… N572T            N572     
#>  5 Tjc0NFQ6bmJsX2… Tjc0NDpuYmxfYW1… nbl_amc_2012_mu… N744T            N744     
#>  6 TjU2MVQ6bmJsX2… TjU2MTpuYmxfYW1… nbl_amc_2012_mu… N561T            N561     
#>  7 TjU0OFQ6bmJsX2… TjU0ODpuYmxfYW1… nbl_amc_2012_mu… N548T            N548     
#>  8 TjU3MlQ6bmJsX2… TjU3MjpuYmxfYW1… nbl_amc_2012_mu… N572T            N572     
#>  9 TjU3NVQ6bmJsX2… TjU3NTpuYmxfYW1… nbl_amc_2012_mu… N575T            N575     
#> 10 TjUwOFQ6bmJsX2… TjUwODpuYmxfYW1… nbl_amc_2012_mu… N508T            N508     
#> # … with 552 more rows, and 26 more variables: entrezGeneId <int>,
#> #   studyId <chr>, center <chr>, Mutation_Status <chr>, validationStatus <chr>,
#> #   startPosition <int>, endPosition <int>, referenceAllele <chr>,
#> #   proteinChange <chr>, Variant_Classification <chr>,
#> #   functionalImpactScore <chr>, fisValue <dbl>, linkXvar <chr>, linkPdb <chr>,
#> #   linkMsa <chr>, ncbiBuild <chr>, Variant_Type <chr>, keyword <chr>,
#> #   chr <chr>, variantAllele <chr>, refseqMrnaId <chr>, proteinPosStart <int>,
#> #   proteinPosEnd <int>, HGVSp_Short <chr>, Protein_position <int>,
#> #   Hugo_Symbol <chr>
#> 
#> $cna
#> NULL
```
