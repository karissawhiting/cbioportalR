
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/karissawhiting/cbioportalR/workflows/R-CMD-check/badge.svg)](https://github.com/karissawhiting/cbioportalR/actions)
<!-- badges: end -->

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
must first get an access token (or whatever credentials your institution
requires) and supply the specific API url at the beginning of your
session (details below).

For more information on cBioPortal, see the following publications:

-   [Gao et al. Sci. Signal.
    2013](https://pubmed.ncbi.nlm.nih.gov/23550210/)
-   [Cerami et al. Cancer Discov.
    2012](https://cancerdiscovery.aacrjournals.org/content/2/5/401.abstract)

For full documentation on the cBioPortal API, please see the following
links:

-   [cBioPortal API and API Clients
    documentation](https://docs.cbioportal.org/6.-web-api-and-clients/api-and-api-clients)
-   [full reference documentation for
    API](https://www.cbioportal.org/api/swagger-ui.html#/)

<img src="man/figures/cbp-diagram.png" width="60%" style="display: block; margin: auto;" />

``` r
x <- tibble::tibble(
  level = c("Database", "Study", "Molecular Profile", "Sample"),
  
  functions = c("`available_studies()`", "`get_genetics_by_study()`", "`get_genetics_by_study()`", "`get_genetics_by_sample`")) %>%
  mutate(level = forcats::fct_relevel(level, "Database",
                                      "Study", "Molecular Profile", "Sample"))

table_funs <- x %>%
   gt::gt() %>%
  gt::fmt_markdown(columns = everything()) %>%
  gt::cols_label(level = gt::md("**Level**"),
             functions = gt::md("**Functions**")) %>%
  gt::tab_options(table.font.size = 'small') %>%
  gt::data_color(
    columns = level,
    colors = scales::col_factor(
      palette = c("BlueViolet", "SlateBlue", "DodgerBlue", "LightSeaGreen", "SandyBrown"),
      domain = NULL,
#      reverse = TRUE
    ),
    alpha = 0.6
  ) %>%
  gt::tab_style(
    style = list(
      gt::cell_text(weight = "bold")
      ),
    locations = gt::cells_body(
      columns = level,
      rows = everything()
    )
  )

table_funs
#> Warning in `[<-.factor`(`*tmp*`, !is.na(x), value = c("<div
#> class='gt_from_md'><p>Database</p>\n</div>", : invalid factor level, NA
#> generated
```

<div id="vxszdwhqkg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vxszdwhqkg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: small;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vxszdwhqkg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxszdwhqkg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vxszdwhqkg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vxszdwhqkg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxszdwhqkg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxszdwhqkg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vxszdwhqkg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vxszdwhqkg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vxszdwhqkg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vxszdwhqkg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vxszdwhqkg .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#vxszdwhqkg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vxszdwhqkg .gt_from_md > :first-child {
  margin-top: 0;
}

#vxszdwhqkg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vxszdwhqkg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vxszdwhqkg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#vxszdwhqkg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxszdwhqkg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#vxszdwhqkg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxszdwhqkg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vxszdwhqkg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vxszdwhqkg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxszdwhqkg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxszdwhqkg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#vxszdwhqkg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxszdwhqkg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#vxszdwhqkg .gt_left {
  text-align: left;
}

#vxszdwhqkg .gt_center {
  text-align: center;
}

#vxszdwhqkg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vxszdwhqkg .gt_font_normal {
  font-weight: normal;
}

#vxszdwhqkg .gt_font_bold {
  font-weight: bold;
}

#vxszdwhqkg .gt_font_italic {
  font-style: italic;
}

#vxszdwhqkg .gt_super {
  font-size: 65%;
}

#vxszdwhqkg .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Level</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Functions</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center" style="background-color: rgba(138,43,226,0.6); color: #FFFFFF; font-weight: bold;">Database</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>available_studies()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_center" style="background-color: rgba(93,109,221,0.6); color: #FFFFFF; font-weight: bold;">Study</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>get_genetics_by_study()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_center" style="background-color: rgba(48,167,198,0.6); color: #000000; font-weight: bold;">Molecular Profile</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>get_genetics_by_study()</code></p>
</div></td></tr>
    <tr><td class="gt_row gt_center" style="background-color: rgba(244,164,96,0.6); color: #000000; font-weight: bold;">Sample</td>
<td class="gt_row gt_left"><div class='gt_from_md'><p><code>get_genetics_by_sample</code></p>
</div></td></tr>
  </tbody>
  
  
</table>
</div>

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
set_cbioportal_db("public")
#> ✓ You are successfully connected!
#> ✓ base_url for this R session is now set to "www.cbioportal.org/api"
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
available_studies() %>% head(n = 10)
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

To pull mutation data for a particular study ID you can use:

``` r
# As a result you will get a list of dataframes of 1) mutation + fusion and 2) cna.
df <- get_genetics_by_study(study_id = "nbl_amc_2012")
#> ℹ Returning all data for the "nbl_amc_2012_mutations" molecular profile in the "nbl_amc_2012" study
#> ! No "cna" data returned. Error:  No molecular profile for `data_type = cna` found in "nbl_amc_2012".  See
#> `available_profiles('nbl_amc_2012')`
#> ! No "fusion" data returned. Error:  No molecular profile for `data_type = fusion` found in "nbl_amc_2012".
#> See `available_profiles('nbl_amc_2012')`

mutations <- df$mut 
df %>% head()
#> $mutation
#> # A tibble: 562 × 28
#>    uniqueSampleKey          uniquePatientKey molecularProfil… sampleId patientId
#>    <chr>                    <chr>            <chr>            <chr>    <chr>    
#>  1 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  2 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  3 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  4 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  5 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  6 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  7 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  8 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#>  9 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#> 10 TjA0MVQ6bmJsX2FtY18yMDEy TjA0MTpuYmxfYW1… nbl_amc_2012_mu… N041T    N041     
#> # … with 552 more rows, and 23 more variables: entrezGeneId <int>,
#> #   studyId <chr>, center <chr>, mutationStatus <chr>, validationStatus <chr>,
#> #   startPosition <int>, endPosition <int>, referenceAllele <chr>,
#> #   proteinChange <chr>, mutationType <chr>, functionalImpactScore <chr>,
#> #   fisValue <dbl>, linkXvar <chr>, linkPdb <chr>, linkMsa <chr>,
#> #   ncbiBuild <chr>, variantType <chr>, keyword <chr>, chr <chr>,
#> #   variantAllele <chr>, refseqMrnaId <chr>, proteinPosStart <int>, …
```
