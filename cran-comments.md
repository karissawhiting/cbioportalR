## Test environments
- Ubuntu 16.04.6 LTS (on github actions), devel, release, oldrel
- macOS (on github actions), release
- win-builder devel
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

**Note 1**: These are names, not mispelling
```
❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Karissa Whiting <karissa.whiting@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    cBioPortal (2:39, 10:72)
    cBioPortal's (11:24)
    genomic (10:40, 12:45)
    microsatellite (13:5)
```

**Note 2**: This note only occurs on windows and I think can be ignored

```
❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
  
  
## Downstream dependencies
There are currently no downstream dependencies for this package

## Additional Comments
Thank you!
