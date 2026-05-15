# Data for the Political Feasibility Module (PFM) for IAMs

R package **mrpfm**, version **0.2.0**

   [![R build status](https://github.com/pik-piam/mrpfm/workflows/check/badge.svg)](https://github.com/pik-piam/mrpfm/actions) [![codecov](https://codecov.io/gh/pik-piam/mrpfm/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrpfm) 

## Purpose and Functionality

Data ingestion and preprocessing for the Political Feasibility Module (PFM),
    following the madrat framework (read*, calc*, convert*, tool* conventions).
    Provides magpie objects consumed by the pfm package.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrpfm")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **mrpfm** in publications use:

Rodrigues R, Kriegler E (2026). "mrpfm: Data for the Political Feasibility Module (PFM) for IAMs." Version: 0.2.0, <https://github.com/pik-piam/mrpfm>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrpfm: Data for the Political Feasibility Module (PFM) for IAMs},
  author = {Renato Rodrigues and Elmar Kriegler},
  date = {2026-05-15},
  year = {2026},
  url = {https://github.com/pik-piam/mrpfm},
  note = {Version: 0.2.0},
}
```
