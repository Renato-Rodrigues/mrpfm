# Bundled mappings — provenance and verification

These files let the PFM stack (mrpfm + pfm) run on a machine with no REMIND input-data folder configured.
They are a **fallback**, not a second opinion: `toolPFMMapping()` reads the madrat
`mappingfolder` first and only falls back here, announcing it when it does.

Layout follows the madrat convention, so `madrat::toolGetMapping(name, type =
"regional", where = "mrpfm")` finds them: `inst/extdata/<type>/<name>`.

## regional/

| file | source | verified against REMIND input data |
|---|---|---|
| `regionmappingH12.csv` | dashboard repo | 249/249 identical |
| `regionmapping_21_EU11.csv` | dashboard repo | 249/249 identical |
| `regionmapping_26.csv` | dashboard repo | 249/249 identical |
| `regionmapping_39.csv` | dashboard repo | 249/249 identical |
| `regionmapping_54.csv` | dashboard repo | 249/249 identical |
| `regionmapping_62.csv` | dashboard repo | 249/249 identical |
| `regionmapping_EU_OECDp.csv` | REMIND input data | copied from it |

Dashboard repo:
<https://github.com/Renato-Rodrigues/regional-carbon-policy-dashboard/tree/main/mappings>

Retrieved 2026-08-14. Every file's byte count matched the GitHub API's reported size,
and each was then compared *semantically* — parsed to country → region and diffed on
the shared country set — against
`<mappingfolder>/regional/<name>`. All six report 249 shared countries and 0
differing assignments.

`regionmapping_EU_OECDp.csv` is the fixed-effects mapping the deployed PSM spec names
(`selected-models-psm.yml`). It is not in the dashboard repo, so it is copied from the
REMIND input data; without it a machine using the bundled fallback could not fit the
deployed model at all.

## Not a regional mapping

`geopoliticalMemberships.csv` sits flat in `extdata/`, not under `regional/`: it is a
country × bloc membership matrix (EU, EUETS, OECD, G7, G20, BRICS, OPEC, NATO, ASEAN,
Mercosur, RCEP, USMCA, AfCFTA, BRI, SCO, GCC, LAS, OIC), not a country → region
aggregation. A flat file is found by both `type = NULL` and `type = "regional"`
lookups, so nothing is lost by placing it correctly.

## The one that is NOT interchangeable

madrat ships its own `regionmappingH12.csv`, and it is an **older version**: 15
countries are assigned to different regions than the copies here.

```
             madrat   ->  here
UKR GEO MDA   REF     ->  NEU
MNG PRK       OAS     ->  REF
GRL SPM       NEU/CAZ ->  EUR
MLI MRT NER TCD  SSA  ->  MEA
ATF           OAS     ->  EUR
CCK CXR       OAS     ->  CAZ
IOT           OAS     ->  IND
```

This is why `toolPFMMapping()` uses a closed search order instead of `where = NULL`.
With `where = NULL` madrat also searches every package in `getConfig("packages")`, and
would happily return madrat's copy — re-aggregating every regional result under a
different definition of the regions, with nothing in the log.

## Refreshing

Re-download from the dashboard repo into `inst/extdata/regional/`, then re-run the
verification — parse both copies to country -> region and diff on the shared country
set, not `diff` on the files (the two differ in line endings while being semantically
identical, so a byte diff tells you nothing):

```r
rd <- function(p) { x <- read.csv(p, sep = ";"); stats::setNames(x$RegionCode, x$CountryCode) }
a <- rd("inst/extdata/regional/regionmapping_21_EU11.csv")
b <- rd(file.path(madrat::getConfig("mappingfolder"), "regional", "regionmapping_21_EU11.csv"))
k <- intersect(names(a), names(b)); sum(a[k] != b[k])   # must be 0
```

Do not edit
these files by hand: they must stay identical to their sources, or the fallback stops
being a fallback and becomes a third, silently different, version.
