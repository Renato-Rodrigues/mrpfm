# Bundled mappings — provenance and verification

These files let the PFM stack (mrpfm + pfm) run on a machine with no REMIND input-data folder configured.
They are a **fallback**, not a second opinion: `toolPFMMapping()` reads the madrat
`mappingfolder` first and only falls back here, announcing it when it does.

Layout follows the madrat convention, so `madrat::toolGetMapping(name, type =
"regional", where = "mrpfm")` finds them: `inst/extdata/<type>/<name>`.

## regional/

| file | source | verified against REMIND input data |
|---|---|---|
| `regionmappingH12.csv` | **the REMIND fork's `config/`** (since 2026-10-02) | byte-identical to it; 249/249 identical to the cluster's REMIND input data |
| `regionmapping_21_EU11.csv` | **the REMIND fork's `config/`** (since 2026-10-02) | byte-identical to it; 249/249 identical to the cluster's REMIND input data |
| `regionmappingH12_future.csv` | dashboard repo (the future region update) | not used by default; the H12 bundled until 2026-10-02 |
| `regionmapping_21_EU11_future.csv` | dashboard repo (the future region update) | not used by default; the EU21 bundled until 2026-10-02 |
| `regionmapping_26.csv` | dashboard repo | 249/249 identical |
| `regionmapping_39.csv` | dashboard repo | 249/249 identical |
| `regionmapping_54.csv` | dashboard repo | 249/249 identical |
| `regionmapping_62.csv` | dashboard repo | 249/249 identical |
| `regionmapping_EU_OECDp.csv` | REMIND input data | copied from it |

**H12 and EU21 are REMIND's own region definitions (decided 2026-10-02).** The REMIND version the
PFM couples to solves on its `config/regionmappingH12.csv` and `config/regionmapping_21_EU11.csv`,
and the cluster's REMIND input data (`/p/projects/rd3mod/inputdata/mappings/regional/`) holds the
same assignments (`tools/compareMappings.R`, 0 reassigned). The dashboard-repo copies used until
then are a **future update** of the region definitions: they assign 15 countries differently
(UKR, GEO, MDA in NEU rather than REF; MNG, PRK in REF rather than OAS; Chad, Mali, Mauritania,
Niger in MEA rather than SSA; Greenland, Saint Pierre and Miquelon, five small territories). The
cluster always resolved to the REMIND version (through its mappingfolder); a machine without one -
the workstation - fell back to the future version, so its downscaling, regional aggregation and
`toolImputeMedians` used other regions than REMIND for those 15 countries. They are kept, under their
own names, as `regionmappingH12_future.csv` and `regionmapping_21_EU11_future.csv`: nothing reads
them unless asked by name (`toolPFMMapping("regionmappingH12_future.csv")`), and when REMIND adopts
the new regions they become the `regionmappingH12.csv` / `regionmapping_21_EU11.csv` here.

Dashboard repo (the other regional files, and the two `_future` files):
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

madrat ships its own `regionmappingH12.csv`, and it is the **same version REMIND uses**; the
**future** definitions (`*_future.csv`) assign 15 countries differently:

```
             madrat / REMIND -> *_future
UKR GEO MDA   REF     ->  NEU
MNG PRK       OAS     ->  REF
GRL SPM       NEU/CAZ ->  EUR
MLI MRT NER TCD  SSA  ->  MEA
ATF           OAS     ->  EUR
CCK CXR       OAS     ->  CAZ
IOT           OAS     ->  IND
```

`toolPFMMapping()` still uses a closed search order rather than `where = NULL`: with
`where = NULL` madrat searches every package in `getConfig("packages")`, and which copy it
returns would depend on what happens to be installed.

## Refreshing

H12 and EU21: copy `config/regionmappingH12.csv` and `config/regionmapping_21_EU11.csv` from the
REMIND fork the PFM couples to, then run `Rscript tools/compareMappings.R <REMIND config>` from the
project root (every comparison must report 0 reassigned). The other regional files: re-download
from the dashboard repo into `inst/extdata/regional/`, then re-run the verification — parse both copies to country -> region and diff on the shared country
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
