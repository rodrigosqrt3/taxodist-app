# taxodist app <img src="https://img.shields.io/badge/shiny-app-blue?logo=r&logoColor=white" align="right" height="28"/>

[![Launch App](https://img.shields.io/badge/Launch%20App-shinyapps.io-4CAF50?style=flat&logo=r&logoColor=white)](https://3w6g1b-rodrigo-villa.shinyapps.io/taxodist_app/)
[![taxodist on CRAN](https://img.shields.io/cran/v/taxodist?label=taxodist&color=2D5016)](https://cran.r-project.org/web/packages/taxodist/index.html)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/taxodist)](https://cran.r-project.org/web/packages/taxodist/index.html)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R](https://img.shields.io/badge/built%20with-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

A multilingual Shiny web application for computing taxonomic distances and exploring taxonomic hierarchies, powered by the [taxodist](https://cran.r-project.org/web/packages/taxodist/index.html) R package and [The Taxonomicon](https://taxonomicon.taxonomy.nl/) database. 

Available in English, Portuguese, Spanish, French, and German.

---

## Live App

**[https://3w6g1b-rodrigo-villa.shinyapps.io/taxodist_app/](https://3w6g1b-rodrigo-villa.shinyapps.io/taxodist_app/)**

---

## Features

The app exposes the main functions of the `taxodist` package through an interactive interface with eight tabs:

| Tab | Description |
|-----|-------------|
| **Pairwise Distance** | Compute the taxonomic distance between any two taxa, with full lineage comparison and MRCA highlighted |
| **Distance Matrix** | Build a pairwise distance matrix from pasted names or a CSV file, inspect the table, and explore the classification tree (named clades at every split) and PCoA |
| **Between Groups** | Compute only the distances from one group of taxa to another (e.g. community members to reference taxa) |
| **Closest Relative** | Given a query taxon and a set of candidates, rank them by taxonomic proximity |
| **Lineage Explorer** | Retrieve the full lineage of any taxon, check clade membership, and click any node to explore that clade |
| **Search Database** | Search The Taxonomicon for candidate records and numeric identifiers; select a row to open its lineage |
| **Coverage Check** | Verify which taxa in a list are present in The Taxonomicon database (taxa that could not be checked because the server was unreachable are reported separately) |
| **Filter by Clade** | Filter a list of taxa, retaining only those belonging to a given clade |

Results can be downloaded (matrices and tables as CSV, plots as PNG, the
classification tree as Newick, and a portable `taxodist` analysis bundle as
JSON). Pairwise comparisons, lineages, and matrices can be shared with a link
that reopens the same query, for example
`?tab=pairwise&a=Tyrannosaurus&b=Velociraptor` or `?tab=lineage&taxon=Homo`.
Names that are not found, and notes from The Taxonomicon such as the entry
chosen for an ambiguous name, are shown in the interface.

The matrix and coverage tabs accept comma- or semicolon-separated CSV files.
Recognized column names include `taxon`, `taxa`, `species`, `name`, and
`scientific_name`; in a headerless file, the first column is used. The app also
offers a downloadable template; [`example_taxa.csv`](example_taxa.csv) can be
used as a quick upload test.

---

## The `taxodist` package

This app is a graphical front-end for the [`taxodist`](https://cran.r-project.org/web/packages/taxodist/index.html) R package. To use `taxodist` directly in R:

```r
install.packages("taxodist")
library(taxodist)

# Pairwise distance
taxo_distance("Tyrannosaurus", "Velociraptor")

# Distance matrix
distance_matrix(c("Homo", "Canis", "Quercus", "Drosophila"))

# Full lineage
get_lineage("Homo sapiens")

# Check clade membership
is_member("Tyrannosaurus", "Theropoda")
```

CRAN page: [https://cran.r-project.org/web/packages/taxodist/index.html](https://cran.r-project.org/web/packages/taxodist/index.html)

---

## How taxonomic distance is computed

The distance between two taxa A and B is defined as:

$$
d(A,B) =
\begin{cases}
0, & A = B, \\
\dfrac{1}{\text{depth}(\text{MRCA}(A,B))}, & A \ne B.
\end{cases}
$$

where MRCA is the Most Recent Common Ancestor of A and B in The Taxonomicon hierarchy, and depth is its position from the root. Taxa that share a more recent (deeper) ancestor receive a smaller distance value. Zero is reserved for identical hierarchy nodes; distinct ancestor-descendant pairs therefore have positive distance.

These values describe depth in a taxonomic classification. They are not
evolutionary time, genetic distance, or phylogenetic branch lengths. Likewise,
the dendrogram and PCoA are representations of the supplied hierarchy-distance
matrix, not independently inferred phylogenies.

---

## Dependencies

| Package | Role |
|---------|------|
| [`taxodist`](https://cran.r-project.org/web/packages/taxodist/index.html) | Taxonomic distance computation via The Taxonomicon |
|[`shiny`](https://shiny.posit.co/) | Web application framework |
| [`shiny.i18n`](https://cran.r-project.org/package=shiny.i18n) | Internationalization / Multilingual support |
| [`bslib`](https://rstudio.github.io/bslib/) | Bootstrap 5 theming |
| [`ggplot2`](https://ggplot2.tidyverse.org/) | Dendrogram & Ordination plotting |
| [`ggrepel`](https://cran.r-project.org/package=ggrepel) | Text label repulsion for PCoA plots |
| [`DT`](https://rstudio.github.io/DT/) | Interactive distance matrix table |

---

## Deployment

The app ships with a lineage cache for the example taxa so that the first
queries are fast. Before every deployment, rebuild it with the same `taxodist`
version that will be installed on the server:

```r
setwd("taxodist_app")
source("build_cache.R")   # writes lineage-cache.rds and lineage-cache.dcf
rsconnect::deployApp()
```

`lineage-cache.dcf` records the `taxodist` version used. The app ignores the
cache when that version differs from the installed one, so lineages parsed by
an older version are never served.

---

## Citation

If you use `taxodist` or this app in your research, please cite the package:

```r
citation("taxodist")
```

---

## Author

Feedback and contributions are welcome. Feel free to [open an issue](https://github.com/rodrigosqrt3/taxodist_app/issues) or a pull request.
