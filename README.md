# taxodist app <img src="https://img.shields.io/badge/shiny-app-blue?logo=r&logoColor=white" align="right" height="28"/>

[![Launch App](https://img.shields.io/badge/Launch%20App-shinyapps.io-4CAF50?style=flat&logo=r&logoColor=white)](https://3w6g1b-rodrigo-villa.shinyapps.io/taxodist_app/)
[![taxodist on CRAN](https://img.shields.io/cran/v/taxodist?label=taxodist&color=2D5016)](https://cran.r-project.org/web/packages/taxodist/index.html)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/taxodist)](https://cran.r-project.org/web/packages/taxodist/index.html)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R](https://img.shields.io/badge/built%20with-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)

A multilingual Shiny web application for computing taxonomic distances and exploring phylogenetic lineages, powered by the [taxodist](https://cran.r-project.org/web/packages/taxodist/index.html) R package and [The Taxonomicon](https://taxonomicon.taxonomy.nl/) database. 

Available in English, Portuguese, Spanish, French, and German.

---

## Live App

**[https://3w6g1b-rodrigo-villa.shinyapps.io/taxodist_app/](https://3w6g1b-rodrigo-villa.shinyapps.io/taxodist_app/)**

---

## Features

The app exposes the main functions of the `taxodist` package through an interactive interface with six tabs:

| Tab | Description |
|-----|-------------|
| **Pairwise Distance** | Compute the taxonomic distance between any two taxa, with full lineage comparison and MRCA highlighted |
| **Distance Matrix** | Build a pairwise distance matrix for a list of taxa, with an interactive table and a publication-ready dendrogram |
| **Closest Relative** | Given a query taxon and a set of candidates, rank them by taxonomic proximity |
| **Lineage Explorer** | Retrieve the full lineage of any taxon and check clade membership |
| **Coverage Check** | Verify which taxa in a list are present in The Taxonomicon database |
| **Filter by Clade** | Filter a list of taxa, retaining only those belonging to a given clade |

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

$$d(A, B) = \frac{1}{\text{depth}(\text{MRCA}(A, B))}$$

where MRCA is the Most Recent Common Ancestor of A and B in The Taxonomicon hierarchy, and depth is its position from the root. Taxa that share a more recent (deeper) ancestor receive a smaller distance value.

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
| [`ggdendro`](https://cran.r-project.org/package=ggdendro) | Dendrogram data extraction |
| [`DT`](https://rstudio.github.io/DT/) | Interactive distance matrix table |

---

## Citation

If you use `taxodist` or this app in your research, please cite the package:

```r
citation("taxodist")
```

---

## Author

Feedback and contributions are welcome. Feel free to [open an issue](https://github.com/rodrigosqrt3/taxodist_app/issues) or a pull request.