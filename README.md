# Data and code for Bauer et al. (submitted)

Markus Bauer <a href="https://orcid.org/0000-0001-5372-4174"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Alina Twerski <a href="https://orcid.org/0000-0001-7966-1335"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Christin Juno Laschke <a href="https://orcid.org/0009-0008-5041-4697"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Annika Schmidt <a href="https://orcid.org/0000-0002-6414-2505"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Line Sturm <a href="https://orcid.org/0009-0002-2735-3060"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Miriam Wiesmeier <a href="https://orcid.org/0009-0007-3542-3352"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Anita Kirmer <a href="https://orcid.org/0000-0002-2396-713X"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Vicky M. Temperton <a href="https://orcid.org/0000-0003-0543-4521"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>
Johannes Kollmann <a href="https://orcid.org/0000-0002-4990-3636"><img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="16" height="16"/></a>


Data and code for:

Bauer M, Twerski A, Laschke CJ, Schmidt A, Sturm L, Wiesmeier M, Kirmer A, Temperton VM & Kollmann J (submitted) __Little sign of recovery debt for functional traits when comparing restorations with positive references, but clear delineation of negative references__ &ndash; *XXX* XX, XXX&ndash;XXX.

[![DOI:10.XXX](http://img.shields.io/badge/DOI-10.XXX-informational.svg)](https://doi.org/10.XXX)

**Study region**: [Germany](https://www.openstreetmap.org/#map=7/50.861/12.327&layers=P)
<br>
<br>
## Content of the repository

1.  **Data**: the folder `data` contains
    -   `Raw` and `processed` data of the sites variables (.csv)
2.  **Outputs**: the folder `outputs` contains
    -   The figures generated (.tiff)
    -   The model checks generated from Rmd-files incl. DHARMa plots (.pdf)
    -   The calculated models ready for the Rmd-files (.Rdata)
    -   The tables generated (.html/.csv)

3.  **R**: the folder `R` contains
    -   Data preparation script ('_prepare_data.R')
    -   Scripts to calculate all models ('model_calculation_') (.R)
    -   Scripts to generate all figures and tables ('show_figure_') (.R)
    -   Metadata script for creating EML file ('_metadata.R')

#### Package versioning

The used versions of R and the packages are saved in `2024_grassworks_habitat_type_traits/renv.lock`.

You can restore this state by executing `renv::restore()` in the console.

## Citation

[![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by/4.0/)

This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

When using the **data available** in this repository, please cite the original publication and the dataset.

**Publication**

> Bauer M, Twerski A, Laschke CJ, Schmidt A, Sturm L, Wiesmeier M, Kirmer A, Temperton VM & Kollmann J (submitted) Little sign of recovery debt for functional traits when comparing restorations with positive references, but clear delineation of negative references. &ndash; *XXX* XX, XXX&ndash;XXX. <https://doi.org/10.XXX>

**Dataset**

> Bauer M, Twerski A, Laschke CJ, Schmidt A, Sturm L, Wiesmeier M, Kirmer A, Temperton VM & Kollmann J (2025) Data and code for Bauer et al. (submitted) (v1.0.0) [Data set]. &ndash; *Zenodo*. [<https://doi.org/10.5281/zenodo.XXX>](https://doi.org/10.5281/zenodo.XXX)

This dataset is also linked to PANGAEA
> XXX (XXX) XXX. &ndash; *PANGAEA*. https://doi.org/10.XXX

Contact [markus1.bauer\@tum.de](mailto:markus1.bauer@tum.de) for any further information.
