# Bruker TimsTOF data file format support for *Spectra*

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsBackendTimsTof/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsBackendTimsTof/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/gh/rformassspectrometry/MsBackendTimsTof/graph/badge.svg?token=DMFOBVJFJQ)](https://codecov.io/gh/rformassspectrometry/MsBackendTimsTof)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)

The *MsBackendTimsTof* R package provides a *backend* for
[`Spectra`](https://bioconductor.org/packages/Spectra) objects enabling direct
import of MS data from Bruker TimsTOF *.d* data files. The package relies on the
*opentimsr* R package for some functionality. The *opentimsr* is an R wrapper
for the [OpenTIMS](https://github.com/michalsta/opentims) C++ library.

## Installation

The *opentimsr* package is no longer available on CRAN and hence needs to be
installed from GitHub using:

```r
remotes::install_github("michalsta/opentims/src/opentimsr")
```

The *MsBackendTimsTof* package can then be installed from GitHub using

```r
remotes::install_github("RforMassSpectrometry/MsBackendTimsTof")
```

For extraction of all spectra and peaks variables from the TimsTOF file format a
*converter library* is needed. This can be either the *open-source* library
shipped with *opentimsr*, or the shared C++ library from Bruker. By default,
*MsBackendTimsTof* uses, and loads the open-source library during the
`library(MsBackendTimsTof)` call. The `setup_converter_library()` function
can be used to download and select the Bruker library instead.

---

## 🤝 Contribution

Please help us improving and completing the package! Any type of contribution
welcome :open_hands: - including discussions, suggestions or actual code. Don't
be afraid - we're friendly :relaxed:! :point_right: get involved by opening an
issue.

Please also check out the [**RforMassSpectrometry Contributions
Guide**](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).

### 📜 Code of Conduct

We follow the [**RforMassSpectrometry Code of
Conduct**](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#code-of-conduct)
to maintain an inclusive and respectful community.
