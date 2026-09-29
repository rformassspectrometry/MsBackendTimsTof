# Bruker TimsTOF data file format support for *Spectra*

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsBackendTimsTof/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsBackendTimsTof/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/github/rformassspectrometry/MsBackendTimsTof/branch/main/graph/badge.svg?token=DMFOBVJFJQ)](https://codecov.io/github/rformassspectrometry/MsBackendTimsTof)
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

For extraction of all spectra and peaks variables from the TimsTOF file format,
the shared C++ library from Bruker is required. This has to be installed using
`opentimsr::download_bruker_proprietary_code(<local folder>)` with `<local
folder>` being the directory to which it should be downloaded).

It is suggested to keep this library in a local folder and to define an
environment variable called `TIMSTOF_LIB` that defines the full path where this
file is located (i.e., a character string defining the full file path with the
file name). This variable can either be defined system-wide, or within the
*.Rprofile* file in a user's home folder. An example entry in a *.Rprofile*:

```
options(TIMSTOF_LIB = "/home/jo/lib/libtimsdata.so")
```

For more information see the package
[homepage](https://rformassspectrometry.github.io/MsBackendTimsTof).

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
