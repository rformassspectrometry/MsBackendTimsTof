# MsBackendTimsTof

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsBackendTimsTof/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsBackendTimsTof/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov.io](http://codecov.io/github/rformassspectrometry/MsBackendTimsTof/coverage.svg?branch=main)](http://codecov.io/github/rformassspectrometry/MsBackendTimsTof?branch=main)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)

Spectra backend supporting TimsTOF data files using the `opentimsr` package.

The `opentimsr` package uses the C++ library from Bruker for data access. This
needs to be downloaded locally (with
`opentimsr::download_bruker_proprietary_code(<local folder>)` with `<local
folder>` being the directory to which it should be downloaded) and loaded in
each new R session using `setup_bruker_so("<local folder>/libtimsdata.so")`.

See the package vignette for installation instructions and general usage.