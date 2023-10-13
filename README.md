# MsBackendTimsTof

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsBackendTimsTof/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsBackendTimsTof/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/github/rformassspectrometry/MsBackendTimsTof/branch/main/graph/badge.svg?token=DMFOBVJFJQ)](https://codecov.io/github/rformassspectrometry/MsBackendTimsTof)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)

Spectra backend supporting TimsTOF data files *via* the `opentimsr` package
which bases on the [OpenTIMS](https://github.com/michalsta/opentims) C++
library. To allow extraction of all spectra and peaks variables the C++ library
from Bruker is required, which can be installed using
`opentimsr::download_bruker_proprietary_code(<local folder>)` with `<local
folder>` being the directory to which it should be downloaded).

It is suggested to keep this library in a local folder and to define an
environment variable called `TIMSTOF_LIB` that defines the full path where this
file is located (i.e. a character string defining the full file path with the
file name). This variable can either be defined system wide, or within the
*.Rprofile* file. An example entry in a *.Rprofile* could for example be:

```
options(TIMSTOF_LIB = "/home/jo/lib/libtimsdata.so")
```

For more information see the package
[homepage](https://rformassspectrometry.github.io/MsBackendTimsTof).


**Note**: currently `opentimsr` is no longer available on CRAN and needs to be
installed from github:

```r
install_github("michalsta/opentims", subdir="opentimsr")
```


# Contributions

Contributions are highly welcome and should follow the [contribution
guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).
Also, please check the coding style guidelines in the [RforMassSpectrometry
vignette](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html).
