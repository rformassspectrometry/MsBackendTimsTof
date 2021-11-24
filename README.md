# MsBackendTimsTof


Spectra backend supporting TimsTOF data files using the `opentimsr` package.

The `opentimsr` package uses the C++ library from Bruker for data access. This
needs to be downloaded locally (with
`opentimsr::download_bruker_proprietary_code(<local folder>)` with `<local
folder>` being the directory to which it should be downloaded) and loaded in
each new R session using `setup_bruker_so("<local folder>/libtimsdata.so")`.
