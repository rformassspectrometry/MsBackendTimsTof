# MsBackendTimsTof 0.1

## MsBackendTimsTof 0.1.0

- Import `coreSpectraVariables` from `Spectra` and thus depend on `Spectra`
  version >= 1.5.14.
- Return `inv_ion_mobility` as a `spectraVariable`.

# MsBackendTimsTof 0.0

## MsBackendTimsTof 0.0.8

- Add documentation and vignette.

## MsBackendTimsTof 0.0.7

- Added `msLevel` to extract MS level of each spectrum in `MsBackendTimsTof`
  objects.

## MsBackendTimsTof 0.0.6

- Fix issue in `spectraData` when parameter `columns` of length 1 is provided
  (issue [#7]
  (https://github.com/rformassspectrometry/MsBackendTimsTof/issues/7)).

## MsBackendTimsTof 0.0.5

- Add test data.
- Add `show` method.
- Fix issue in `peaksData` with duplicated spectra (e.g. due to
  `sps[c(1, 1, 1)]`).

## MsBackendTimsTof 0.0.4

- Fixed a problem in `mz`, `intensity`, `peaksData` methods that caused these
  methods to return spectra values in wrong order given a reordered
  `MsBackendTimsTof` object.
- `spectraVariables` and `spectraData` methods.
- Fixed a problem in file indexing possibly arising after object subsetting.
- Unit tests update.

## MsBackendTimsTof 0.0.3

- Export `MsBackendTimsTof` function.
- Refactor extraction of general spectra variables.

## MsBackendTimsTof 0.0.2

- `mz`, `intensity`, `peaksData` methods to access m/z, intensities, peak
  matrices of spectra in the backend.
- `rtime` method to access retention time of spectra in the backend.

## MsBackendTimsTof 0.0.1

- `MsBackendTimsTof` class definition.
- `backendInitialize` method to initialize a `MsBackendTimsTof` backend.
- `[` method to subset the backend object.
- `length` method to get object length (number of spectra in the backend)
- `dataStorage` method to get the TimsTOF ’*.d’ folders where each spectra is
  retrieved from.
