# MsBackendTimsTof 0.0

## MsBackendTimsTof 0.0.4

- fixed a problem in `mz`, `intensity`, `peaksData` methods that caused these
  methods to return spectra values in wrong order given a reordered
  `MsBackendTimsTof` object.
- `spectraVariables` and `spectraData` methods.
- fixed a problem in file indexing possibly arising after object subsetting.
- unit tests update.

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
