## v0.3.1 (2022-03-16)

* Update to cmdliner 1.1.0

## v0.3.0 (2022-01-28)

* Exceptions from Owee are caught in `query_abi` and `query_manifest`

## v0.2.0 (2021-12-16)

* Rename binary from solo5-elftool to osolo5-elftool so it can co-exist with
  the C binary
* Implement `query_abi` and `osolo5-elftool query-abi`
* Return a result error instead of an assertion exception on unknown manifest
  entry types
* Remove noop tests

## v0.1.0 (2021-12-15)

* Initial public release
