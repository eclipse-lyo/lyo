# Lyo Store changelog

## [Unreleased/4.0.0]

### Added
### Changed
### Deprecated

- `JenaTdbStoreImpl` has been lagging behind in functionality and will be deprecated in this release.
  - `StoreFactory#inMemory` and `StoreFactory#onDisk` are also deprecated as they rely on this implementation.
  - Use `SparqlStoreImpl#SparqlStoreImpl(JenaQueryExecutor)` to pass a `DatasetQueryExecutorImpl` instance instead.



### Removed
### Fixed
### Security

---

See https://keepachangelog.com/en/1.0.0/ for more info.