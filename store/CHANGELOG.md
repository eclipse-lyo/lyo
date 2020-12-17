# Lyo Store changelog

## [4.0.0]

### Deprecated

- `JenaTdbStoreImpl` has been lagging behind in functionality and will be deprecated in this release.
  - `StoreFactory#inMemory` and `StoreFactory#onDisk` are also deprecated as they rely on this implementation.
  - Use `SparqlStoreImpl#SparqlStoreImpl(JenaQueryExecutor)` to pass a `DatasetQueryExecutorImpl` instance instead.

---

See https://keepachangelog.com/en/1.0.0/ for more info.