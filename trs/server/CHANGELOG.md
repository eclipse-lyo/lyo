# Lyo TRS Server library changelog

## 4.0 (draft)

- `ChangeHistories` has been extracted to the `PagedTrs` interface and the implementing class has been renamed to `InmemPagedTrs`.
- `TrackedResourceSetService` is no longer abstract (still extendable).
  - `TrackedResourceSetService` supports constructor dependency injection of the `IChangeHistories` instance.
  - You can still extend it and override `getChangeHistories()` returning an instance of `SimpleChangeHistories` to get the same behaviour as in the earlier release.
- `TrackedResourceSetService` always has a `@Path("/trs")` (because it's hardcoded in other places).
- Unit tests are added based on the Jersey test library and Grizzly embedded server.

> Where is Lyo 3.0? There was a Lyo 3.0.0-SNAPSHOT development effort long time ago, which has been since abandoned and merged into Lyo 2.2. Furthermore, next version of OSLC will be OSLC 3 and Lyo 3 might mislead users that it adds OSLC 3 support.

## 2.4

- First release; see [release notes](https://open-services.net/news/2018-eclipse-lyo-2.4.0/) for further details.
