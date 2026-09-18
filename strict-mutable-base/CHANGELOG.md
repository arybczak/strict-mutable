# strict-mutable-base-2.0.0.1 (????-??-??)
* Documentation of each operation now points to its lazy counterpart in `base`
  and states which values are evaluated to WHNF.
* Use `pure $!` instead of `evaluate` to force values. This results in better
  Core and stricter demand signatures for operations that force their value.

# strict-mutable-base-2.0.0.0 (2026-08-19)
* Drop support for GHC < 8.10.
* Drop ticks in names of all operations and types to make the modules
  forward-compatible with versions to-be-included in `base`
  (https://github.com/haskell/core-libraries-committee/issues/341).

# strict-mutable-base-1.1.0.0 (2024-09-04)
* Rename `getChanContents'` to `getChan'Contents` for consistency.

# strict-mutable-base-1.0.0.0 (2024-09-02)
* Initial release.
