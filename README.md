Type-safe linear algebra library
================================

- General matrix types are implemented in native Haskell.

- The dimensions of matrices are statically typed.

- Provides bindings to high performance C++ linear algebra libraries such Eigen and Spectra.

Following GHC extensions may be needed:

- ScopedTypeVariables
- RankNTypes
- TypeFamilies
- DataKinds

Example
-------

```haskell
let mat = D.matrix [ [1,0,3]
                   , [0,5,6]
                   , [0,0,0] ] :: Matrix 3 3 Double
    mat' = D.convertAny mat :: SparseMatrix 3 3 Double

print mat
print mat'

print $ eigs (sing :: Sing 1) mat == eigs (sing :: Sing 1) mat'

print $ cholesky mat

print $ mat %*% mat %*% mat
print $ mat' %*% mat' %*% mat
```