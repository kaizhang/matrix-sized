#include <Spectra/GenEigsSolver.h>
#include "eigen-runtime.h"
#include <iostream>

using namespace Spectra;
using namespace Eigen;

extern "C" RET spectral_eigs( 
    int k,
    void* d, void* v, 
    const void* p, int n)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    typedef Map< Matrix<T3,Dynamic,Dynamic> > MapComplexMatrix;
    MapMatrix M((T1*)p, n, n);
    MapComplexMatrix D((T3*)d, k, 1);
    MapComplexMatrix V((T3*)v, n, k);

    DenseGenMatProd<double> op(M);
    GenEigsSolver< double, LARGEST_MAGN, DenseGenMatProd<double> > eigs(&op, k, 2*k+1);
    eigs.init();
    int nconv = eigs.compute();
    if(eigs.info() == 0)
        D = eigs.eigenvalues();
        V = eigs.eigenvectors();
    return 0;
}