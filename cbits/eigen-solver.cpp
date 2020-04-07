#include <Spectra/GenEigsSolver.h>
#include <Spectra/SymEigsSolver.h>
#include "eigen-runtime.h"
#include <Eigen/Sparse>
#include <Spectra/MatOp/SparseGenMatProd.h>

using namespace Spectra;
using namespace Eigen;

extern "C" RET eigen_eig( 
    void* d, void* v, 
    const void* p, int n)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    typedef Map< Matrix<T3,Dynamic,Dynamic> > MapComplexMatrix;
    MapMatrix M((T1*)p, n, n);
    MapComplexMatrix D((T3*)d, n, 1);
    MapComplexMatrix V((T3*)v, n, n);
    EigenSolver<MatrixXd> es(M);
    D = es.eigenvalues();
    V = es.eigenvectors();
}

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
    int ncv = 2 * k + 1;
    ncv = (ncv <= n) ? ncv : n;
    GenEigsSolver< double, LARGEST_MAGN, DenseGenMatProd<double> > eigs(&op, k, ncv);
    eigs.init();
    int nconv = eigs.compute();
    if(eigs.info() == 0)
        D = eigs.eigenvalues();
        V = eigs.eigenvectors();
    return 0;
}

extern "C" RET spectral_eigsh( 
    int k,
    void* d, void* v, 
    const void* p, int n)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    MapMatrix M((T1*)p, n, n);
    MapMatrix D((T1*)d, k, 1);
    MapMatrix V((T1*)v, n, k);

    DenseGenMatProd<double> op(M);
    int ncv = 2 * k;
    ncv = (ncv <= n) ? ncv : n;
    SymEigsSolver< double, LARGEST_MAGN, DenseGenMatProd<double> > eigsh(&op, k, ncv);
    eigsh.init();
    int nconv = eigsh.compute();
    if(eigsh.info() == 0)
        D = eigsh.eigenvalues();
        V = eigsh.eigenvectors();
    return 0;
}

extern "C" RET spectral_seigs( 
    int k,
    void* d, void* v,
    const void* values,
    const void* outerIndexPtr,
    const void* innerIndices,
    int n, int s)
{
    typedef Map< Matrix<T3,Dynamic,Dynamic> > MapComplexMatrix;
    typedef Map<const SparseMatrix<T1> > MapSparseMatrix;
    MapSparseMatrix M(n, n, s, (int*)outerIndexPtr, (int*)innerIndices, (T1*)values);
    MapComplexMatrix D((T3*)d, k, 1);
    MapComplexMatrix V((T3*)v, n, k);

    SparseGenMatProd<double> op(M);
    int ncv = 2 * k + 1;
    ncv = (ncv <= n) ? ncv : n;
    GenEigsSolver< double, LARGEST_MAGN, SparseGenMatProd<double> > eigs(&op, k, ncv);
    eigs.init();
    int nconv = eigs.compute();
    if(eigs.info() == 0)
        D = eigs.eigenvalues();
        V = eigs.eigenvectors();
    return 0;
}

extern "C" RET spectral_seigsh( 
    int k,
    void* d, void* v,
    const void* values,
    const void* outerIndexPtr,
    const void* innerIndices,
    int n, int s)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    typedef Map<const SparseMatrix<T1> > MapSparseMatrix;
    MapSparseMatrix M(n, n, s, (int*)outerIndexPtr, (int*)innerIndices, (T1*)values);
    MapMatrix D((T1*)d, k, 1);
    MapMatrix V((T1*)v, n, k);

    SparseGenMatProd<double> op(M);
    int ncv = 2 * k;
    ncv = (ncv <= n) ? ncv : n;
    SymEigsSolver< double, LARGEST_MAGN, SparseGenMatProd<double> > eigsh(&op, k, ncv);
    eigsh.init();
    int nconv = eigsh.compute();
    if(eigsh.info() == 0)
        D = eigsh.eigenvalues();
        V = eigsh.eigenvectors();
    return 0;
}


template <class T>
RET cholesky(void* px, const void* pa, int n)
{
    typedef Map< Matrix<T,Dynamic,Dynamic> > MapMatrix;
    MapMatrix x((T*)px, n, n);
    MapMatrix A((T*)pa, n, n);
    x = A.llt().matrixL();
    return 0;
}
API(cholesky, (int code,
    void* px, const void* pa, int n), (px,pa,n));


template <class T, class TT>
RET bdcsvd(
    void* pu, void* ps, void* pv, 
    const void* px, int r, int c)
{
    int m = r < c ? r : c;
    typedef Map< Matrix<T,Dynamic,Dynamic> > MapMatrix;
    typedef Map< Matrix<TT,Dynamic,Dynamic> > MapMatrix2;
    MapMatrix A((T*)px, r, c);
    MapMatrix U((T*)pu, r, m);
    MapMatrix2 s((TT*)ps, m, 1);
    MapMatrix V((T*)pv, c, m);
    BDCSVD< Matrix<T,Dynamic,Dynamic>> svd(A, ComputeThinU|ComputeThinV);
    U = svd.matrixU();
    V = svd.matrixV();
    s = svd.singularValues();
    return 0;
}
API2(bdcsvd, (int code,
    void* pu, void* ps, void* pv, const void* px, int r, int c), (pu,ps,pv,px,r,c));