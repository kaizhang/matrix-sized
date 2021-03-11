#include "eigen-runtime.h"
#include <Eigen/Core>
#include <Eigen/SparseCore>
#include <Eigen/Sparse>
#include <Spectra/GenEigsSolver.h>
#include <Spectra/GenEigsRealShiftSolver.h>
#include <Spectra/SymEigsSolver.h>
#include <Spectra/SymEigsShiftSolver.h>
#include <Spectra/SymGEigsSolver.h>
#include <Spectra/MatOp/DenseSymMatProd.h>
#include <Spectra/MatOp/SparseGenRealShiftSolve.h>
#include <Spectra/MatOp/SparseSymShiftSolve.h>
#include <Spectra/MatOp/SparseCholesky.h>
#include <Spectra/MatOp/SparseGenMatProd.h>
#include <Spectra/Util/GEigsMode.h>

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

extern "C" const int spectral_eigs( 
    const int k,   // number of eigenvectors to return
    void* d, void* v,  // pointer to the results
    const void* p, const int n,  // pointer to the input and dimensionality
    const int ncv, int maxit, double tol,
    const int mode, const double sigma,
    const int select)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    typedef Map< Matrix<T3,Dynamic,Dynamic> > MapComplexMatrix;
    MapMatrix M((T1*)p, n, n);
    MapComplexMatrix D((T3*)d, k, 1);
    MapComplexMatrix V((T3*)v, n, k);

    if(mode == 0) {
        DenseGenMatProd<double> op(M);
        GenEigsSolver<DenseGenMatProd<double>> eigs(op, k, ncv);
        eigs.init();
        int nconv = eigs.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigs.info() == CompInfo::Successful) {
            D = eigs.eigenvalues();
            V = eigs.eigenvectors();
        } 
        return static_cast<int>(eigs.info());
    } else {
        DenseGenRealShiftSolve<double> op(M);
        GenEigsRealShiftSolver<DenseGenRealShiftSolve<double>> eigs(op, k, ncv, sigma);
        eigs.init();
        int nconv = eigs.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigs.info() == CompInfo::Successful) {
            D = eigs.eigenvalues();
            V = eigs.eigenvectors();
        } 
        return static_cast<int>(eigs.info());
    }
}

extern "C" const int spectral_eigsh( 
    int k,
    void* d, void* v, 
    const void* p, int n,
    const int ncv, int maxit, double tol,
    const int mode, const double sigma,
    const int select)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    MapMatrix M((T1*)p, n, n);
    MapMatrix D((T1*)d, k, 1);
    MapMatrix V((T1*)v, n, k);

    if(mode == 0) {
        DenseGenMatProd<double> op(M);
        SymEigsSolver<DenseGenMatProd<double>> eigsh(op, k, ncv);
        eigsh.init();
        int nconv = eigsh.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigsh.info() == CompInfo::Successful) {
            D = eigsh.eigenvalues();
            V = eigsh.eigenvectors();
        }
        return static_cast<int>(eigsh.info());
    } else {
        DenseSymShiftSolve<double> op(M);
        SymEigsShiftSolver<DenseSymShiftSolve<double>> eigsh(op, k, ncv, sigma);
        eigsh.init();
        int nconv = eigsh.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigsh.info() == CompInfo::Successful) {
            D = eigsh.eigenvalues();
            V = eigsh.eigenvectors();
        } 
        return static_cast<int>(eigsh.info());
    }
}

extern "C" const int spectral_seigs( 
    int k,
    void* d, void* v,
    const void* values, const void* outerIndexPtr, const void* innerIndices, int n, int s,
    const int ncv, int maxit, double tol,
    const int mode, const double sigma,
    const int select)
{
    typedef Map< Matrix<T3,Dynamic,Dynamic> > MapComplexMatrix;
    typedef Map<const SparseMatrix<T1> > MapSparseMatrix;
    MapSparseMatrix M(n, n, s, (int*)outerIndexPtr, (int*)innerIndices, (T1*)values);
    MapComplexMatrix D((T3*)d, k, 1);
    MapComplexMatrix V((T3*)v, n, k);

    if(mode == 0) {
        SparseGenMatProd<double> op(M);
        GenEigsSolver<SparseGenMatProd<double>> eigs(op, k, ncv);
        eigs.init();
        int nconv = eigs.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigs.info() == CompInfo::Successful) {
            D = eigs.eigenvalues();
            V = eigs.eigenvectors();
        }
        return static_cast<int>(eigs.info());
    } else {
        SparseGenRealShiftSolve<double> op(M);
        GenEigsRealShiftSolver<SparseGenRealShiftSolve<double>> eigs(op, k, ncv, sigma);
        eigs.init();
        int nconv = eigs.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigs.info() == CompInfo::Successful) {
            D = eigs.eigenvalues();
            V = eigs.eigenvectors();
        }
        return static_cast<int>(eigs.info());
    }
}

extern "C" const int spectral_seigsh( 
    int k,
    void* d, void* v,
    const void* values, const void* outerIndexPtr, const void* innerIndices, int n, int s,
    const int ncv, int maxit, double tol,
    const int mode, const double sigma,
    const int select)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    typedef Map<const SparseMatrix<T1> > MapSparseMatrix;
    MapSparseMatrix M(n, n, s, (int*)outerIndexPtr, (int*)innerIndices, (T1*)values);
    MapMatrix D((T1*)d, k, 1);
    MapMatrix V((T1*)v, n, k);

    if(mode == 0) {
        SparseGenMatProd<double> op(M);
        SymEigsSolver<SparseGenMatProd<double>> eigsh(op, k, ncv);
        eigsh.init();
        int nconv = eigsh.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigsh.info() == CompInfo::Successful) {
            D = eigsh.eigenvalues();
            V = eigsh.eigenvectors();
        }
        return static_cast<int>(eigsh.info());
    } else {
        SparseSymShiftSolve<double> op(M);
        SymEigsShiftSolver<SparseSymShiftSolve<double>> eigsh(op, k, ncv, 0);
        eigsh.init();
        int nconv = eigsh.compute(static_cast<SortRule>(select), maxit, tol);
        if(eigsh.info() == CompInfo::Successful) {
            D = eigsh.eigenvalues();
            V = eigsh.eigenvectors();
        }
        return static_cast<int>(eigsh.info());
    }
}

// Generalized eigen solver for real symmetric matrices
extern "C" const int spectral_geigsh( 
    int k,
    void* d, void* v, 
    const void* a, int n,
    const void* values, const void* outerIndexPtr, const void* innerIndices, int s,
    const int ncv, int maxit, double tol,
    const int select)
{
    typedef Map< Matrix<T1,Dynamic,Dynamic> > MapMatrix;
    typedef Map<const SparseMatrix<T1> > MapSparseMatrix;
    MapMatrix A((T1*)a, n, n);
    MapSparseMatrix B(n, n, s, (int*)outerIndexPtr, (int*)innerIndices, (T1*)values);
    MapMatrix D((T1*)d, k, 1);
    MapMatrix V((T1*)v, n, k);

    DenseSymMatProd<double> op(A);
    SparseCholesky<double> Bop(B);

    SymGEigsSolver<DenseSymMatProd<double>, SparseCholesky<double>, GEigsMode::Cholesky> geigs(op, Bop, k, ncv);

    geigs.init();
    int nconv = geigs.compute(static_cast<SortRule>(select), maxit, tol);
    if(geigs.info() == CompInfo::Successful) {
        D = geigs.eigenvalues();
        V = geigs.eigenvectors();
    }
    return static_cast<int>(geigs.info());
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