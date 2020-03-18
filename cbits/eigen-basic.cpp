#include "eigen-runtime.h"
#include <Eigen/Core>
#include <Eigen/Sparse>
#include <Eigen/LU>

using namespace Eigen;

template <class T>
Map< Matrix<T,Dynamic,Dynamic> > matrix(void* p, int r, int c) {
    return Map< Matrix<T,Dynamic,Dynamic> >((T*)p, r, c);
}

template <class T>
Map< Matrix<T,Dynamic,Dynamic> > matrix(const void* p, int r, int c) {
    return Map< Matrix<T,Dynamic,Dynamic> >((T*)p, r, c);
}

template <class T>
Map< SparseMatrix<T> > smatrix(void* val, void* outer,
    void* inner, int r, int c, int s) {
    return Map< SparseMatrix<T> >(r, c, s, (int*)outer, (int*)inner, (T*)val);
}

template <class T>
Map<SparseMatrix<T> > smatrix(const void* val, const void* outer,
    const void* inner, int r, int c, int s) {
    return Map< SparseMatrix<T> >(r, c, s, (int*)outer, (int*)inner, (T*)val);
}

// Matrix product
template <class T>
RET dd_mul( void* p, int r, int c,
    const void* p1, int r1, int c1,
    const void* p2, int r2, int c2)
{
    matrix<T>(p,r,c) = matrix<T>(p1,r1,c1) * matrix<T>(p2,r2,c2);
    return 0;
}
API(dd_mul, (int code,
    void* p, int r, int c,
    const void* p1, int r1, int c1,
    const void* p2, int r2, int c2), (p,r,c,p1,r1,c1,p2,r2,c2));

template <class T>
RET ds_mul( void* p, int r, int c,
    const void* p1, int r1, int c1,
    const void* val, const void* outer, const void* inner,
    int r2, int c2, int s)
{
    matrix<T>(p,r,c) = matrix<T>(p1,r1,c1) * smatrix<T>(val, outer, inner, r2, c2, s);
    return 0;
}
API(ds_mul, (int code,
    void* p, int r, int c,
    const void* p1, int r1, int c1,
    const void* val, const void* outer, const void* inner,
    int r2, int c2, int s), (p,r,c,p1,r1,c1,val,outer,inner,r2,c2,s));

template <class T>
RET sd_mul( void* p, int r, int c,
    const void* val, const void* outer, const void* inner,
    int r2, int c2, int s,
    const void* p1, int r1, int c1)
{
    matrix<T>(p,r,c) = smatrix<T>(val, outer, inner, r2, c2, s) * matrix<T>(p1,r1,c1);
    return 0;
}
API(sd_mul, (int code,
    void* p, int r, int c,
    const void* val, const void* outer, const void* inner, int r2, int c2, int s,
    const void* p1, int r1, int c1), (p,r,c,val,outer,inner,r2,c2,s,p1,r1,c1));

template <class T>
RET ss_mul( void** v, void** o, void** i, int r, int c, int* s,
    const void* v1, const void* o1, const void* i1, int r1, int c1, int s1,
    const void* v2, const void* o2, const void* i2, int r2, int c2, int s2)
{
    typedef Map<SparseMatrix<T> > MapSparseMatrix;
    MapSparseMatrix a(r1, c1, s1, (int*)o1, (int*)i1, (T*)v1);
    MapSparseMatrix b(r2, c2, s2, (int*)o2, (int*)i2, (T*)v2);
    SparseMatrix<T> M = (a * b).pruned();
    *s = M.nonZeros();
    T* p1 = (T*) malloc(*s * sizeof(T));
    memcpy(p1, M.valuePtr(), *s * sizeof(T));
    *v = p1;
    int* p2 = (int*) malloc(*s * sizeof(int));
    memcpy(p2, M.innerIndexPtr(), *s * sizeof(T));
    *i = p2;
    int* p3 = (int*) malloc(c * sizeof(int));
    memcpy(p3, M.outerIndexPtr(), c * sizeof(int));
    *o = p3;
    return 0;
}
API(ss_mul, (int code,
    void** v, void** o, void** i, int r, int c, int* s,
    const void* v1, const void* o1, const void* i1, int r1, int c1, int s1,
    const void* v2, const void* o2, const void* i2, int r2, int c2, int s2),
    (v,o,i,r,c,s,v1,o1,i1,r1,c1,s1,v2,o2,i2,r2,c2,s2));


#define UNOP(name) \
extern "C" RET __attribute__((noinline)) eigen_##name(int code, void* p, int r, int c, const void* p1, int r1, int c1) {\
        GUARD_START\
        switch (code) {\
            case 0: matrix<T0>(p,r,c) = matrix<T0>(p1,r1,c1).name(); break;\
            case 1: matrix<T1>(p,r,c) = matrix<T1>(p1,r1,c1).name(); break;\
            case 2: matrix<T2>(p,r,c) = matrix<T2>(p1,r1,c1).name(); break;\
            case 3: matrix<T3>(p,r,c) = matrix<T3>(p1,r1,c1).name(); break;\
        }\
        GUARD_END\
    }

UNOP(inverse);