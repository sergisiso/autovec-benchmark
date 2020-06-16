#include <vector>
#include <iostream>

#if SPECIALIZE
static const int mask[9] = {1,2,3,0,0,0,3,2,1};
constexpr int m = 3;
constexpr int n = 8192;
#else
int * mask;
int m;
int n;
#endif


#if SPECIALIZE
void BM_convolve(int * __restrict__ image, int * __restrict__ out, int p_n, int p_m, int * p_mask) {
#else
void BM_convolve(int * image, int * out, int p_n, int p_m, int * p_mask) {
  n = p_n; // Original had range 16 to 1024
  m = p_m;
  mask = p_mask;
#endif

  for(int i = 0; i < n - m; ++i)
    for(int j = 0; j < n - m; ++j)
      for(int k = 0; k < m; ++k)
        for(int l = 0; l < m; ++l)
          out[i * (n-m) + j] += image[(i+k) * n + j+l] * mask[k *m + l];
}

