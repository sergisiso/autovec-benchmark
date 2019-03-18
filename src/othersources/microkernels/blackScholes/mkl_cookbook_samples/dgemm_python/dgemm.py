#  Copyright(C) 2015 Intel Corporation. All Rights Reserved.
#
#  The source code, information  and  material ("Material") contained herein is
#  owned  by Intel Corporation or its suppliers or licensors, and title to such
#  Material remains  with Intel Corporation  or its suppliers or licensors. The
#  Material  contains proprietary information  of  Intel or  its  suppliers and
#  licensors. The  Material is protected by worldwide copyright laws and treaty
#  provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
#  modified, published, uploaded, posted, transmitted, distributed or disclosed
#  in any way  without Intel's  prior  express written  permission. No  license
#  under  any patent, copyright  or  other intellectual property rights  in the
#  Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
#  implication, inducement,  estoppel or  otherwise.  Any  license  under  such
#  intellectual  property  rights must  be express  and  approved  by  Intel in
#  writing.
#
#  *Third Party trademarks are the property of their respective owners.
#
#  Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
#  this  notice or  any other notice embedded  in Materials by Intel or Intel's
#  suppliers or licensors in any way.
#
#*******************************************************************************
#  Content:
#      dgemm example
#*******************************************************************************

import numpy as np
import scipy.linalg.blas as slb
import time

M = 10000
N = 6000
matrix_list = [64, 128, 256, 512, 1024, 2048, 4096, 8192]

np.show_config()

for K in matrix_list:
        a = np.array(np.random.random((M, N)), dtype=np.double, order='C', copy=False)
        b = np.array(np.random.random((N, K)), dtype=np.double, order='C', copy=False)
        A = np.matrix(a, dtype=np.double, copy=False)
        B = np.matrix(b, dtype=np.double, copy=False)

        start = time.time()
        C = slb.dgemm(1.0, a=A, b=B)
        end = time.time()

        tm = end - start
        print ('{0:4}, {1:9.7}'.format(K, tm))
