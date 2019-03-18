# ==============================================================================
# Copyright(C) 2014-2015 Intel Corporation. All Rights Reserved.
#
# The source code, information  and  material ("Material") contained herein is
# owned  by Intel Corporation or its suppliers or licensors, and title to such
# Material remains  with Intel Corporation  or its suppliers or licensors. The
# Material  contains proprietary information  of  Intel or  its  suppliers and
# licensors. The  Material is protected by worldwide copyright laws and treaty
# provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
# modified, published, uploaded, posted, transmitted, distributed or disclosed
# in any way  without Intel's  prior  express written  permission. No  license
# under  any patent, copyright  or  other intellectual property rights  in the
# Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
# implication, inducement,  estoppel or  otherwise.  Any  license  under  such
# intellectual  property  rights must  be express  and  approved  by  Intel in
# writing.
# 
# *Third Party trademarks are the property of their respective owners.
# 
# Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
# this  notice or  any other notice embedded  in Materials by Intel or Intel's
# suppliers or licensors in any way.
#
# ==============================================================================
# Makefile for GNU make

# ==============================================================================
#  Content:
#      Black-Scholes formula example makefile
# ==============================================================================
#
#    Parameters of this makefile:
#    ----------------------------
#
#      TARGET_ARCH= SSE, AVX <default>, AVX2, MIC
#
#      PREC= s (float) <default>, d (double)
#
#      ACC=ha, la, ep <default> : meaning math function accuracy level
# ==============================================================================

SRC:=                      \
       black-scholes.c     \
       black-scholes_mkl.c \
       data_gen.c          \
       main.c

# ==============================================================================
# ############## Configure CFLAGS  #############################################
# ==============================================================================
CC          := icc
QOPT        :=
FQOPT       :=f
EQCOLON     :="="

CFLAGS      += -O2
CFLAGS      += -$(QOPT)restrict
CFLAGS      += -openmp
CFLAGS      += -I./

PREC ?= s
ifeq ($(PREC),d)
else
    CFLAGS += -D__DO_FLOAT__
endif

TARGET_ARCH ?= AVX

ifeq ($(TARGET_ARCH),SSE)
    CFLAGS += -$(QOPT)xSSE4.2
endif
ifeq ($(TARGET_ARCH),AVX)
    CFLAGS += -$(QOPT)xAVX
endif
ifeq ($(TARGET_ARCH),AVX2)
    CFLAGS += -$(QOPT)xCORE_AVX2
endif
ifeq ($(TARGET_ARCH),MIC)
    CFLAGS += -mmic -opt-streaming-stores always
endif

ACC ?= ep
ifeq ($(ACC),ha)
    CFLAGS += -$(FQOPT)imf-precision$(EQCOLON)high -D_VML_ACCURACY_HA_
endif
ifeq ($(ACC),la)
    CFLAGS += -$(FQOPT)imf-precision$(EQCOLON)medium -D_VML_ACCURACY_LA_
endif
ifeq ($(ACC),ep)
    CFLAGS += -$(FQOPT)imf-precision$(EQCOLON)low -$(FQOPT)imf-domain-exclusion$(EQCOLON)31 -D_VML_ACCURACY_EP_
endif

# ==============================================================================
# ############## Define make rules #############################################
# ==============================================================================

all: black-scholes.out
	./black-scholes.out 1024

black-scholes.out: $(SRC)
	$(CC) $(CFLAGS) $(SRC) -mkl -oblack-scholes.out

clean:
	rm -rf *.o *.out *.optrpt
