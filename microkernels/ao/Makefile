# Copyright (c) 2019 Sergi Siso
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#   1. Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#   2. Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#   3. Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


include ../common.mk

APP=ao
ASMR=optreports/$(TEST_COMPILER)-$(VECTOR_ISA)_


all: rt-vec rt-novec ct-vec ct-novec
	@echo " ------ Vectorization Report -------"
	@echo -n "Hidden info - NoVec:  "; ../checksimd.sh ${ASMR}rtnovec.s | tail -n 1
	@echo -n "Hidden info - Vec:    ";../checksimd.sh ${ASMR}rtvec.s | tail -n 1
	@echo -n "Exposed info - NoVec: ";../checksimd.sh ${ASMR}ctnovec.s | tail -n 1
	@echo -n "Exposed info - Vec:   ";../checksimd.sh ${ASMR}ctvec.s | tail -n 1

rt-vec:
	${CC} ${CFLAGS} ${REPORT}rtvec.txt ${UNSAFE} -c -o ${APP}_rtvec.o ${APP}_serial.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} -c -o ${ASMR}rtvec.s ${APP}_serial.cpp
	${CC} ${CFLAGS} ${UNSAFE} -o ${APP}.rt-vec ${APP}_rtvec.o ${APP}.cpp

ct-vec:
	${CC} ${CFLAGS} ${REPORT}ctvec.txt ${UNSAFE} -DSPECIALIZE -c -o ${APP}_ctvec.o ${APP}_serial.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} -DSPECIALIZE -c -o ${ASMR}ctvec.s ${APP}_serial.cpp
	${CC} ${CFLAGS} ${UNSAFE} -o ${APP}.ct-vec ${APP}_ctvec.o ${APP}.cpp

rt-novec:
	${CC} ${CFLAGS} ${REPORT}rtnovec.txt ${UNSAFE} ${NOVEC} -c -o ${APP}_rtnovec.o ${APP}_serial.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} ${NOVEC} -c -o ${ASMR}rtnovec.s ${APP}_serial.cpp
	${CC} ${CFLAGS} ${UNSAFE} ${NOVEC} -o ${APP}.rt-novec ${APP}_rtnovec.o ${APP}.cpp

ct-novec:
	${CC} ${CFLAGS} ${REPORT}ctnovec.txt ${UNSAFE} ${NOVEC} -DSPECIALIZE -c -o ${APP}_ctnovec.o ${APP}_serial.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} ${NOVEC} -DSPECIALIZE -c -o ${ASMR}ctnovec.s ${APP}_serial.cpp
	${CC} ${CFLAGS} ${UNSAFE} ${NOVEC} -o ${APP}.ct-novec ${APP}_ctnovec.o ${APP}.cpp

clean:
	rm -rf ${APP}.rt-vec ${APP}.ct-vec ${APP}.rt-novec ${APP}.ct-novec *.ppm *.o *.optrpt


