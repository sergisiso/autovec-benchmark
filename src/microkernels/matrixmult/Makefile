
include ../common.mk

APP=matrixmult
ASMR=optreports/$(TEST_COMPILER)-$(VECTOR_ISA)_


all: rt-vec rt-novec ct-vec ct-novec
	@echo " ------ Vectorization Report -------"
	@echo -n "Hidden info - NoVec:  "; ../checksimd.sh ${ASMR}rtnovec.s | tail -n 1
	@echo -n "Hidden info - Vec:    ";../checksimd.sh ${ASMR}rtvec.s | tail -n 1
	@echo -n "Exposed info - NoVec: ";../checksimd.sh ${ASMR}ctnovec.s | tail -n 1
	@echo -n "Exposed info - Vec:   ";../checksimd.sh ${ASMR}ctvec.s | tail -n 1

rt-vec:
	${CC} ${CFLAGS} ${REPORT}rtvec.txt ${UNSAFE} -o ${APP}.rt-vec ${APP}.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} -o ${ASMR}rtvec.s ${APP}.cpp

ct-vec:
	${CC} ${CFLAGS} ${REPORT}ctvec.txt ${UNSAFE} -DSPECIALIZE  -o ${APP}.ct-vec ${APP}.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} -DSPECIALIZE  -o ${ASMR}ctvec.s ${APP}.cpp

rt-novec:
	${CC} ${CFLAGS} ${REPORT}rtnovec.txt ${UNSAFE} ${NOVEC} -o ${APP}.rt-novec ${APP}.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} ${NOVEC} -o ${ASMR}rtnovec.s ${APP}.cpp

ct-novec:
	${CC} ${CFLAGS} ${REPORT}ctnovec.txt ${UNSAFE} ${NOVEC} -DSPECIALIZE -o ${APP}.ct-novec ${APP}.cpp
	${CC} ${CFLAGS} ${ASM} ${UNSAFE} ${NOVEC} -DSPECIALIZE -o ${ASMR}ctnovec.s ${APP}.cpp

clean:
	rm -rf ${APP}.rt-vec ${APP}.ct-vec ${APP}.rt-novec ${APP}.ct-novec *.ppm *.o *.optrpt


