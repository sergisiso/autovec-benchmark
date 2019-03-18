
include ../common.mk

APP=mandelbrot
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


