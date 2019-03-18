
CC=icpc -std=c++11
CFLAGS=-Ofast -xHost -qopt-report=5
NOVEC=-no-simd -no-vec

all: rt-vec rt-novec ct-vec ct-novec

rt-vec:
	${CC} ${CFLAGS} -qopenmp -c -o fast-walsh_rtvec.o fast-walsh_serial.cpp
	${CC} ${CFLAGS} -o fast-walsh.rt-vec fast-walsh_rtvec.o fast-walsh.cpp

ct-vec:
	${CC} ${CFLAGS} -qopenmp -DSPECIALIZE -c -o fast-walsh_ctvec.o fast-walsh_serial.cpp
	${CC} ${CFLAGS} -o fast-walsh.ct-vec fast-walsh_ctvec.o fast-walsh.cpp

rt-novec:
	${CC} ${CFLAGS} ${NOVEC} -c -o fast-walsh_rtnovec.o fast-walsh_serial.cpp
	${CC} ${CFLAGS} ${NOVEC} -o fast-walsh.rt-novec fast-walsh_rtnovec.o fast-walsh.cpp

ct-novec:
	${CC} ${CFLAGS} ${NOVEC} -DSPECIALIZE -c -o fast-walsh_ctnovec.o fast-walsh_serial.cpp
	${CC} ${CFLAGS} ${NOVEC} -o fast-walsh.ct-novec fast-walsh_ctnovec.o fast-walsh.cpp

clean:
	rm -rf fast-walsh.rt-vec fast-walsh.ct-vec fast-walsh.rt-novec fast-walsh.ct-novec *.ppm *.o *.optrpt


