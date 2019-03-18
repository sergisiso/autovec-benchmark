
CC=icpc -std=c++11
CFLAGS=-Ofast -xHost -qopt-report=5
NOVEC=-no-simd -no-vec

all: rt-vec rt-novec ct-vec ct-novec

rt-vec:
	${CC} ${CFLAGS} -qopenmp -c -o noise_rtvec.o noise_serial.cpp
	${CC} ${CFLAGS} -o noise.rt-vec noise_rtvec.o noise.cpp

ct-vec:
	${CC} ${CFLAGS} -qopenmp -DSPECIALIZE -c -o noise_ctvec.o noise_serial.cpp
	${CC} ${CFLAGS} -o noise.ct-vec noise_ctvec.o noise.cpp

rt-novec:
	${CC} ${CFLAGS} ${NOVEC} -c -o noise_rtnovec.o noise_serial.cpp
	${CC} ${CFLAGS} ${NOVEC} -o noise.rt-novec noise_rtnovec.o noise.cpp

ct-novec:
	${CC} ${CFLAGS} ${NOVEC} -DSPECIALIZE -c -o noise_ctnovec.o noise_serial.cpp
	${CC} ${CFLAGS} ${NOVEC} -o noise.ct-novec noise_ctnovec.o noise.cpp

clean:
	rm -rf noise.rt-vec noise.ct-vec noise.rt-novec noise.ct-novec *.ppm *.o *.optrpt


