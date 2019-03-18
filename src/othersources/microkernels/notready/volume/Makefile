
CC=icpc -std=c++11
CFLAGS=-Ofast -xHost -qopt-report=5
NOVEC=-no-simd -no-vec

all: rt-vec rt-novec ct-vec ct-novec

rt-vec:
	${CC} ${CFLAGS} -qopenmp -c -o volume_rtvec.o volume_serial.cpp
	${CC} ${CFLAGS} -o volume.rt-vec volume_rtvec.o volume.cpp

ct-vec:
	${CC} ${CFLAGS} -qopenmp -DSPECIALIZE -c -o volume_ctvec.o volume_serial.cpp
	${CC} ${CFLAGS} -o volume.ct-vec volume_ctvec.o volume.cpp

rt-novec:
	${CC} ${CFLAGS} ${NOVEC} -c -o volume_rtnovec.o volume_serial.cpp
	${CC} ${CFLAGS} ${NOVEC} -o volume.rt-novec volume_rtnovec.o volume.cpp

ct-novec:
	${CC} ${CFLAGS} ${NOVEC} -DSPECIALIZE -c -o volume_ctnovec.o volume_serial.cpp
	${CC} ${CFLAGS} ${NOVEC} -o volume.ct-novec volume_ctnovec.o volume.cpp

clean:
	rm -rf volume.rt-vec volume.ct-vec volume.rt-novec volume.ct-novec *.ppm *.o *.optrpt


