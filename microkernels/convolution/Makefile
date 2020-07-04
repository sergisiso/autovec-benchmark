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

# Don't change the order of the following 3 statements
APP := convolution
include ../common.mk

ifndef PREFIX
PREFIXCMD ?=
else
$(info Here)
PREFIXCMD := $(PREFIX) --
EXTRALDFLAGS = -I/home/sergi/workspace/phd/doping/bin /home/sergi/workspace/phd/doping/bin/libdoping.so -rdynamic -ldl
PREFSTRING = _doping
endif

NAME=$(APP)_$(VECSTRING)_$(SPECSTRING)$(PREFSTRING)

#CC := $(PREFIX) $(CC)

all: compile

compileall:
	make compile NOVEC=yes SPECIALIZE=yes
	make compile NOVEC=no SPECIALIZE=yes
	make compile NOVEC=yes SPECIALIZE=no
	make compile NOVEC=no SPECIALIZE=no

compile:
	$(PREFIXCMD) ${CC} ${CFLAGS} ${REPORT} -I. -c -o ${NAME}.o ${APP}_serial.cpp
	# ${CC} ${CFLAGS} -c ${ASM}$(NAME).s ${APP}_serial.cpp
	${CC} ${CFLAGS} -o ${NAME}.exe ${NAME}.o ${APP}.cpp $(EXTRALDFLAGS)
	

clean:
	rm -rf *.ppm *.o *.optrpt gnu_* *.exe *.doping.cpp


