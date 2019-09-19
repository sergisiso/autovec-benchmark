#!/bin/bash

GCCVER="gcc-9.1.0"
GCCFILE="gcc-9.1.0.tar.gz"
GCCURL="ftp://ftp.mirrorservice.org/sites/sourceware.org/pub/gcc/releases/gcc-9.1.0/"

CLANGVER="clang-8.0.0"
CLANGFILE="cfe-8.0.0.src.tar.xz"
LLVMRTFILE="compiler-rt-8.0.0.src.tar.xz" # Needed for PGO and instrumentation functionalities
LLVMFILE="llvm-8.0.0.src.tar.xz"
LLVMURL="https://releases.llvm.org/8.0.0/"


ROOTDIR="$PWD/compilers"
DOWNLOADS="downloads"
BUILD="build"
INSTALLS="installs"

# If 'compilers directory does not exist create the directory structure
if [ ! -d $ROOTDIR ]; then
    mkdir $ROOTDIR
    mkdir $ROOTDIR/$DOWNLOADS
fi


# Downoad compilers
if [ ! -f $ROOTDIR/$DOWNLOADS/$GCCFILE ]; then
    cd $ROOTDIR/$DOWNLOADS && wget -O $GCCFILE ${GCCURL}${GCCFILE} && cd -
fi

if [ ! -f $ROOTDIR/$DOWNLOADS/$CLANGFILE ]; then
    cd $ROOTDIR/$DOWNLOADS && wget -O $LLVMFILE ${LLVMURL}${LLVMFILE} && cd -
    cd $ROOTDIR/$DOWNLOADS && wget -O $CLANGFILE ${LLVMURL}${CLANGFILE} && cd -
    cd $ROOTDIR/$DOWNLOADS && wget -O $LLVMRTFILE ${LLVMURL}${LLVMRTFILE} && cd -
fi

# Install compilers
# 1) Install GCC
if [ ! -d $ROOTDIR/$INSTALLS/$GCCVER ]; then
    echo "Installing $GCCVER ..."
    BUILDPATH=$ROOTDIR/$BUILD/$GCCVER
    rm -irf $BUILDPATH # Remove previous builds
    mkdir -p $BUILDPATH/src
    tar -xf $ROOTDIR/$DOWNLOADS/$GCCFILE -C $BUILDPATH/src
    cd $BUILDPATH/src/$GCCVER && ./contrib/download_prerequisites && cd -
    mkdir -p $BUILDPATH/objdir
    cd $BUILDPATH/objdir && ../src/$GCCVER/configure --prefix=$ROOTDIR/$INSTALLS/$GCCVER --disable-multilib --enable-languagues=c,c++ && make -j 8 && make install && cd -
    echo "$GCCVER Installed Successfully!"
fi

# 2) Install LLVM-clang
if [ ! -d $ROOTDIR/$INSTALLS/$CLANGVER ]; then
    echo "Installing $CLANGVER ..."
    BUILDPATH=$ROOTDIR/$BUILD/$CLANGVER
    rm -irf $BUILDPATH # Remove previous builds
    mkdir -p $BUILDPATH/src
    tar -xf $ROOTDIR/$DOWNLOADS/$LLVMFILE -C $BUILDPATH/src
    tar -xf $ROOTDIR/$DOWNLOADS/$CLANGFILE -C $BUILDPATH/src
    tar -xf $ROOTDIR/$DOWNLOADS/$LLVMRTFILE -C $BUILDPATH/src
    cd $BUILDPATH/src && mv "cfe-8.0.0.src" "clang" && mv "compiler-rt-8.0.0.src" "compiler-rt" && cd -
    mkdir -p $BUILDPATH/objdir
    cd $BUILDPATH/objdir && cmake -DLLVM_ENABLE_PROJECTS="clang;compiler-rt" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$ROOTDIR/$INSTALLS/$CLANGVER -G "Unix Makefiles" ../src/llvm-8.0.0.src  && make -j 8 && make install && cd -
fi
