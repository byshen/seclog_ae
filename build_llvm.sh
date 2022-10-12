# Step 0: install build essentials
sudo apt-get install build-essential zlib1g-dev python python-pip

# Step 1: Install llvm
cd ~
wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz
tar -xvf llvm-9.0.0.src.tar.xz
mkdir llvm-9.0.0.obj
mkdir llvm-9.0.0.install

cd llvm-9.0.0.obj
cmake  -DLLVM_TARGETS_TO_BUILD=X86 \
    -DCMAKE_BUILD_TYPE="Release"  \ # (or use "Debug" for debug version)
    -DCMAKE_INSTALL_PREFIX=../llvm-9.0.0.install \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    ../llvm-9.0.0.src

# Make could take 20 min to 1 hr depending on your machine :)
make -j6 # or use more cores that you have.
sudo make install 

# Step 2: Install wllvm
pip install wllvm

# Always execute this command to set up env variable for extracting bitcode
export LLVM_COMPILER=clang 
