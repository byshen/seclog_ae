# We only tested with Ubuntu 18.04. On other platform, please make sure you have clang 9.0.0 installed!

# Step 0: install build essentials
sudo apt-get install build-essential zlib1g-dev python python-pip

# install clang 9.0.0
cd ~
curl -SL https://releases.llvm.org/9.0.0/clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz | tar -xJC .
mv clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04 clang_9.0.0
sudo mv clang_9.0.0 /usr/local
export PATH=/usr/local/clang_9.0.0/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/clang_9.0.0/lib:$LD_LIBRARY_PATH
# After this, you should have clang and clang++


# 
# Step 1: Install llvm
cd ~
wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz
tar -xvf llvm-9.0.0.src.tar.xz
mkdir llvm-9.0.0.obj
mkdir llvm-9.0.0.install

cd ~/llvm-9.0.0.obj
cmake  -DLLVM_TARGETS_TO_BUILD=X86 -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE="Debug" -DCMAKE_INSTALL_PREFIX=../llvm-9.0.0.install -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DBUILD_SHARED_LIBS=ON ../llvm-9.0.0.src

# Make could take 20 min to 1 hr depending on your machine :)
make -j6 # or use more cores that you have.
sudo make install 

# Step 2: Install wllvm
pip install wllvm

# Always execute this command to set up env variable for extracting bitcode
export LLVM_COMPILER=clang 
