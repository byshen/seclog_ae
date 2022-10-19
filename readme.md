# AceInstrument (SecLog)


This repository includes source code of AceInstrument (SecLog), a static analysis tool to analyze and instrument the software's access-control logging.

- [Table of Concents](#aceinstrument--seclog-)
  * [0. Prerequisite](#0-prerequisite)
  * [1. Compile SecLog](#1-compile-seclog)
  * [2. Prepare software bitcode files](#2-prepare-software-bitcode-files)
    + [Detailed instructions](./compile-software.md)
  * [3. Run AceInstrument on program bitcodes](#3-run-aceinstrument-on-program-bitcodes)
  * [Misc info](#misc-info)


## 0. Prerequisite
SecLog requires two prerequisites.
- `LLVM 9.0.0` for compiling the source code; 
- [`wllvm`](https://github.com/travitch/whole-program-llvm) for extracting the software's LLVM bitcode. 

The following are the commands for installation on Ubuntu 18.04. (The docker image is coming soon.)

```bash
# Step 0: install build essentials
sudo apt-get install build-essential zlib1g-dev cmake

cd ~
curl -SL https://releases.llvm.org/9.0.0/clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz | tar -xJC .
mv clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04 clang_9.0.0
sudo mv clang_9.0.0 /usr/local
export PATH=/usr/local/clang_9.0.0/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/clang_9.0.0/lib:$LD_LIBRARY_PATH
# After this, you should have clang and clang++

# Step 1: Install llvm
cd ~
wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz
tar -xvf llvm-9.0.0.src.tar.xz
mkdir llvm-9.0.0.obj
mkdir llvm-9.0.0.install

cd llvm-9.0.0.obj
cmake  -DLLVM_TARGETS_TO_BUILD=X86 -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE="Debug" -DCMAKE_INSTALL_PREFIX=../llvm-9.0.0.install -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DBUILD_SHARED_LIBS=ON ../llvm-9.0.0.src

# Make could take 20 min to 1 hr depending on your machine :)
make -j6 # or use more cores that you have.
sudo make install 

# Step 2: Install wllvm
pip install wllvm

# Always execute this command to set up env variable for extracting bitcode
export LLVM_COMPILER=clang 
```

## 1. Compile SecLog

```bash 
# Download the source code
cd ~/llvm-9.0.0.src/lib/Transforms/
git clone https://github.com/byshen/seclog_ae AceInstrument # clone this repo

# modify the CMakeLists to include AceInstrument
echo "add_subdirectory(AceInstrument)" >> ~/llvm-9.0.0.src/lib/Transforms/CMakeLists.txt

# Compile the LLVM source again (around 5 minutes)
cd ~/llvm-9.0.0.obj
make -j6
```

## 2. Prepare software bitcode files

Compile the software source into LLVM bitcode files. AceInstrument will conduct analysis on the bitcode `.bc` files.

### For the compilation instructions, see this [detailed instructions here](./compile-software.md). 

The bitcode files used in our evaluation has been provided in `dir_bcfiles/`.

After you obtain the software binary `software_name`, simply execute the following command.


```bash 
export LLVM_COMPILER=clang 
extract-bc software_name
```
Then you will obtain a file named `software_name.bc`.

- For simple test programs (e.g., `dir_bcfiles/test_struct_param.c`), simply execute the following command to get the `.bc` file.

```bash
clang -emit-llvm -o test_struct_param.bc -c test_struct_param.c 
```


## 3. Run AceInstrument on program bitcodes

1. Copy the generated bitcode files to `dir_bcfiles/`.
2. Update `BUILD_DIR` and `APP_DIR` in `./scripts/opt_exec.sh`
```bash
./scripts/opt_exec.sh softwarename
```

3. The analysis results will be in `output/softwarename.output`. To process the outputs, run the following command

```bash
cd output
python3  output_parser.py -i softwarename
```
4. The final analysis result will `output/softwarename_summary.csv`.

5. To run all software, run the following script
```bash
./run_all.sh
```

> If you would like to automatically insert log messages inserted by SecLog, uncomment Line 27 in `util/EnhanceLog.h` to enable `INSERT_LOG` option. You need reassemble the instrumented bitcode to binary, in order to run the program.
## Misc info

Other useful common commands

```bash
# convert .bc to .ll files
llvm-dis program.bc # to .ll
llvm-as program.ll  # to .bc

# generate executable from .bc
llc -filetype=obj -O2 program.bc -o program.o
clang program.o -o program
./program
```

