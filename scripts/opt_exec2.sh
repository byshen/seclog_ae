app=$1

BUILD_DIR="/home/tshan/Academic/llvm/llvm-9.0.0.src/build"

# "/home/byshen/clang_files/llvm-9.0.0.obj"
APP_DIR="/home/tshan/Academic/llvm/llvm-9.0.0.src/lib/Transforms/AceInstrument"
# "/home/byshen/clang_files/llvm-9.0.0.src/lib/Transforms/AceInstrument"

cd $APP_DIR
cp conf_files/$app.conf target.conf
# cp dir_checkfiles/$1.func check.func
# cp dir_exitfiles/$1.exfunc ex.func
# cp dir_libcallfiles/$1.libfunc libcall.func


#valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all \
    $BUILD_DIR/bin/opt -load  $BUILD_DIR/lib/AceInstrumentPass.so \
    -ace-instrument $APP_DIR/dir_bcfiles/$app.bc  2> $APP_DIR/$app.output


