app=$1
sub=$2

BUILD_DIR="/home/ubuntu/llvm-9.0.0.obj"
APP_DIR="/home/ubuntu/llvm-9.0.0.src/lib/Transforms/AceInstrument"

cd $APP_DIR
if [ "$app" = "postfix" ]; then
    cp conf_files/"$app-$sub".conf target.conf
else
    cp conf_files/"$app".conf target.conf
fi
# cp dir_checkfiles/$1.func check.func
# cp dir_exitfiles/$1.exfunc ex.func
# cp dir_libcallfiles/$1.libfunc libcall.func


#valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all \
if [ "$app" = "postfix" ]; then
    $BUILD_DIR/bin/opt -load  $BUILD_DIR/lib/AceInstrumentPass.so \
        -ace-instrument $APP_DIR/dir_bcfiles/postfix/$sub.bc  2> $APP_DIR/output/"$app-$sub".output
else
    $BUILD_DIR/bin/opt -load  $BUILD_DIR/lib/AceInstrumentPass.so \
        -ace-instrument $APP_DIR/dir_bcfiles/$app.bc  2> $APP_DIR/output/$app.output
fi

