#!/bin/bash

software=$1
mode=$2


vsftpd_path="./software/vsftpd/vsftpd-3.0.3/"
vsftpd_bc="./software/vsftpd/vsftpd-3.0.3/vsftpd.bc"
httpd_bc="./software/httpd/httpd-2.4.46/httpd.bc"
httpd_path="./software/httpd/httpd-2.4.46/"

copy_to_software() {
    echo "[input] parameters are, " $@ 
    if [ -e  $1 ]; then 
	echo "[copying to ace]" $1
	cp $1 ./dir_bcfiles/
    else
	echo $1 "not found"
    fi
}

copy_to_build() {
    echo "[input] parameters are, " $@ 
    if [ -e  $1 ]; then 
	echo "[copying to compile]" $1
	cp $1 $2$1
    else
	echo $1 "not found"
    fi
}

if [ $software = "vsftpd" ]; then
    bc=$vsftpd_bc
    path=$vsftpd_path
fi

if [ $software = "httpd" ]; then
    bc=$httpd_bc
    path=$httpd_path
fi

if [ $mode = 1 ]; then 
    copy_to_software $bc
else
    copy_to_build $software"_log.bc" $path
fi

# copy_to_software $vsftpd_bc
# copy_to_software $httpd_bc
# copy_to_build vsftpd_log.bc $vsftpd_path