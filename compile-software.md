# Compile software bitcode from source code

You must have clang-9.0.0 and `wllvm` installed to start the following compilation

The bitcode of the ten applications used in our evaluation can be obtained from this Google Drive folder (link removed for double blind review).


- Table of contents
  * [Apache](#apache)
  * [Postgresql](#postgresql)
  * [Cherokee](#cherokee)
  * [NFS-ganesha](#nfs-ganesha)
  * [Proftpd](#proftpd)
  * [Postfix](#postfix)
  * [Haproxy](#haproxy)
  * [Vsftpd](#vsftpd)
  * [mSQL](#msql)
  * [redis](#redis)


## Apache

Before the installation, install `apr`, `pcre-8.38` and `libpcre3-dev` based on the instructions from [official manual](http://httpd.apache.org/docs/2.4/install.html).

```bash
# Obtain the source
git clone https://github.com/apache/httpd
cd httpd
# check out to the evaluated version.
git reset --hard 009bb53af525fb7f51c0ddabec984e800e9d267

# compile 
CC="wllvm" CXX="wllvm++" CFLAGS="-g -O0" \
    ./configure \
    --with-included-apr \
    --with-pcre=/usr/local/pcre/ \ --enable-mods-static=all
make -j8
```


References:
- Installation instruction http://httpd.apache.org/docs/2.4/install.html
- Statically link all the modules for analysis https://serverfault.com/questions/791116/difference-in-the-process-of-static-and-dynamic-compilation-of-modules-in-apache


## Postgresql

Obtain the source code of version 12.2 
```bash
CC="wllvm" CXX="wllvm++" CFLAGS="-g -O0" CXXFLAGS="-g -O0" ./configure --with-openssl 
make -j8
```


## Cherokee

```bash
git clone https://github.com/cherokee/webserver
git reset --hard 51f13b9535e652421c128ef541371854637ac32e

sudo apt-get install autoconf automake libtool libtool-bin

CC=wllvm CXX=wllvm++ CFLAGS="-g -O0" ./autogen.sh --prefix=/usr --sysconfdir=/etc --localstatedir=/var --enable-static-module=all --enable-static --enable-shared=no --with-mysql=no --with-ffmpeg=no --with-ldap=no --enable-beta --enable-trace --enable-backtraces --enable-maintainer-mode

make -j8
```


References:
- https://cherokee-project.com/doc/basics_installation_unix.html
- https://cherokee-project.com/doc/basics_download.html
- https://github.com/cherokee/webserver 
 

## NFS-ganesha

```bash
git clone https://github.com/nfs-ganesha/nfs-ganesha
git reset --hard 3122cdda857a92a7b24aeba0e1e8b8f7719224ef

sudo apt-get install  libkrb5-dev

git submodule update --init --recursive
CC=clang CXX=clang++ cmake -DUSE_GSS=OFF ../src
```

References:
- https://github.com/nfs-ganesha/nfs-ganesha/wiki/Compiling


## Proftpd

```bash
git clone https://github.com/proftpd/proftpd
git reset --hard 4ffe04158840130e023ed3d3e558b8d70e28e20e

CC=wllvm CXX=wllvm++ CFLAGS="-g -O0" ./configure --with-modules=mod_copy:mod_digest:mod_site_misc:mod_sql:mod_tls:mod_sftp

make -j8
```

## Postfix

```bash
git clone https://github.com/vdukhovni/postfix
git reset --hard 5671f91c4de26241a0cb9215ed91bb6bf9e230f6
sudo apt-get install libdb-dev

make makefiles CC=wllvm CXX=wllvm++ OPT="-O0" 
```
References:
- https://github.com/vdukhovni/postfix/blob/master/postfix/INSTALL

## Haproxy
```bash
# obtain the source
git clone https://github.com/haproxy/haproxy
git reset --hard ea875e62e6b2f69c50533c5cd52eb5284c69723f
```

change `CC=wllvm` in Makefile
also change
```
 CPU_CFLAGS            = -O0
 #CPU_CFLAGS            = $(CPU_CFLAGS.$(CPU))
```
Then use the following command 

```bash
make TARGET=linux-glibc -j6 USE_OPENSSL=1
```

## Vsftpd

```bash
git clone https://github.com/sverre/vsftpd-3.0.3
```
Change the Makefile with the following
```
CC = wllvm
CFLGS = -g -O0
```
Then `make -j8`.


## mSQL

Download mSQL 4.2 from here.
```
CC=wllvm CXX=wllvm++ CFLAGS="-g -O0" ./setup
```

## redis


```bash
git clone https://github.com/redis/redis
git reset --hard 022f09447be8bfd1137d5f85b679be61989a3d8f

CC=wllvm CXX=wllvm++ OPTIMIZATION="-O0" make 
```
References:
- https://github.com/vdukhovni/postfix/blob/master/postfix/INSTALL

