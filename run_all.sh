#!/bin/sh

run_one_app() { 
    start_time=`date +%s`
    if [ $# -ge 2 ] # number of parameters
    then # $2 was given
        ./scripts/opt_exec.sh $1 $2
    else
        ./scripts/opt_exec.sh $1
    fi
    end_time=`date +%s`
    echo execution time was `expr $end_time - $start_time` s.
}


run_one_app httpd
run_one_app postgres
run_one_app proftpd
run_one_app redis-server
run_one_app haproxy
run_one_app msqld
run_one_app vsftpd
run_one_app nfsd
run_one_app cherokee-admin
run_one_app postfix smtpd
