#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np
import argparse

# Create the parser
my_parser = argparse.ArgumentParser(description='Parse output of analysis')

# Add the arguments
my_parser.add_argument('-i', '--input', action='store', required=True)


# Execute the parse_args() method
args = my_parser.parse_args()

# software_name = "vsftpd"
software_name = args.input

path = software_name + ".output"
try:
    f = open(path, "r")
except:
    print(path + " not found")
    raise("Error") 


def parser():
    line = f.readline()
    database= []
    a = 1
    flag = 0
    while line:
        if "[info] ===================" in line:
            line = f.readline()
            data = line.split()
            funcname = data[0]
            
            while "instrs" not in line:
                line = f.readline()
                if "Killed" in line:
                    flag = 1
                    break
                if "0 instrs" in line:
                    flag = 2
                    break
            if flag is 1:
                flag = 0
                continue
            if flag is 2:
                flag = 0
                database.append([funcname, inside, 0, 0, "no_branch"])
                continue
            # after [info] affected 1 branch instrs
            branchInstr = f.readline()
            branchInstr = branchInstr.strip()
            
            while "inside this function" not in line:
                line = f.readline()
                if "Killed" in line:
                    flag = 1
                    break
                if "affected 0 branch instrs" in line:
                    flag = 1
                    break
            if flag is 1:
                flag = 0
                continue
            data = line.split()
            inside = data[-1]
        
            while "successors" not in line:
                line = f.readline()
                if "Killed" in line:
                    flag = 1
                    break
                if "affected 0 branch instrs" in line:
                    flag = 1
                    break
            if flag is 1:
                flag = 0
                continue
            line = f.readline()
            while "error" in line:
                line = f.readline()
            data = line.split()
            branch1 = data[0]
            branch2 = data[1]
            database.append([funcname, inside, branch1, branch2, branchInstr])
        line = f.readline()




    dfe = pd.DataFrame(database, columns=['Function','Call_func','branch1', 'branch2', 'branch_instr'])

    output = software_name+".csv"
    dfe.to_csv(output)

def new_parser():
    line = f.readline()
    database= []
    a = 1
    flag = 0
    
    while line:
        if "[info] ===================" in line:
            line = f.readline()
            data = line.strip().split(" ")
            funcname = data[0]
            funcuseNO = data[2]
            line = f.readline()
            tmp = line.strip().split(" ")
            used_in_func = tmp[-1]

            affected_BR = 0
            BR_res = []
            BR_choose = -1
            # process affected BRs
            END_OF_THIS = False
            while END_OF_THIS != True:
                line = f.readline()
                if "THIS IS RARE, no affected branch instructions" in line:
                    END_OF_THIS = True
                if "[info] ================DONE" in line:
                    END_OF_THIS = True
                if "with affected BR" in line:
                    affected_BR += 1
                
                if "[info] Match choose" in line:
                    tmp = line.strip().split(" ")
                    BR_choose = tmp[-1]
                    BR_res.append([affected_BR, BR_choose])
                    BR_choose = -1

            if affected_BR == 0:
                database.append([funcname, funcuseNO, used_in_func, 0, -1])
            else:
                for BR_pair in BR_res:
                    database.append([funcname, funcuseNO, used_in_func, BR_pair[0], BR_pair[1]])
            if affected_BR > 1:
                for BR_pair in BR_res:
                    PRINT[funcname, funcuseNO, used_in_func, BR_pair[0], BR_pair[1]]
            print(funcname, funcuseNO, used_in_func)
        line = f.readline()

    dfe = pd.DataFrame(database, columns=['Function','Use #','In func', 'BR #', 'Choose Branch'])

    output = software_name+".csv"
    dfe.to_csv(output)


def summary_parser():
    line = f.readline()
    database= []

    while line:
        if "[summary]" in line:
            data = line.strip()[9:]
            data = data.split(',')
            database.append(data)
            print(len(data))
            print(data)
        line = f.readline()

    dfe = pd.DataFrame(database, columns=['Function','Use #','In func', 'Choose Branch', 'has log in AC', 'has log at call site', 'has log at upper call site','#AC params', '# local vars', '# call site args', 'logtemplate','real params'])

    output = software_name+"_summary.csv"
    dfe.to_csv(output)

if __name__ == '__main__':
    # new_parser()
    summary_parser()