#!/bin/bash
######################################################################
# the following is a very basic test script for pasty. It runs
# pasty on a sample data set and checks the output against reference
# output
#
# (C) 2008 Markus Dittrich
######################################################################

mktmp="$(mktemp)"
pasty="../src/pasty"

# define command line options
parse="-r"
paste="-w"
insep="-i"
outsep="-o"

# small test function
test_result () {

  reffile="${1}"
  outfile="${2}"
  result="$(diff -q ${outfile} ${reffile})"

  if [ "${result}" != "" ]; then
    printf "  ** ${result} **"
    printf "\t[failed]\n"
    exit 1
  else
    printf "\t[ok]\n"
  fi
}

###############################################################
# test 1
printf "test 01 ...."
tmpfile="${mktmp}"
${pasty} test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.1.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 2
printf "test 02 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0:0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.2.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 3
printf "test 03 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.3.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 4
printf "test 04 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 1:1 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.4.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 5
printf "test 05 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 1:0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.5.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 6
printf "test 06 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0 test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.6.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 7
printf "test 07 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0:1:1 ${outsep} :: test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.7.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 8
printf "test 08 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0 ${outsep} " " test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.8.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 9
printf "test 09 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0,1 ${outsep} ++++ test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.9.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 10 
printf "test 10 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0,1 test_file1.dat test_file2.dat test_file1.dat ${outsep} "++++" > "${tmpfile}"
test_result "test.10.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 11 
printf "test 11 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 1 ${insep} : test_file3.dat > "${tmpfile}"
test_result "test.11.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 12 
printf "test 12 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0,1 ${insep} : test_file3.dat > "${tmpfile}"
test_result "test.12.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 13 
printf "test 13 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 1 ${insep} ":" test_file3.dat test_file4.dat > "${tmpfile}"
test_result "test.13.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 14 
printf "test 14 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 1 ${insep} ":" ${paste} 0,1 test_file3.dat test_file4.dat > "${tmpfile}"
test_result "test.14.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 15 
printf "test 15 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 1 ${insep} ":" ${paste} 0,1,5 test_file3.dat test_file4.dat > "${tmpfile}"
test_result "test.15.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 16 
printf "test 16 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0,1 ${paste} "1,0,1,0,1,1,5" test_file1.dat > "${tmpfile}"
test_result "test.16.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 17 
printf "test 17 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0,1 ${paste} "3,2,1,0" test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.17.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 18 
printf "test 18 ...."
tmpfile="${mktmp}"
${pasty} ${parse} 0,1,2,3 ${paste} 3,2,1,0 test_file5.dat > "${tmpfile}"
test_result "test.18.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 19 
printf "test 19 ...."
tmpfile="${mktmp}"
${pasty} test_file5.dat ${parse} 0,1,2,3 ${paste} 3,2,1,0 > "${tmpfile}"
test_result "test.19.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 20 -- make sure we can handle trailing empty lines 
# properly
printf "test 20 ...."
tmpfile="${mktmp}"
${pasty} test_file6.dat ${parse} 0,1,2,3 ${paste} 0,0,1,1 > "${tmpfile}"
test_result "test.20.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################


###############################################################
# test 21 -- make sure we can handle trailing empty lines 
# properly
printf "test 21 ...."
tmpfile="${mktmp}"
${pasty} test_file6.dat ${parse} 0,1,2,3 test_file5.dat ${paste} 0,0,1,1 > "${tmpfile}"
test_result "test.21.ref" "${tmpfile}" 
rm -f "${tmpfile}"
###############################################################



# at this point all tests passed
echo
echo "Congratulations - all tests passed!"
echo
