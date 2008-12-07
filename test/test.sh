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


# test 1
printf "test 01 ...."
tmpfile="${mktmp}"
${pasty} test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.1.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 2
printf "test 02 ...."
tmpfile="${mktmp}"
${pasty} -c 0:0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.2.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 3
printf "test 03 ...."
tmpfile="${mktmp}"
${pasty} -c 0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.3.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 4
printf "test 04 ...."
tmpfile="${mktmp}"
${pasty} -c 1:1 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.4.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 5
printf "test 05 ...."
tmpfile="${mktmp}"
${pasty} -c 1:0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.5.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 6
printf "test 06 ...."
tmpfile="${mktmp}"
${pasty} -c 0 test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.6.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 7
printf "test 07 ...."
tmpfile="${mktmp}"
${pasty} -c 0:1:1 -u :: test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.7.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 8
printf "test 08 ...."
tmpfile="${mktmp}"
${pasty} -c 0 -u " " test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.8.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 9
printf "test 09 ...."
tmpfile="${mktmp}"
${pasty} -c 0,1 -u ++++ test_file1.dat test_file2.dat test_file1.dat > "${tmpfile}"
test_result "test.9.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 10 
printf "test 10 ...."
tmpfile="${mktmp}"
${pasty} -c 0,1 test_file1.dat test_file2.dat test_file1.dat -u "++++" > "${tmpfile}"
test_result "test.9.ref" "${tmpfile}" 
rm -f "${tmpfile}"


# at this point all tests passed
echo
echo "Congratulations - all tests passed!"
echo
