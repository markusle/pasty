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
    printf "\t\t\t[ok]\n"
  fi
}


# test 1
printf "test 1:"
tmpfile="${mktmp}"
${pasty} test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.1.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 2
printf "test 2:"
tmpfile="${mktmp}"
${pasty} -c 0:0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.2.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 3
printf "test 3:"
tmpfile="${mktmp}"
${pasty} -c 0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.3.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 4
printf "test 4:"
tmpfile="${mktmp}"
${pasty} -c 1:1 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.4.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# test 5
printf "test 5:"
tmpfile="${mktmp}"
${pasty} -c 1:0 test_file1.dat test_file2.dat > "${tmpfile}"
test_result "test.5.ref" "${tmpfile}" 
rm -f "${tmpfile}"

# at this point all tests passed
echo
echo "Congratulations - all tests passed!"
echo
