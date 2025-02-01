#!/bin/bash

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Directory containing the tests
TEST_DIR="tests"

# Iterate over all test input files
for test_input in $TEST_DIR/*.in; do
    test_number=$(basename "$test_input" .in)
    test_output="${TEST_DIR}/${test_number}.in.out"
    test_result="${TEST_DIR}/${test_number}.in.res"
    test_diff="${TEST_DIR}/${test_number}.diff"
    
    # Run the test
    ./hw1.out < "$test_input" >& "$test_result"
    
    # Compare the results
    if diff "$test_result" "$test_output" > /dev/null; then
        echo -e "Test ${test_number} ${GREEN}PASSED${NC}"
    else
        echo -e "Test ${test_number} ${RED}FAILED${NC}"
        diff "$test_result" "$test_output" > $test_diff
        
    fi
done
