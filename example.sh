#!/bin/sh

set -u

bname=hs-dosyear-leapchk
bin=$(cabal exec -- which "${bname}")

run_case() {
	echo "---"
	printf "> %s\n" "$1"
	log=$(echo "$1" | "$bin" 2>&1)
	ret=$?
	if [ $ret -eq 0 ]; then
		echo "exit: ok"
	else
		echo "exit: ng"
	fi
	echo "$log"
}

main() {
	if [ $# -eq 0 ]; then
		run_case 1980 # leap
		run_case 1981 # non-leap
		run_case 2100 # non-leap, special
		run_case 2107 # non-leap
		run_case 1979 # out of range
		run_case 2108 # out of range
		run_case 1985.823 # invalid
		run_case 2000 # leap
		run_case '2000 year' # invalid
		run_case 'year 2000' # invalid
	else
		run_case "$@"
	fi
}

main "$@"
