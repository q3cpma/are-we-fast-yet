#!/bin/sh
set -eu
cd -- "$(dirname -- "$(readlink -f -- "$0")")"

tolower() { echo "$1" | tr '[:upper:]' '[:lower:]'; }

impls=$(
	for impl in ccl sbcl clasp ecl_bytecode ecl_native clisp_bytecode lua luajit python3 node # clisp_interp
	do
		command -v "${impl%_*}" >/dev/null && echo "$impl"
	done | paste -sd' '
)
trap 'exit 1' HUP INT QUIT ABRT ALRM TERM
trap 'rm -f -- *.fas' EXIT

ccl_ver=$(ccl --version | cut -d' ' -f2)
sbcl_ver=$(sbcl --version | cut -d' ' -f2)
clasp_ver=$(clasp --version | cut -d'-' -f 2-)
ecl_ver=$(ecl --version | cut -d' ' -f2)
clisp_ver=$(clisp --version | awk '{print $3; exit}')
lua_ver=$(lua -v 2>&1 | cut -d' ' -f2)
luajit_ver=$(luajit -v | cut -d' ' -f2)
python3_ver=$(python3 --version | cut -d' ' -f2)
node_ver=$(node --version | sed 's#^v##')

ccl() { command ccl -b -l harness.lisp --eval '(ccl:quit)' -- "$@"; }
sbcl() { command sbcl --script harness.lisp "$@"; }
clasp() { command clasp --script harness.lisp -- "$@"; }
ecl_bytecode() { ecl --shell harness.lisp -- "$@"; }
ecl_native()
{
	local file=$(tolower "$1").lisp
	ecl --compile "$file" &&
		{
			ecl --shell harness.lisp -- "$@"
			rm -- "${file%.lisp}.fas"  # Otherwise, Clisp tries to load them
		}
}
clisp_interp() { clisp harness.lisp -- "$@"; }
clisp_bytecode() { clisp -C harness.lisp -- "$@"; }
lua() (cd ../Lua; command lua harness.lua "$@")
luajit() (cd ../Lua; command luajit harness.lua "$@")
python3() (cd ../Python; command python3 harness.py "$@")
node() (cd ../JavaScript/; command node harness.js "$@")

red=$(tput setaf 1) || true
bold=$(tput bold) || true
sgr0=$(tput sgr0) || true
time()
{
	local out=
	out=$("$@" 2>&1)
	if [ $? -ne 0 ]
	then
		echo "FAIL"
		{
			echo "$@: $bold${red}FAIL$sgr0"
			echo "$out"
		} >&2
	else
		local div=$4
		case "$bench" in Mandelbrot*|NBody*) div=1; esac
		echo "$out" | sed -En "s#.*average: ([0-9.]+)us.*#\1 / $div#p" | bc
	fi
}


num_iter=$1
inner_iter=$2
benchmarks=$(
	printf '%s\n' *.lisp | \
		awk -F/ '{print $NF}' | \
		sort | \
		uniq | \
		grep -Evx '(benchmark|harness|som|template|uiop-minimal)\.lisp' | \
		awk '
		{
			sub("\.lisp$", "")
			if (sub("^nbody", "NBody"))
				print
			else
				print toupper(substr($0, 1, 1)) substr($0, 2)
		}'
)

first=true
for bench in 'IMPL \ BENCH' $benchmarks
do
	$first && first=false || printf '\t'
	printf '%s' "$bench"
done
echo

for impl in $impls
do
	cmd=${impl%_*}
	! command -v "$cmd" >/dev/null && continue
	[ "$cmd" != "$impl" ] && impl_type=${impl#*_}
	printf '%s' "$cmd${impl_type+ $impl_type} $(eval echo \$${cmd}_ver)"
	for bench in $benchmarks
	do
		_inner_iter=$inner_iter
		[ "$inner_iter" != 1 ] && case "$bench" in
			Mandelbrot*) _inner_iter=750;;
			NBody*)      _inner_iter=250000;;
		esac
		# echo $impl $bench "$@" >&2
		printf '\t%s' "$(time "$impl" "$bench" "$num_iter" "$_inner_iter")"
	done
	echo
	unset impl_type
done
