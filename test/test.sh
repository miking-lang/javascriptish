echo "Testing files"

declare -a files=("scope-working.jsh"
				  "scope-error.jsh"
				  "missing-call.jsh"
				  "too-many-parameters.jsh"
				  "catch-return.jsh")

for i in "${files[@]}"
do
	echo "Investigating file $i"
	eval "./jsh analyze $i"
done