
default: jaguar


#builds project
jaguar:
	stack build

#builds project and runs benchmarks
bench:
	stack bench

prof:
	stack build --profile
	stack exec --profile jaguar-exe -- -q input.txt +RTS -p
	#profiteur jaguar-exe.prof   #to generate html


#loads project into ghci using bench as the main package
bench-ghci:
	stack ghci --bench --main-is :jaguar-bench
	
	
#builds, runs benchmarks and generates a profiler report to file jaguar-bench.prof
bench-prof:
	stack bench --profile +RTS
