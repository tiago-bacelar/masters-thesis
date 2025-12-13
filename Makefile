
default: jaguar


#builds project
jaguar:
	stack build

#builds project and runs benchmarks
bench:
	stack bench


#loads project into ghci using bench as the main package
bench-ghci:
	stack ghci --bench --main-is :jaguar-bench
