
default: jaguar

CMD := ghci
SRC := Lang/Parser.hs $(wildcard *.hs Solver/*.hs Lang/*.hs)
INCLUDE := -iimplementations -iimplementations/ireal -iimplementations/cdar/src -iimplementations/exact-real/src -iimplementations/haskell-fast-reals/src -XTypeFamilies
#-i$(echo aern2/*/src | sed -e 's/ / -i/g')
BENCH_SRC := $(wildcard benchmarking/*.hs)
BENCH_INCLUDE := -ibenchmarking

OUT_DIR := bin
#OPTS := -prof -fprof-auto
OPTS := 

Lang/Parser.hs: Lang/Parser.y
#For legibility and info file:
#happy --ghc -i -o Parser.hs Lang.y

#For performance:
	happy --ghc -a -g -c -o $@ $<

bench: $(SRC) $(BENCH_SRC)
	$(CMD) -O benchmarking/Benchmark.hs $(INCLUDE) $(BENCH_INCLUDE) -o $@ -outputdir $(OUT_DIR) $(OPTS)

solver: $(SRC)
	$(CMD) Solver/Solver.hs $(INCLUDE) -o $@ -outputdir $(OUT_DIR) $(OPTS)

jaguar: $(SRC)
	$(CMD) Main.hs $(INCLUDE) -o $@ -outputdir $(OUT_DIR) $(OPTS)

.PHONY: clean
clean:
	rm -rf $(OUT_DIR)/*