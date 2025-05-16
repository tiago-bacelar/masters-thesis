
default: jaguar

CMD := ghci
SRC := Lang/Parser.hs $(wildcard *.hs Solver/*.hs Lang/*.hs)
BENCH_SRC := $(wildcard benchmarking/*.hs)
#-i$(echo aern2/*/src | sed -e 's/ / -i/g')
INCLUDE := -iimplementations -iimplementations/ireal -iimplementations/cdar/src -iimplementations/exact-real/src -iimplementations/haskell-fast-reals/src -XTypeFamilies
OUT_DIR := bin

Lang/Parser.hs: Lang/Parser.y
	#For legibility and info file:
	#happy --ghc -i -o Parser.hs Lang.y

	#For performance:
	happy --ghc -a -g -c -o $@ $<

bench: $(SRC) $(BENCH_SRC)
	$(CMD) benchmarking/Benchmark.hs $(INCLUDE) -o $@ -outputdir $(OUT_DIR)

solver: $(SRC)
	$(CMD) Solver/Solver.hs $(INCLUDE) -o $@ -outputdir $(OUT_DIR)

jaguar: $(SRC)
	$(CMD) Main.hs $(INCLUDE) -o $@ -outputdir $(OUT_DIR)

.PHONY: clean
clean:
	rm -f $(OUT_DIR)/*.hi $(OUT_DIR)/*.o