.PHONY: clean runtests proftests covtests

GHC_OPTS=-O2 -funbox-strict-fields -i.. -outputdir build -fwarn-unused-imports

PROGS=smallints

smallints: smallints.hs
	ghc --make $(GHC_OPTS) smallints.hs

clean:
	rm -rf $(PROGS) *.o *.hi *~ build
