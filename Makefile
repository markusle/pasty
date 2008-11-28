GHC_FLAGS_DEVEL = -O2 -fwarn-incomplete-patterns -fwarn-incomplete-record-updates -fwarn-missing-fields -fwarn-missing-methods -fwarn-missing-signatures -fwarn-name-shadowing -fwarn-orphans -fwarn-overlapping-patterns -fwarn-simple-patterns -fwarn-tabs -fwarn-type-defaults -fwarn-monomorphism-restriction -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches

OBJECTS = pasty.hs ByteStringHelper.hs

pasty: $(OBJECTS)
	ghc $(GHC_FLAGS_DEVEL) --make pasty.hs

.PHONY: clean

clean:
	rm -f *.o *.hi pasty
