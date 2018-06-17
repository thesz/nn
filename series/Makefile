all: pd
	./pd

pd: PenDigits.hs E.hs
	ghc --make -o pd -odir objs -hidir objs -O3 -fno-warn-tabs -main-is PenDigits.main PenDigits.hs

pdrp: PenDigitsRP.hs E.hs
	ghc --make -o pdrp -odir objs -hidir objs -O3 -fno-warn-tabs -main-is PenDigitsRP.main PenDigitsRP.hs

f: F.hs
	ghc --make -o f -odir objs -hidir objs -O3 -Wno-tabs -main-is F.main F.hs
