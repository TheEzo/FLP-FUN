# Projekt: PLG-2-NKA
# Login:   xwilla00
# Autor:   Tomas Willaschek
# Rok:     2020
PROJ=plg-2-nka
SRC=src/Main.hs

default:
	ghc --make $(SRC) -o $(PROJ)

clean:
	rm -f $(PROJ)
