# FLP - funkcionální projekt 
__plg-2-nka__

__xwilla00__

---

## Implementace 
Zadání bylo splněno se všemi požadavky, program tedy převádí pravou lineární gramatiku z tvaru A->xB ('A' a 'B' jsou neterminály, 'x' je sekvence terminálů) do tvaru:
* A->aB ('a' je jeden terminál) nebo A-># ('#' značí epsilon);
* NKA - nedeterministický konečný automat;

Řešení dále obsahuje ošetření základních chybových stavů, například:
* Neterminál nenáleží seznamu [A..Z];
* Terminál nenáleží seznamu [a..z];
* Pravidla nejsou ve tvaru A->xB (chybějící šipka a formát pravidla);
* Pravidla obsahují neznámý (ne)terminál;

Dále je implementována funkce split, která vylepšuje načítání dat ze vstupu a dovoluje přítomnost redundantních čárek (',') v sekvenci terminálů a neterminálů.

## Překlad a spuštění
Překlad programu:
```make```

Spuštění programu:
```./plg-2-nka volby [vstup]```

Spuštění testů:
```make test```

## Testy
Archív obsahuje sadu základních testů, které testují, zda program při korektním vstupu vypíše korektní výstup, čtení vstupu ze souroru a ze stdin. Dále jsou zahrnuty testy s chybným vstupem, u kterých se očekává, že program skončí nenulovým exit kódem.
Tyto testy jsou celé řízeny skriptem ```test.sh```, který se nachází v rootu archívu.

Výstup testů je velmi podobný testům v pythonu, tedy '.' značí výsledek OK, 'F' značí error.
