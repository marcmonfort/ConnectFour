# Quatre en ratlla 

Documentació de la pràctica de Haskell del joc Quatre en ratlla, fet per l'estudiant Marc Monfort Grau.

## Instruccions

Instruccions per tenir a punt el codi del projecte.

### Prerequisits

Fitxer joc.hs, compilador de Haskell GHC, i paquet per utilitzar System.Random.

### Instal·lació

Instal·lar el compilador de Haskell GHC:

```bash
> sudo apt install ghc
```

Instal·lar el paquet `random` de Haskell:

En Ubuntu:
```bash
> sudo apt install cabal-install
> cabal update
> cabal install random
```

## Compilació i execució

Compilem el fitxer joc.hs:

```bash
> ghc joc.hs
```

I l'executem:

```bash
> ./joc
```

### Nova Partida

El joc s'executarà per terminal.

Per començar un nou joc hem d'indicar la mida del tauler. El cas habitual és de 6 files i 7 columnes:

```bash
	 ¡New Game!
	————————————
 Board size:
   Rows    = 6
   Columns = 7
```

Després haurem d'escollir l'estratègia de la IA:

```bash
 AI Strategies:
   1. random
   2. greedy
   3. smart
 Choose strategy: 3
```

L'estratègia `random` i `greedy` executarà el joc directament. Si escollim l'estratègia `smart` haurem d'escollir el nivell de dificultat.
La dificultat equival al nivell de profunditat de l'algorisme **minimax**. És recomanable el nivell 4 o 5, per evitar un alt temps d'espera i obtenir una dificultat adequada.

```bash
 AI Strategies:
   1. random
   2. greedy
   3. smart
 Choose strategy: 3
 Difficulty (recommended 4 or 5) = 5
```
A continuació ens sortirà el tauler buit amb la mida indicada. Començarem la partida seleccionant la columna on volem posar la primera fitxa.
```bash
    1   2   3   4   5   6   7   
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ╰───┴───┴───┴───┴───┴───┴───╯
  Your turn
  Choose column: 4
```
Al següent torn, la IA serà l'encarregada de posar una nova fitxa. El temps en escollir la columna dependrà de l'estratègia i la dificultat seleccionada.
```
    1   2   3   4   5   6   7   
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │ ○ │   │   │   │
  ╰───┴───┴───┴───┴───┴───┴───╯
  AI turn
  thinking... 
```
S'indicarà la columna escollida amb el missatge:

```
  Column -> 2
```
Tornarem al nostre torn, on haurem d'escollir una nova columna.
```
    1   2   3   4   5   6   7   
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │   │   │   │   │   │   │
  ├───┼───┼───┼───┼───┼───┼───┤
  │   │ ● │   │ ○ │   │   │   │
  ╰───┴───┴───┴───┴───┴───┴───╯
  Your turn
  Choose column: 
```

### Fi de la partida

La partida finalitzarà quan un dels jugadors tingui quatre fitxes en ratlla, o no quedi cap més forat al tauler.

En cas de victòria:
```
    1   2   3   4   
  │   │ ○ │   │   │
  ├───┼───┼───┼───┤
  │   │ ○ │   │   │
  ├───┼───┼───┼───┤
  │ ● │ ○ │   │   │
  ├───┼───┼───┼───┤
  │ ● │ ○ │ ● │   │
  ╰───┴───┴───┴───╯

  Winner!!! Good Job
```

En cas de derrota:
```
    1   2   3   4   
  │   │   │   │ ● │
  ├───┼───┼───┼───┤
  │ ○ │   │ ● │ ● │
  ├───┼───┼───┼───┤
  │ ○ │ ● │ ○ │ ● │
  ├───┼───┼───┼───┤
  │ ● │ ○ │ ○ │ ○ │
  ╰───┴───┴───┴───╯

  Game OVER! - You've lost!
```

En cas d'empat:
```
    1   2   3   4   
  │ ● │ ○ │ ● │ ○ │
  ├───┼───┼───┼───┤
  │ ● │ ○ │ ○ │ ● │
  ├───┼───┼───┼───┤
  │ ○ │ ● │ ● │ ● │
  ├───┼───┼───┼───┤
  │ ○ │ ○ │ ○ │ ● │
  ╰───┴───┴───┴───╯
  It's a TIE
```
## Estratègies

### Random

En l'estratègia `random` la IA (ordinador) escollirà una columna a l'atzar. Si aquesta columna està plena, escollirà de nou una altra columna a l'atzar fins a poder posar una nova fitxa.
No es pot donar el cas on l'ordinador es quedi travat per no haver-hi cap forat, perquè aquesta situació la comprovem abans. 

### Greedy

Amb l'estratègia `greedy` la IA posarà una fitxa a la columna on pugui concatenar el nombre més alt de fitxes pròpies, o la columna que evita que el contrari faci 4-en-ratlla a la jugada següent (en el cas que la IA no pugui guanyar en la jugada actual).

Si hi ha més d'una columna amb el mateix nombre màxim de fitxes concatenades, escollirem la primera que trobem.

### Smart

#### Minimax
L'estratègia `smart` fa servir l'algorisme **minimax** per crear un arbre de possibles taulers. A partir del tauler actual generem tots els possibles taulers, posant un fitxa del nostre color a cada columna. Després, generem de nou tots els possibles taulers a partir dels generats anteriorment, amb una fitxa del color contrari, i així successivament fins a arribar a un tauler final on un dels jugadors ja ha guanyat, o fins a arribar al nivell màxim de profunditat que hem assignat al començament en concepte de dificultat.

En aquest punt avaluem el tauler amb una heurística que comentarem a continuació. El resultat d'aquesta heurística l'hem de propagar des dels taulers "fulla", fins a l'arrel de l'arbre. En cada nivell, el pare agafa el resultat del tauler fill que tingui la màxima o mínima puntuació, segons si en aquell nivell li tocava jugar a la IA o al contrincant. En finalitzar l'algorisme, obtindrem la columna amb el millor resultat segons la profunditat de generació indicada, i també, considerant que el contrincant en tot moment posarà la seva fitxa a la millor posició possible per poder guanyar.

#### Heurística
La part important d'aquesta estratègia és implementar una heurística que avaluï el tauler. És fàcil avaluar un tauler final, on un dels jugadors ha guanyat. Però en el joc en qüestió, tot i que seria possible (i ens donaria una jugada perfecta) generar totes les possibles combinacions i després avaluar sobre taulers finals, és computacionalment molt costós, i no és adequat en el nostre cas. Per això ens fa falta la funció heurística que avaluï taulers no finals.

Després de provar diferents combinacions, l'heurística amb millor resultat consisteix a comptar per totes les combinacions de quatre caselles juntes el nombre de fitxes de la IA que poden aconseguir fer un quatre-en-ratlla, i restar les del contrincant.

Aleshores, en una combinació de quatre caselles on **només hi ha fitxes d'un mateix color**:

* Sumem 3 si la IA té 3 fitxes.
* Sumem 2 si la IA té 2 fitxes.
* Sumem 1 si la IA té 1 fitxa.

(i fem el mateix amb les fitxes del contrincant, però restant)

En el cas que hi hagi 4 fitxes (4-en-ratlla), el tauler és un tauler final i hem d'assignar una puntuació d'infinit en cas que guanyi la IA o de -infinit si guanya el contrincant. En el codi considerem 1000 com valor infinit.

En l'heurística també considerem la situació especial de victòria amb 3 fitxes. Aquesta situació és quan hi han 3 fitxes d'un mateix color concatenades, i la possibilitat de posar una fitxa en ambdós costats. En aquesta situació el contrincant no té possibilitat de bloquejar els dos costats, i assegurem una victòria en dos torns (sense tenir en compte una possible victòria del contrincant al torn següent.)

```
    1   2   3   4   5   
  │   │   │   │   │   │
  ├───┼───┼───┼───┼───┤
  │   │   │ ○ │ ○ │   │
  ├───┼───┼───┼───┼───┤
  │   │ ● │ ● │ ● │   │
  ╰───┴───┴───┴───┴───╯
```
En aquesta situació augmentem el valor del tauler en 500. El que ve a ser el mateix que infinit, però d'un grau inferior, ja que és preferible una victòria en el torn actual, que una victòria en dos torns.

## Autor

* **Marc Monfort Grau**

## Llicència

No hi ha cap llicència.


