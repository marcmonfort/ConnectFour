# Quatre en ratlla 

Documentació de la practica de Haskell del joc Quatre en ratlla, fet per l'estudiant Marc Monfort Grau.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Compilador de Haskell, per exemple GHC.

```bash
> sudo apt install ghc
```

### Installing

A step by step series of examples that tell you how to get a development env running

Intal·lar el compilador de Haskell GHC:

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

En Mac:
```bash
> brew install cabal-install
> cabal update
> cabal install --lib random
```

End with an example of getting some data out of the system or using it for a little demo

## Compilació i execució

Compilem el fitxer joc.hs:

```bash
> ghc joc.hs
```

I l'executem:

```bash
> ./joc
```

### Break down into end to end tests

El joc s'executara per terminal.

Per començar un nou joc hem d'indicar el tamany del tauler. El cas habitual és de 6 files i 7 columnes:

```bash
	 ¡New Game!
	————————————
 Board size:
   Rows    = 6
   Columns = 7
```

Després haurem d'escollir l'estrategia de la IA:

```
 AI Strategies:
   1. random
   2. greedy
   3. smart
 Choose strategy: 3
```

La estrategia `random` i `greedy` executarà el joc directament. Si escollim la estrategia `smart` haurem d'escollir el nivell de dificultat.
La dificultat equival al nivell de profunditat de l'algorisme **minimax**. Es recomanable el nivell 4 o 5, per evitar un alt temps d'espera i obtenir una dificultat adecuada.
```bash
 AI Strategies:
   1. random
   2. greedy
   3. smart
 Choose strategy: 3
 Difficulty (recommended 4 or 5) = 5
```
A continuació ens sortira el tauler buit del tamany indicat. Començarem la partida seleccionant la columna on volem posar la primera fitxa.
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
Al seguent torn, la IA sera la encarregada de posar una nova fitxa. El temps en escollir la columna dependra de la estrategia i la dificultat seleccionada.
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
  │   │   │   │ ● │   │   │   │
  ╰───┴───┴───┴───┴───┴───┴───╯
  AI turn
  thinking... 

```

```
Give an example
```
```
Give an example
```
```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc

