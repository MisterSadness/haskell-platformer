A simple platformer written in Haskell using Yampa and SDL2

The repo contains a ready stack project, but SDL2 and library headers have to be [manually installed](https://github.com/haskell-game/sdl2#building).


### Some usefull comands that can be used in the project's main directory:
* To build the project
```
stack build
```
* To run the game
```
stack exec game
```
* To build documentation
```
stack exec -- haddock --html src/* --hyperlinked-source --odir=dist/docs
```