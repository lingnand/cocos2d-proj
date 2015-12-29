This is a project template for developing a game using [ghcjs-cocos2d](https://github.com/lynnard/ghcjs-cocos2d) or [reflex-cocos2d](https://github.com/lynnard/reflex-cocos2d).

## Setup

```
$ cabal --ghcjs configure
```

## Build

```
$ cabal build
```

Each `build` will trigger an automatic deploy that copies the compiled js source to `./main.js` inside the project. This allows you to quickly preview the new game after build.

## Preview

Choose any static web server to serve the project directory on a given address (I'm using `http-server`). Then go to that address in your browser to play the game.

The same source has also been tested to work with [cocos2d-js](https://github.com/cocos2d/cocos2d-js)'s jsb on iOS and OSX.
