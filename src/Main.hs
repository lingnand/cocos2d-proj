{-# LANGUAGE JavaScriptFFI #-}

import GHCJS.Types
import GHCJS.Foreign.Callback
import Data.Colour
import Data.Colour.Names
import JavaScript.Cocos2d.Layer
import JavaScript.Cocos2d.Types

foreign import javascript unsafe "cc.Scene.extend({onEnter:function() {this._super(); this.addChild($1);}})" sceneClass :: LayerColor -> IO JSVal
foreign import javascript unsafe "new $1()" newInstance :: JSVal -> IO JSVal
foreign import javascript unsafe "cc.game.onStart = function() {cc.LoaderScene.preload([], $1)}; cc.game.run();" runApp :: Callback a -> IO ()
foreign import javascript unsafe "cc.director.runScene($1);" runScene :: JSVal -> IO ()

main = do
    cb <- syncCallback ContinueAsync $ do
        putStrLn "*** Program started ***"
        createLayerColor (opaque red) 1000 1000 >>= sceneClass >>= newInstance >>= runScene
    runApp cb
