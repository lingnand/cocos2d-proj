{-# LANGUAGE LambdaCase #-}
import Data.Colour
import Data.Colour.Names
import Control.Monad
import JavaScript.Cocos2d
import JavaScript.Cocos2d.Sys
import JavaScript.Cocos2d.EGLView
import JavaScript.Cocos2d.Director
import JavaScript.Cocos2d.Types
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Layer
import JavaScript.Cocos2d.Scene

main = do
    view <- getView
    getSys >>= getOS >>= \case
        IOS -> setEnableRetina view True
        x -> putStrLn $ "OS: " ++ show x
    setAdjustViewPort view True
    setDesignResolutionSize view 960 640 ShowAll
    setResizeWithBrowserSize view True
    join $ runScene <$> getDirector <*> do
        scene <- createScene
        setOnEnter scene $ addChild scene =<< createLayerColor blueviolet
        return scene
