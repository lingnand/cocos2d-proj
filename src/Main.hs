{-# LANGUAGE LambdaCase #-}
import Linear
import qualified Data.Set as S
import Data.Default
import Data.Colour.Names
import Control.Monad.IO.Class
import Control.Lens
import JavaScript.Cocos2d
import JavaScript.Cocos2d.Types
import Reflex
import Reflex.Cocos2d

main = do
    getOS >>= \case
        IOS -> setEnableRetina True
        x -> putStrLn $ "OS: " ++ show x
    setAdjustViewPort True
    setDesignResolutionSize 960 640 ShowAll
    setResizeWithBrowserSize True
    winSize <- getWinSize
    runCocos2dHost $ do
        evts <- globalEvents
        dks <- dynKeysDown (select evts KeyPressed)
                           (select evts KeyReleased)
        sc <- scene $ do
            layerColor (def & color .~ constDyn blueviolet) $ do
                let unit ks = foldl (app ks) zero [ (ArrowUp, _y +~ 1)
                                                  , (ArrowDown, _y -~ 1)
                                                  , (ArrowLeft, _x -~ 1)
                                                  , (ArrowRight, _x +~ 1) ]
                    app ks v (k, op) | S.member k ks = op v
                                     | otherwise = v
                    start = winSize / 2
                dpos <- forDyn dks $ (+start) . (*100) . unit
                addVoidAction $ liftIO . print <$> updated dpos
                layerColor_ $ def & position .~ dpos
                                  & size .~ constDyn (pure 150)
                                  & color .~ constDyn orange
        liftIO $ runScene sc
