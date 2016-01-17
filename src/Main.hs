{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import Linear
import Numeric
import Data.Range
import Data.Default
import Data.Colour.Names
import Data.Time.Clock
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Lens
import Reflex
import Reflex.Cocos2d
import JavaScript.Cocos2d
import qualified Platformer as P

runningMan :: NodeGraph t m => V2 Double -> Event t NominalDiffTime -> FixEvent t m
runningMan winSize ts = FixEvent $ do
    let fns = [ "res/walk/000"++show n++".png" | n <- [1..8]::[Int] ]
    (progress, finished) <- load fns
    let loading = do
          percent::Dynamic t Double <- foldDyn (const . uncurry (/)) 0 progress
          notice <- forDyn percent $ \p -> "Loading " ++ showFFloat (Just 2) (p*100) "%..."
          label_ $ def & position .~ constDyn (winSize/2)
                       & fontSize .~ constDyn 25
                       & text .~ notice
    node def -| loading $ ffor finished . const $ do
        ts' <- slowdown 6 ts
        fnE <- zipListWithEvent const (cycle fns) ts'
        fnDyn <- holdDyn (head fns) fnE
        label_ $ def & position .~ constDyn (winSize & _x //~ 2
                                                     & _y //~ 8)
                     & text .~ fnDyn
        sprite_ $ def & position .~ constDyn (winSize/2)
                      & color .~ constDyn white
                      & spriteName .~ fnDyn
    return never

main = do
    os <- getOS
    putStrLn $ "OS: " ++ show os
    case os of
      Just IOS -> setEnableRetina True
      _ -> return ()
    setAdjustViewPort True
    setDesignResolutionSize 960 640 ShowAll
    setResizeWithBrowserSize True
    winSize <- getWinSize
    mainScene $ do
      evts <- uiEvents
      dks <- dynKeysDown (evts^.keyPressed) (evts^.keyReleased)
      ts <- ticks
      rec let newChild = unfixEvent <$> switchPromptlyDyn dyns
          (_, dyns)  <- newChild & layerColor (def & color .~ constDyn blueviolet) -| do
            [lTouched, rTouched] <- forM [(200, yellow), (400, red)] $ \(x, c) -> do
                l <- layerColor $ def & position .~ constDyn (V2 x (winSize^._y/2))
                                      & size .~ constDyn (pure 100)
                                      & color .~ constDyn c
                forHMaybe (attachDyn (l^.size) (evts^.touchesBegan)) $ \(sz, locs) ->
                  runMaybeT . msum . ffor locs $ \t -> do
                    loc' <- convertToNodeSpace l $ t^.location
                    guard $ inRange (zero, sz) loc'
                    return ()
            return $ leftmost [ const (P.platformer winSize dks ts) <$> lTouched
                              , const (runningMan winSize ts) <$> rTouched
                              ]
      return ()
