{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import Numeric
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Reflex
import Reflex.Cocos2d.Prelude
import qualified Platformer as P

runningMan :: NodeGraph t m => V2 Double -> Event t NominalDiffTime -> FixEvent t m
runningMan winSize ts = FixEvent $ do
    let l = (0 .+^ winSize/2)
        fns = [ "res/walk/000"++show n++".png" | n <- [1..8]::[Int] ]
    (progress, finished) <- load fns
    let loading = do
          percent::Dynamic t Double <- foldDyn (const . uncurry (/)) 0 progress
          notice <- forDyn percent $ \p -> "Loading " ++ showFFloat (Just 2) (p*100) "%..."
          label_ $ def & pos .~ constDyn l
                       & fontSize .~ constDyn 25
                       & text .~ notice
    node def -| loading $ ffor finished . const $ do
        ts' <- slowdown 6 ts
        fnE <- zipListWithEvent const (cycle fns) ts'
        fnDyn <- holdDyn (head fns) fnE
        label_ $ def & pos .~ constDyn (l & _y //~ 4)
                     & text .~ fnDyn
        sprite_ $ def & pos .~ constDyn l
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
                l <- layerColor $ def & pos .~ constDyn (x ^& (winSize^._y/2))
                                      & size .~ constDyn (pure 100)
                                      & color .~ constDyn c
                currPos <- sample . current $ l^.pos
                liftIO $ print currPos
                forHMaybe (attachDyn (l^.size) (evts^.touchesBegan)) $ \(sz, touches) ->
                  let box = fromCorners 0 (0.+^sz) in
                  runMaybeT . msum . ffor touches $ \t -> do
                    loc' <- convertToNodeSpace l $ t^.loc
                    guard $ box `contains` loc'
                    return ()
            return $ leftmost [ const (P.platformer winSize dks ts) <$> lTouched
                              , const (runningMan winSize ts) <$> rTouched
                              ]
      return ()
