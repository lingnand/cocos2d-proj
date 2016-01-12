{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import Linear
import Data.Fix
import Data.Default
import Data.Colour.Names
import Control.Monad
import Control.Compose
import Control.Lens
import Reflex
import Reflex.Cocos2d
import JavaScript.Cocos2d
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Types
import qualified Platformer as P

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
      rec let newChild = unO . unFix <$> switchPromptlyDyn dyns
          (_, dyns)  <- newChild & layerColor (def & color .~ constDyn blueviolet) |- do
            [lTouched, rTouched] <- forM [(200, yellow), (400, red)] $ \(x, c) -> do
                l <- layerColor $ def & position .~ constDyn (V2 x (winSize^._y/2))
                                      & size .~ constDyn (pure 100)
                                      & color .~ constDyn c
                forHMaybe (attachDyn (l^.size) (evts^.touchesBegan)) $ \((V2 w h), locs) -> do
                  results <- filterM ?? locs $ convertToNodeSpace l . (^.location)
                                             >=> \(V2 x y) -> return (x <= w && y <= h)
                  return $ guard (not $ null results) >> return ()
            return $ leftmost [ const (P.platformer winSize dks ts) <$> lTouched ]
      return ()
