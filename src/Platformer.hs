{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module Platformer where

import Linear
import qualified Data.Set as S
import Data.Default
import Data.Time.Clock
import Data.Colour.Names
import Control.Lens
import Reflex
import Reflex.Cocos2d

data Player = Player { _vel :: V2 Double
                     , _pos :: V2 Double
                     } deriving (Show)

makeLenses ''Player

gravity :: V2 Double
gravity = V2 0 (-200)

speed :: V2 Double
speed = V2 300 180

arrow :: S.Set Key -> V2 Double
arrow ks = foldl app zero [ (ArrowUp, _y +~ 1)
                          , (ArrowDown, _y -~ 1)
                          , (ArrowLeft, _x -~ 1)
                          , (ArrowRight, _x +~ 1)
                          ]
    where app v (k, app) | S.member k ks = app v
                         | otherwise = v

update :: (S.Set Key, NominalDiffTime) -> Player -> Player
update (keys, dt) p = p & vel .~ v
                        & pos +~ v*dt'
                        & pos._y %~ max 0
    where v = p^.vel & _x .~ dir^._x
                     & (+gravity*dt')
                     & _y %~ if | p^.pos._y == 0 -> const (dir^._y)
                                | otherwise -> id
          dir = arrow keys * speed
          dt' = realToFrac dt

platformer :: NodeGraph t m => V2 Double -> Dynamic t (S.Set Key) -> Event t NominalDiffTime -> FixEvent t m
platformer winSize dks ts = FixEvent $ do
      dplayer <- foldDyn update (Player zero (winSize/2)) $ attachDyn dks ts
      dpos <- mapDyn (^.pos) dplayer
      -- hfor (updated dpos) $ liftIO . print
      layerColor_ $ def & position .~ dpos
                        & size .~ constDyn (pure 150)
                        & color .~ constDyn orange
      return never
