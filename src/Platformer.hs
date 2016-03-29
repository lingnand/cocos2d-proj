{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module Platformer where

import qualified Data.Set as S
import Reflex
import Reflex.Cocos2d.Prelude

data Player = Player { _velocity :: V2 Double
                     , _position :: P2 Double
                     } deriving (Show)

makeLenses ''Player

grav :: V2 Double
grav = V2 0 (-200)

speed :: V2 Double
speed = V2 300 180

arrow :: S.Set Key -> V2 Double
arrow ks = foldl app zero [ (ArrowUp, _y +~ 1)
                          , (ArrowDown, _y -~ 1)
                          , (ArrowLeft, _x -~ 1)
                          , (ArrowRight, _x +~ 1)
                          , (KeyK, _y +~ 1)
                          , (KeyJ, _y -~ 1)
                          , (KeyH, _x -~ 1)
                          , (KeyL, _x +~ 1)
                          ]
    where app v (k, app) | S.member k ks = app v
                         | otherwise = v

update :: (S.Set Key, NominalDiffTime) -> Player -> Player
update (keys, dt) p = p & velocity .~ v
                        & position %~ (.+^ v*dt')
                        & position._y %~ max 0
    where v = p^.velocity & _x .~ dir^._x
                          & (+grav*dt')
                          & _y %~ if | p^.position._y == 0 -> const (dir^._y)
                                     | otherwise -> id
          dir = arrow keys * speed
          dt' = realToFrac dt

platformer :: NodeGraph t m => V2 Double -> Dynamic t (S.Set Key) -> Event t NominalDiffTime -> FixEvent t m
platformer winSize dks ts = FixEvent $ do
      dplayer <- foldDyn update (Player 0 (0 .+^ winSize/2)) $ attachDyn dks ts
      dpos <- mapDyn (^.position) dplayer
      -- hfor (updated dpos) $ liftIO . print
      layerColor_ $ def & pos .~ dpos
                        & size .~ constDyn (pure 150)
                        & color .~ constDyn orange
      return never
