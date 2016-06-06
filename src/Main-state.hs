{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import GHC.Generics
import Numeric
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Fixed
import Data.Function
import Data.Enumerate
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Reflex
import Reflex.Cocos2d.Prelude
import qualified Platformer as P

-- State model of the app --

data AppState = AppState
              { -- some shared app state
              , SceneState
              }

data MenuButtonConfig = MenuButtonConfig
    { _menuButtonPos :: P2 Double
    , _menuButtonSideLen :: Double
    , _menuButtonColor :: Colour Double
    , _menuButtonScene :: SceneData
    }

data StartSceneConfig =
    { _buttonSideLength :: Double
    , _buttonConfigs :: [MenuButtonConfig]
    }

data SceneData
    = StartScene { _startSceneConfig :: StartSceneConfig }

data LoadSceneConfig = LoadSceneConfig
    { _loadTextPos :: P2 Double
    , _loadTextFontSize :: Double
    }

data EnemyStatus = EnemyAlive | EnemyDead deriving (Enum, Generic, Enumerable, Eq, Ord, Show, Read)

data CollisionType = CTGround | CTBox BoxStatus | Enemy EnemyStatus deriving (Generic, Enumerable, Eq, Ord, Show, Read)

instance Enum CollisionType where
    toEnum = toEnum_enumerable array_CollisionType
    fromEnum = fromEnum_enumerable table_CollisionType


array_CollisionType :: A.Array Int CollisionType
array_CollisionType = array_enumerable

table_CollisionType :: M.Map CollisionType Int
table_CollisionType = table_enumerable

-- | Load the given resources with a loading message, return an Event that
-- fires with the loading finishes
loadingScene :: NodeGraph t m => LoadSceneConfig -> FreeT (Event t) m ()
loadingScene config = do
    (progress, finished) <- lift $ load resources
    lift $ do
      -- loading scene
      percent <- foldDyn (const . uncurry ((/) `on` fromIntegral)) (0::Double) progress
      notice <- forDyn percent $ \p -> "Loading " ++ showFFloat (Just 2) (p*100) "%..."
      label_ [ pos := config^.loadTextPos
             , fontSize := config^.loadTextFontSize
             , dyn' text := notice
             ]
    liftF finished

data Animation = Animation { _animFrames :: [String]
                           , _animFps :: Double
                           }


data BoxConfig = BoxConfig
    { _boxSideLen :: Double
    , _boxMass :: Double
    , _boxFriction :: Double
    , _boxElasticity :: Double
    , _boxOutOfStagePos :: P2 Double
    , _boxImage :: String
    , _boxBrokenAnimDyn :: Dynamic t String
    , _boxBrokenKeepInStageDur :: Double
    }

data BoxStatus = BoxDragged | BoxFreeForm | BoxBroken | BoxRemoved deriving (Enum, Generic, Enumerable, Eq, Ord, Show, Read)

data BoxState = BoxState
    { _boxPos :: P2 Double
    , _boxRot :: Direction V2 Double
    , _boxStatus :: BoxStatus
    } deriving (Eq, Ord, Show, Read)

box :: NodeGraph t m => DynSpace t -> BoxConfig -> DynStateT BoxState t m ()
box sp conf = do
    -- body and view
    outOfStageE <- delay' 2 brokenE
    posDyn <- asks (^.boxPos)
    rotDyn <- asks (^boxRot)
    statusDyn <- asks (^.boxStatus)
    let demuxStatusDyn = demux statusDyn
    isFreeFormDyn <- getDemuxed demuxStatusDyn BoxFreeForm
    ctDyn <- map CTBox statusDyn
    b <- dynamicBody sp [ def & shape      .~ Poly (square $ conf^.boxSideLen)
                              & mass       .~ conf^.boxMass
                              & friction   .~ conf^.boxFriction
                              & elasticity .~ conf^.boxElasticity
                        ]
                        [ dyn pos           := posDyn
                        , dyn rot           := rotDyn
                        , dyn active        := isFreeFormDyn
                        -- , evt vel           := 0 <$ setBodyPosE
                        -- , evt angularVel    := 0 <$ droppingE
                        , dyn collisionType := ctDyn
                        ]
    frameDyn <- (fmap joinDyn) . forDyn statusDyn $ \case
                    BoxBroken -> conf^.boxBrokenAnimDyn
                    _ -> constDyn (conf^.boxImage)
    sprite_ [ dyn (divided pos rot) := b^.transDyn
            , dyn spriteName        := frameDyn
            ]
    -- state modifiers
    let brokenE = fforMaybe (b^.collisionBegan) $ \a ->
                    case (a^.thisCollisionType, a^.otherCollisionType) of
                      (_, CTBox _) -> Nothing
                      (CTBox FreeForm, _) -> Just ()
                      _ -> Nothing
    removeE <- delay (conf^.boxBrokenKeepInStageDur) brokenE
    isRemovedDyn <- getDemuxed demuxStatusDyn BoxRemoved
    isDraggedDyn <- getDemuxed demuxStatusDyn BoxDragged
    modifyDyn [ (boxStatus .~ BoxBroken) <$ brokenE
              , (boxStatus .~ BoxRemoved) <$ removeE
              -- whenever a box is dragged, we keep the rotation to xDir
              , (boxRot .~ xDir) <$ updated isDraggedDyn
              -- whenever a box is removed, we move the position to outOfStage
              , (boxPos .~ conf^.boxOutOfStagePos) <$ fmapMaybe guard (updated isRemovedDyn)
              ]

spawnEnemies :: (NodeGraph t m)
             => V2 Double
             -> DynSpace t
             -> Event t NominalDiffTime
             -> V2 Double -- ^ enemy rectangular size
             -> Dynamic t String -- ^ enemy animation (alive)
             -> RandT m ()
spawnEnemies winSize@(V2 width _) sp ticks enemySize aliveAnim = do
    -- get the running total of the time
    randTsD <- lift $ foldDyn (+) 0 =<< dilate 2 ticks
    -- as time goes on, we increase the probability of starting new enemies
    spawnE <- (fmapMaybe id <$>) . picks $ ffor (updated randTsD) $ \t ->
                  let spawnProb = max (cos (realToFrac t)) 0
                  in weighted [ (Just (), spawnProb)
                              , (Nothing, 1-spawnProb)
                              ]
    let nEnemiesCache = 10
        startPos, outOfStageP :: P2 Double
        startPos = 0 .+^ winSize/2
        outOfStageP = 0 .+^ winSize*2
        enemySpeed = width/5
        enemyY = enemySize^._y/2
    lift $ mdo
      -- set up the stack of enemies cache
      (_, spawnEs, _) <- distribute spawnE nEnemiesCache recycleEs
      recycleEs <- forM spawnEs $ \spawnE' -> mdo
        let aliveE = Alive <$ spawnE'
            dieE = fforMaybe (b^.collisionBegan) $ \a -> case a^.otherCollisionType of
                        CTBox Alive -> Just Dead
                        _ -> Nothing
        aliveD <- holdDyn Dead $ leftmost [ aliveE, dieE ]
        posFlipDyn <- fmap joinDyn $ forMDyn aliveD $ \case
          Alive -> (foldDyn (+) 0 ticks >>=) . mapDyn $ \t ->
              let dist = (realToFrac t) * enemySpeed + startPos^._x
                  rem = dist `mod'` (2*width)
              in if rem > width then ((2*width-rem) ^& enemyY, True {- flipped -})
                                else (rem ^& enemyY, False)
          Dead -> return $ constDyn (outOfStageP, False)
        posDyn <- mapDyn fst posFlipDyn
        b <- staticBody sp [ def & shape      .~ Poly (uncurry rect $ unr2 enemySize)
                                 & mass       .~ 30
                                 & elasticity .~ 0.2
                           ]
                           [ dyn' pos          := posDyn
                           , collisionType     := Enemy Dead
                           , evt collisionType := Enemy <$> updated aliveD
                           ]
        animD <- fmap joinDyn . forDyn aliveD $ \case
          Alive -> aliveAnim
          Dead -> constDyn ""
        flippedD <- mapDyn snd posFlipDyn
        sprite_ [ dyn (divided pos rot) := b^.transDyn
                , dyn flippedX          := flippedD
                , dyn spriteName        := animD
                ]
        return dieE
      return ()

data BoxThrowerSceneState = BoxThrowerSceneState
    { _boxes :: M.Map Int BoxState -- ids are just simple indexes
    , _removedBoxes :: [Int]
    }

boxThrower :: (NodeGraph t m, PrimMonad m)
           => Gen (PrimState m)
           -> V2 Double
           -> Event t (DragEvent t)
           -> Event t NominalDiffTime -> FreeT (Event t) m ()
boxThrower gen winSize@(V2 sw sh) drags ts = do
    let midP = (0 .+^ winSize/2)
        boxImage = "res/box.png"
        enemyFns = [ "res/walk/000"++show i++".png" | i <- [1..8]::[Int] ]
    -- Scene 1
    loadingScene midP $ boxImage:enemyFns
    -- Scene 2
    lift $ do
      let nBoxes = 5
          sideLen = 150
          outOfStageP = 0 .+^ winSize*2
      sp <- space ts [ iterations := 5
                     , gravity    := 0^&(-100)
                     ]
      -- create the walls
      staticBody sp [ def & shape .~ Segment 0 a b
                          & elasticity .~ 0.6
                    | let rectPts = rect sw sh
                    , (a, b) <- zip rectPts (tail $ cycle rectPts)
                    ]
                    [ pos           := midP
                    , collisionType := CTGround
                    ]
      -- create simple squares
      rec (stD, dsEs, _) <- distribute drags nBoxes recycles
          recycles <- forM dsEs $ \dsE -> box dsE sp sideLen outOfStageP boxImage (constDyn boxImage)
      -- spawn the enemies
      enemyAnimD <- holdDyn (head enemyFns)
                    =<< zipListWithEvent const (cycle enemyFns)
                    =<< dilate (1/10) ts
      runRandT ?? gen $ spawnEnemies winSize sp ts (150 ^& 260) enemyAnimD
      nBoxesLeftTxtD <- (nubDyn <$> mapDyn length stD) >>= mapDyn (\n -> show n ++ " BOXES left")
      label_ [ pos      := 0 .+^ winSize & _x *~ 0.9 & _y *~ 0.05
             , fontSize := 20
             , dyn text := nBoxesLeftTxtD
             ]


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
    gen <- createSystemRandom
    mainScene $ do
      askPostBuildEvent >>= runEvent_ . (liftIO (putStrLn "OH YEAH - main stuff finished building") <$)
      evts <- uiEvents
      dks <- dynKeysDown (evts^.keyPressed) (evts^.keyReleased)
      ts <- ticks
      -- fps10 <- dilate (1/10) ts
      drags <- dragged (evts^.singleTouches)
      void $ layerColor [ color := blueviolet ] -<< do
        lift $ askPostBuildEvent >>= runEvent_ . (liftIO (putStrLn "first scene built") <$)
        [lTouched, rTouched] <- forM [ (300, yellow)
                                     , (500, brown) ] $ \(x, c) -> lift $ do
            l <- layerColor [ pos   := x ^& (winSize^._y/2)
                            , size  := pure 100
                            , color := c
                            ]
            filterMEvent ?? (evts^.touchBegan) $ nodeContains l . (^.loc)
        wrap $ leftmost [ P.platformer winSize dks ts <$ lTouched
                        , boxThrower gen winSize drags ts <$ rTouched
                        ]
