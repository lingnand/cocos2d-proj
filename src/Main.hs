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
import Reflex.Cocos2d.Prelude hiding (Box)
import qualified Platformer as P

-- | Load the given resources with a loading message, return an Event that
-- fires with the loading finishes
loadingScene :: NodeGraph t m => P2 Double -> [String] -> FreeT (Event t) m ()
loadingScene labelPosition resources = do
    (progress, finished) <- lift $ load resources
    lift $ do
      -- loading scene
      percent <- foldDyn (const . uncurry ((/) `on` fromIntegral)) (0::Double) progress
      notice <- forDyn percent $ \p -> "Loading " ++ showFFloat (Just 2) (p*100) "%..."
      label_ [ pos := labelPosition
             , fontSize := 25
             , dyn' text := notice
             ]
    liftF finished

data LiveState = Alive | Dead deriving (Enum, Generic, Enumerable, Eq, Ord, Show, Read)

data CollisionType = Ground | Box LiveState | Enemy LiveState deriving (Generic, Enumerable, Eq, Ord, Show, Read)

instance Enum CollisionType where
    toEnum = toEnum_enumerable array_CollisionType
    fromEnum = fromEnum_enumerable table_CollisionType

array_CollisionType :: A.Array Int CollisionType
array_CollisionType = array_enumerable

table_CollisionType :: M.Map CollisionType Int
table_CollisionType = table_enumerable

box :: NodeGraph t m
    => Event t (DragEvent t) -- ^ drags
    -> DynSpace t
    -> Double -- ^ box side len as in physical simulation
    -> P2 Double
    -> String -- ^ box image
    -> Dynamic t String -- ^ animation when box broken
    -> m (Event t ()) -- ^ recycle Event
box drags sp sideLen outOfStageP boxImage brokenAnim = mdo
    -- create a box state that represents the box (data)
    let dbeganE = (^.dragBegan) <$> drags
        brokenE = fforMaybe (b^.collisionBegan) $ \a ->
                      case (a^.thisCollisionType, a^.otherCollisionType) of
                        (_, Box _) -> Nothing
                        (Box Alive, _) -> Just ()
                        _ -> Nothing
    dmovedE <- switchPromptly never $ (^.dragMoved) <$> drags
    droppingE <- switchPromptly never $ (^.dragEnded) <$> drags
    outOfStageE <- delay' 2 brokenE
    let ctE  = leftmost [ Box Dead <$ brokenE, Box Alive <$ droppingE ]
        setBodyPosE =  leftmost [ (^.loc) <$> dbeganE
                                , (^.loc) <$> dmovedE
                                , outOfStageP <$ outOfStageE ]
    b <- dynamicBody sp [ def & shape      .~ Poly (square sideLen)
                              & mass       .~ 5
                              & friction   .~ 0.2
                              & elasticity .~ 1
                        ]
                        [ pos               := outOfStageP
                        , evt pos           := setBodyPosE
                        , evt rot           := xDir <$ setBodyPosE
                        , evt vel           := 0 <$ setBodyPosE
                        , evt angularVel    := 0 <$ droppingE
                        , active            := False -- sleep the bodies on start
                        , evt active        := leftmost [ True <$ droppingE
                                                        , False <$ outOfStageE ]
                        , collisionType     := Box Dead
                        , evt collisionType := ctE
                        ]
    boxFrameD <- joinDyn <$> holdDyn (constDyn boxImage) (brokenAnim <$ brokenE)
    sprite_ [ dyn (divided pos rot) := b^.transDyn
            , dyn spriteName        := boxFrameD
            ]
    return outOfStageE


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
                        Box Alive -> Just Dead
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

boxThrower :: (NodeGraph t m, PrimMonad m)
           => Gen (PrimState m)
           -> V2 Double
           -> Event t (DragEvent t)
           -> Event t NominalDiffTime -> FreeT (Event t) m ()
boxThrower gen winSize@(V2 sw sh) drags ts = do
    lift $ askPostBuildEvent >>= runEvent_ . (liftIO (putStrLn "second scene built") <$)
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
                    , collisionType := Ground
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
