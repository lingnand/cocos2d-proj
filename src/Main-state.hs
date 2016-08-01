{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import Numeric
import Data.Function
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Free
import Reflex
import Reflex.Cocos2d.Prelude

import Config
import State
-- import qualified Platformer as P

-- State model of the app --

-- | Load the given resources with a loading message, return an Event that
-- fires with the loading finishes
loadingScene :: NodeGraph t m => LoadSceneConfig -> [String] -> m (Event t ())
loadingScene config resources = do
    (progress, finished) <- load resources
    -- loading scene
    percent <- foldDyn (const . uncurry ((/) `on` fromIntegral)) (0::Double) progress
    notice <- forDyn percent $ \p -> "Loading " ++ showFFloat (Just 2) (p*100) "%..."
    label_ [ position  := config^.loadText.textPos
           , fontSize  := config^.loadText.textFontSize
           , dyn' text := notice
           ]
    return finished

mkBox :: NodeGraph t m => DynSpace t -> BoxConfig t -> DynStateT BoxState t m (Event t ())
mkBox sp conf = do
    -- body and view
    -- setters should be nubbed
    setPosDyn <- nubDyn <$> asksDyn (^.boxSetPos)
    statusDyn <- nubDyn <$> asksDyn (^.boxStatus)
    setPosDyn' <- let comb _ BoxRemoved = conf^.boxOutOfStagePos
                      comb p _ = p
                  in nubDyn <$> combineDyn comb setPosDyn statusDyn
    usePhysicsDyn <- mapDyn (`elem` [BoxFreeForm, BoxBroken]) statusDyn
    b <- dynamicBody sp [ fixtures                      := [
                            def & shape      .~ Poly (square $ conf^.boxSideLen)
                                & mass       .~ conf^.boxMass
                                & friction   .~ conf^.boxFriction
                                & elasticity .~ conf^.boxElasticity
                          ]
                        , dyn position                  := setPosDyn'
                        , dyn (xDir >$ rotation)             := setPosDyn'
                        , dyn (0 >$ velocity)           := setPosDyn'
                        , dyn (0 >$ angularVelocity)    := setPosDyn'
                        , dyn (CTBox >$< collisionType) := statusDyn
                        , dyn active                    := usePhysicsDyn
                        ]
    frameDyn <- (fmap joinDyn) . forDyn statusDyn $ \case
                    BoxBroken -> conf^.boxBrokenAnimDyn
                    _ -> constDyn (conf^.boxImage)
    sprite_ [ dyn transform  := b^.transDyn
            , dyn spriteName := frameDyn
            ]
    -- state modifiers
    let brokenE = fforMaybe (b^.collisionBegan) $ \a ->
                    case (a^.thisCollisionType, a^.otherCollisionType) of
                      (_, CTBox _) -> Nothing
                      (CTBox BoxFreeForm, _) -> Just ()
                      _ -> Nothing
    removedE <- delay (conf^.boxBrokenKeepInStageDur) brokenE
    mapM_ modifyDyn [ (boxStatus .~ BoxBroken) <$ brokenE
                    , (boxStatus .~ BoxRemoved) <$ removedE
                    ]
    return removedE

-- spawnEnemies :: (NodeGraph t m)
--              => V2 Double
--              -> DynSpace t
--              -> Event t NominalDiffTime
--              -> V2 Double -- ^ enemy rectangular size
--              -> Dynamic t String -- ^ enemy animation (alive)
--              -> RandT m ()
-- spawnEnemies winSize@(V2 width _) sp ticks enemySize aliveAnim = do
--     randTsD <- lift $ foldDyn (+) 0 =<< dilate 2 ticks
--     -- as time goes on, we increase the probability of starting new enemies
--     spawnE <- (fmapMaybe id <$>) . picks $ ffor (updated randTsD) $ \t ->
--                   let spawnProb = max (cos (realToFrac t)) 0
--                   in weighted [ (Just (), spawnProb)
--                               , (Nothing, 1-spawnProb)
--                               ]
--     let nEnemiesCache = 10
--         startPos, outOfStageP :: P2 Double
--         startPos = 0 .+^ winSize/2
--         outOfStageP = 0 .+^ winSize*2
--         enemySpeed = width/5
--         enemyY = enemySize^._y/2
--     lift $ mdo
--       -- set up the stack of enemies cache
--       (_, spawnEs, _) <- distribute spawnE nEnemiesCache recycleEs
--       recycleEs <- forM spawnEs $ \spawnE' -> mdo
--         let aliveE = Alive <$ spawnE'
--             dieE = fforMaybe (b^.collisionBegan) $ \a -> case a^.otherCollisionType of
--                         CTBox Alive -> Just Dead
--                         _ -> Nothing
--         aliveD <- holdDyn Dead $ leftmost [ aliveE, dieE ]
--         posFlipDyn <- fmap joinDyn $ forMDyn aliveD $ \case
--           Alive -> (foldDyn (+) 0 ticks >>=) . mapDyn $ \t ->
--               let dist = (realToFrac t) * enemySpeed + startPos^._x
--                   rem = dist `mod'` (2*width)
--               in if rem > width then ((2*width-rem) ^& enemyY, True {- flipped -})
--                                 else (rem ^& enemyY, False)
--           Dead -> return $ constDyn (outOfStageP, False)
--         posDyn <- mapDyn fst posFlipDyn
--         b <- staticBody sp [ def & shape      .~ Poly (uncurry rect $ unr2 enemySize)
--                                  & mass       .~ 30
--                                  & elasticity .~ 0.2
--                            ]
--                            [ dyn' pos          := posDyn
--                            , collisionType     := Enemy Dead
--                            , evt collisionType := Enemy <$> updated aliveD
--                            ]
--         animD <- fmap joinDyn . forDyn aliveD $ \case
--           Alive -> aliveAnim
--           Dead -> constDyn ""
--         flippedD <- mapDyn snd posFlipDyn
--         sprite_ [ dyn (divided pos rot) := b^.transDyn
--                 , dyn flippedX          := flippedD
--                 , dyn spriteName        := animD
--                 ]
--         return dieE
--       return ()

touchBeganReducer :: Touch -> BoxThrowerSceneState -> Maybe BoxThrowerSceneState
touchBeganReducer t s = case s^.controlledBox of
    Just _            -> Nothing
    _ | IS.null idles -> Nothing
      | otherwise     -> s & controlledBox .~ Just key
                           & idleBoxes .~ idles'
                           & boxes.at key.mapped.boxStatus .~ BoxControlled
                           & touchMovedReducer t
      where idles = s^.idleBoxes
            (key, idles') = IS.deleteFindMin idles

touchMovedReducer :: Touch -> BoxThrowerSceneState -> Maybe BoxThrowerSceneState
touchMovedReducer t s = ffor (s^.controlledBox) $ \i -> s & boxes.at i.mapped.boxSetPos .~ t^.loc

touchEndedReducer :: Touch -> BoxThrowerSceneState -> Maybe BoxThrowerSceneState
touchEndedReducer t s = s^.controlledBox >>= \i ->
                            s & touchMovedReducer t
                              & _Just.controlledBox .~ Nothing
                              -- the box is allowed to freely drop
                              & _Just.boxes.at i.mapped.boxStatus .~ BoxFreeForm

boxThrower :: (NodeGraph t m, PrimMonad m)
           => Gen (PrimState m) -> BoxThrowerSceneConfig t
           -> DynStateT BoxThrowerSceneState t m ()
boxThrower gen conf = void $ node [] <-< do
    let midP = 0 .+^ (conf^.winSize/2)
        -- enemyFns = [ "res/walk/000"++show i++".png" | i <- [1..8]::[Int] ]
    -- Scene 1
    liftF =<< lift (loadingScene (conf^.loadSceneConfig) [conf^.boxConfig.boxImage])
    -- Scene 2
    lift $ do
      sp <- space (conf^.globalTicks)
                  [ iterations := conf^.spIterations
                  , gravity    := conf^.spGravity
                  ]
      -- create the walls
      staticBody sp [ fixtures      := [
                        def & shape .~ Segment 0 a b
                              & elasticity .~ conf^.wallElasticity
                        | let rectPts = uncurry rect $ unr2 (conf^.winSize)
                        , (a, b) <- zip rectPts (tail $ cycle rectPts)
                      ]
                    , position      := midP
                    , collisionType := CTGround
                    ]
      -- create the boxes
      -- NOTE: we only build the positive difference at the moment
      boxesDiffE <- let f prev curr | diff <- IM.difference curr prev
                                    , not (IM.null diff) = (curr, Just diff)
                                    | otherwise          = (curr, Nothing)
                    in asksDyn (^.boxes) >>= pushPostBuild >>= mapAccumMaybe f IM.empty

      buildEvent_ . ffor boxesDiffE $ \bxs -> do
        -- iterate through all keys and create the event
        IM.foldlWithKey ?? return () ?? bxs $ \kr k v -> kr >> do
            removedE <- zoomDyn (boxes.at k.pnon v) $ mkBox sp (conf^.boxConfig)
            -- whenever a box is removed, push the id into idleBoxes
            modifyDyn $ (idleBoxes %~ IS.insert k) <$ removedE

      nBoxesLeftTxtD <- asksDyn $ \s -> show (IS.size $ s^.idleBoxes) ++ " BOXES left"
      label_ [ position := conf^.boxesLeftText.textPos
             , fontSize := conf^.boxesLeftText.textFontSize
             , dyn text := nBoxesLeftTxtD
             ]

      -- reducer events for the touches
      mapM_ modifyDynMaybe [ touchBeganReducer <$> conf^.touchBegan
                           , touchMovedReducer <$> conf^.touchMoved
                           , touchEndedReducer <$> conf^.touchEnded
                           ]
      -- enemyAnimD <- holdDyn (head enemyFns)
      --               =<< zipListWithEvent const (cycle enemyFns)
      --               =<< dilate (1/10) ts
      -- runRandT ?? gen $ spawnEnemies winSize sp ts (150 ^& 260) enemyAnimD

startScene :: NodeGraph t m => StartSceneConfig t -> [(MenuButtonConfig t, a)] -> m (Event t a)
startScene conf buttons = do
    es <- forM buttons $ \(bConf, a) -> do
            l <- layerColor [ position := bConf^.menuButtonPos
                            , size     := pure (bConf^.menuButtonSideLen)
                            , color    := bConf^.menuButtonColor
                            ]
            onEventMaybe (conf^.touchBegan) $ \t -> runMaybeT $ do
                contains <- nodeContains l (t^.loc)
                liftIO $ putStrLn $ show (t^.loc) ++ " " ++ show contains
                guard =<< nodeContains l (t^.loc)
                return a
    return $ leftmost es

app :: NodeGraph t m =>  DynStateT AppState t m ()
app = do
    winSize <- getWinSize
    evts <- uiEvents
    ts <- ticks
    gen <- liftIO createSystemRandom
    let appEnv = AppEnv
               { _winSize = winSize
               , _globalTicks = ts
               }
    sceneTypeDyn <- nubDynBy ((/=) `on` sceneStateType) <$> asksDyn (^.sceneState)
    renderSceneDyn <- forDyn sceneTypeDyn $ \case
      StartScene -> do
          let ssConfig = StartSceneConfig
                       { _ssToSingleTouches = evts^.singleTouches
                       }
          nextScene <- startScene ssConfig
                          [ (
                              MenuButtonConfig { _menuButtonPos = 300 ^& 320
                                               , _menuButtonSideLen = 200
                                               , _menuButtonColor = yellow
                                               }
                            , BoxThrowerScene BoxThrowerSceneState
                             {
                               _boxes = IM.fromList
                                      [ ( i
                                        , BoxState
                                         { _boxSetPos = 0 .+^ winSize*2
                                         , _boxSetRot = xDir
                                         , _boxStatus = BoxRemoved
                                         }
                                        )
                                      | i <- [1..5]
                                      ]
                             , _idleBoxes = IS.fromList [1..5]
                             , _controlledBox = Nothing
                             }
                            )
                          ]
          askPostBuildEvent >>= runEvent_ . (liftIO (putStrLn "first scene building finished!") <$)
          modifyDyn $ (sceneState .~) <$> nextScene
      BoxThrowerScene bts ->  do
          let btsConfig = BoxThrowerSceneConfig
                        { _btsToAppEnv = appEnv
                        , _btsToLoadSceneConfig = LoadSceneConfig
                                                { _loadText = TextConfig
                                                            { _textPos = 0 .+^ winSize/2
                                                            , _textFontSize = 25
                                                            }
                                                }
                        , _btsToSingleTouches = evts^.singleTouches
                        , _spGravity = 0 ^& (-100)
                        , _spIterations = 5
                        , _wallElasticity = 0.6
                        , _boxConfig = BoxConfig
                                     { _boxSideLen = 150
                                     , _boxMass = 5
                                     , _boxFriction = 0.2
                                     , _boxElasticity = 1
                                     , _boxOutOfStagePos = 0 .+^ winSize*2
                                     , _boxImage = "res/box.png"
                                     , _boxBrokenAnimDyn = constDyn "res/box.png"
                                     , _boxBrokenKeepInStageDur = 2
                                     }
                        , _boxesLeftText = TextConfig
                                         { _textPos = 0 .+^ winSize & _x *~ 0.9
                                                                    & _y *~ 0.05
                                         , _textFontSize = 20
                                         }
                        }
          zoomDyn (sceneState.pnon' _BoxThrowerScene bts) (boxThrower gen btsConfig)

    void $ layerColor [ color := blueviolet ] -<< renderSceneDyn

main = do
    os <- getOS
    putStrLn $ "OS: " ++ show os
    setAdjustViewPort True
    setDesignResolutionSize 960 640 ShowAll
    setResizeWithBrowserSize True
    mainScene $ do
      let appStateZ = AppState { _sceneState = StartScene }
      stateDyn <- execDynStateT app appStateZ
      runDyn =<< forDyn stateDyn (liftIO . print)




          -- [lTouched, rTouched] <- forM [ (300, yellow)
          --                              , (500, brown) ] $ \(x, c) -> lift $ do
          --     l <- layerColor [ pos   := x ^& (winSize^._y/2)
          --                     , size  := pure 100
          --                     , color := c
          --                     ]
          --     filterMEvent ?? (evts^.touchBegan) $ nodeContains l . (^.loc)
          -- wrap $ leftmost [ P.platformer winSize dks ts <$ lTouched
          --                 , boxThrower gen winSize drags ts <$ rTouched
          --                 ]
          --
          --
          --
          --
