{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Config  where

import Reflex
import Reflex.Cocos2d.Prelude

-- various configuration records

data AppEnv t = AppEnv
    { _winSize :: V2 Double
    , _globalTicks :: Event t NominalDiffTime
    }

makeClassy ''AppEnv

-- start scene

data StartSceneConfig t = StartSceneConfig
    { _ssToSingleTouches :: SingleTouchEvents t
    }

makeLenses ''StartSceneConfig

instance HasSingleTouchEvents (StartSceneConfig t) t where
  singleTouches = ssToSingleTouches

data MenuButtonConfig a = MenuButtonConfig
    { _menuButtonPos :: P2 Double
    , _menuButtonSideLen :: Double
    , _menuButtonColor :: Colour Double
    }

makeLenses ''MenuButtonConfig

-- box thrower scene

data BoxConfig t = BoxConfig
    { _boxSideLen :: Double
    , _boxMass :: Double
    , _boxFriction :: Double
    , _boxElasticity :: Double
    , _boxOutOfStagePos :: P2 Double
    , _boxImage :: String
    , _boxBrokenAnimDyn :: Dynamic t String
    , _boxBrokenKeepInStageDur :: NominalDiffTime
    }

makeLenses ''BoxConfig

data TextConfig = TextConfig
    { _textPos :: P2 Double
    , _textFontSize :: Double
    }

makeLenses ''TextConfig

-- loading scene

data LoadSceneConfig = LoadSceneConfig
    { _loadText :: TextConfig
    }

makeClassy ''LoadSceneConfig

data BoxThrowerSceneConfig t = BoxThrowerSceneConfig
    { _btsToAppEnv :: AppEnv t
    , _btsToLoadSceneConfig :: LoadSceneConfig
    , _btsToSingleTouches :: SingleTouchEvents t
    , _spGravity :: V2 Double
    , _spIterations :: Int
    , _wallElasticity :: Double
    , _boxConfig :: BoxConfig t
    , _boxesLeftText :: TextConfig
    }

makeLenses ''BoxThrowerSceneConfig

instance HasAppEnv (BoxThrowerSceneConfig t) t where
  appEnv = btsToAppEnv
instance HasSingleTouchEvents (BoxThrowerSceneConfig t) t where
  singleTouches = btsToSingleTouches
instance HasLoadSceneConfig (BoxThrowerSceneConfig t) where
  loadSceneConfig = btsToLoadSceneConfig
