{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module State where

import Reflex.Cocos2d.Prelude

import GHC.Generics
import Data.Enumerate
import qualified Data.Array as A
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS

data BoxStatus = BoxControlled | BoxFreeForm | BoxBroken | BoxRemoved deriving (Enum, Generic, Enumerable, Eq, Ord, Show, Read)

data BoxState = BoxState
    { _boxSetPos :: P2 Double
    , _boxSetRot :: Direction V2 Double
    , _boxStatus :: BoxStatus
    } deriving (Eq, Ord, Show, Read)

makeLenses ''BoxState

data BoxThrowerSceneState = BoxThrowerSceneState
    { _boxes :: IM.IntMap BoxState -- ids are just simple indexes
    , _idleBoxes :: IS.IntSet
    , _controlledBox :: Maybe Int
    } deriving (Eq, Ord, Show, Read)

makeLenses ''BoxThrowerSceneState

-- collision types

data EnemyStatus = EnemyAlive | EnemyDead deriving (Enum, Generic, Enumerable, Eq, Ord, Show, Read)

data CollisionType = CTGround | CTBox BoxStatus | Enemy EnemyStatus deriving (Generic, Enumerable, Eq, Ord, Show, Read)

array_CollisionType :: A.Array Int CollisionType
array_CollisionType = array_enumerable

table_CollisionType :: M.Map CollisionType Int
table_CollisionType = table_enumerable

instance Enum CollisionType where
    toEnum = toEnum_enumerable array_CollisionType
    fromEnum = fromEnum_enumerable table_CollisionType

data SceneState
    = StartScene
    | BoxThrowerScene BoxThrowerSceneState
    deriving (Eq, Ord, Show, Read)

makePrisms ''SceneState

type SceneStateType = Int

sceneStateType :: SceneState -> SceneStateType
sceneStateType StartScene = 0
sceneStateType (BoxThrowerScene _) = 1


data AppState = AppState
              { -- some shared app state
                -- ...
                _sceneState :: SceneState
              } deriving (Eq, Ord, Show, Read)

makeLenses ''AppState
