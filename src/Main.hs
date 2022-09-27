{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import System.Random

import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppDecrease
  | AppGenRandom
  | AppSaveRandom Int
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello world",
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease,
        spacer,
        button "Decrease count" AppDecrease,
        spacer,
        button "Random" AppGenRandom
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppDecrease -> [Model (model & clickCount -~ 1)]
  AppGenRandom -> [Task $
      AppSaveRandom <$> randomRIO (1, 65536)
    ]
  AppSaveRandom value -> [Model $ model & clickCount .~ value]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0
