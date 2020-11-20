{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Counter where

import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Reflex
import Reflex.Dom

counter ::
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) =>
  m ()
counter = do
  el "h1" $ text "Counter"

  click <- button "Counter"
  acc <- foldDyn (+) (0 :: Int) $ 1 <$ click
  dynText $ T.pack . show <$> acc
