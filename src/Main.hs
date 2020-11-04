{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Reflex
import Reflex.Dom
import Text.Read (readMaybe)

main :: IO ()
main =
  mainWidgetWithHead headWidget tutorial

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text "reflex-stone"

bodyWidget ::
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) =>
  m ()
bodyWidget = do
  el "h1" $ text "reflex-stone"
  clicked <- stoneButton
  cnt <- foldDyn (+) (0 :: Int) $ 1 <$ clicked
  elClass "p" "result" $ do
    dyn_ $
      ffor cnt $ \case
        0 -> text "Go ahead and hit the stone"
        n -> do
          text $ T.pack (show n)
          text " heads!"
  divClass "footer" $ do
    elAttr "a" ("href" =: homePage) $
      text "View source on GitHub"
  where
    homePage = "https://github.com/srid/reflex-stone"

stoneButton :: DomBuilder t m => m (Event t ())
stoneButton = do
  let attr = "style" =: "font-size: 200%;"
  clickEvent $ elAttr' "button" attr stone

stone :: DomBuilder t m => m ()
stone =
  text "🗿"

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent w =
  fmap (fmap (const ()) . domEvent Click . fst) w

tutorial ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m ()
tutorial = el "div" $ do
  tutorial1
  tutorial2
  tutorial3
  tutorial4
  tutorial5
  tutorial6
  tutorial7
  tutorial8
  tutorial9

tutorial1 ::
  ( DomBuilder t m
  ) =>
  m ()
tutorial1 = el "div" $ text "Welcome to Reflex"

tutorial2 ::
  ( DomBuilder t m
  ) =>
  m ()
tutorial2 = el "div" $ do
  el "p" $ text "Reflex is:"
  el "ul" $ do
    el "li" $ text "Effectient"
    el "li" $ text "Higher-order"
    el "li" $ text "Glitch-free"

tutorial3 ::
  ( DomBuilder t m,
    PostBuild t m
  ) =>
  m ()
tutorial3 = el "div" $ do
  t <- inputElement def
  text " "
  dynText $ _inputElement_value t

tutorial4 ::
  ( DomBuilder t m,
    PostBuild t m
  ) =>
  m ()
tutorial4 = el "div" $ do
  t <-
    inputElement $
      def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  text " "
  dynText $ _inputElement_value t

tutorial5 ::
  ( DomBuilder t m,
    PostBuild t m
  ) =>
  m ()
tutorial5 = el "div" $ do
  x <- numberInput
  let numberString = fmap (pack . show) x
  text " "
  dynText numberString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n

tutorial6 ::
  ( DomBuilder t m,
    PostBuild t m
  ) =>
  m ()
tutorial6 = el "div" $ do
  nx <- numberInput
  text " + "
  ny <- numberInput
  text " = "
  let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
      resultString = fmap (pack . show) result
  dynText resultString
  where
    numberInput ::
      ( DomBuilder t m
      ) =>
      m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n

data Op = Plus | Minus | Times | Divide
  deriving (Eq, Ord, Show)

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (/)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

tutorial7 ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  m ()
tutorial7 = el "div" $ do
  nx <- numberInput
  op <- _dropdown_value <$> dropdown Times (constDyn ops) def
  ny <- numberInput
  text " = "
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x, y) -> runOp o <$> x <*> y) op values
      resultString = fmap (pack . show) result
  dynText resultString
  where
    numberInput ::
      ( DomBuilder t m
      ) =>
      m (Dynamic t (Maybe Double))
    numberInput = do
      n <-
        inputElement $
          def
            & inputElementConfig_initialValue .~ "0"
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n

buttonClass ::
  ( DomBuilder t m
  ) =>
  Text ->
  Text ->
  m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

numberPad ::
  ( DomBuilder t m
  ) =>
  m (Event t Text)
numberPad = do
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> buttonClass "number zero" "0"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
  where
    numberButton = buttonClass "number"

tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 =
  el "div" $
    do
      numberButton <- numberPad
      clearButton <- button "C"
      let buttons =
            leftmost
              [ Nothing <$ clearButton,
                Just <$> numberButton
              ]
      dstate <- accumDyn collectButtonPresses initialState buttons
      text " "
      dynText dstate
  where
    initialState :: Text
    initialState = T.empty

    collectButtonPresses :: Text -> Maybe Text -> Text
    collectButtonPresses state buttonPress =
      case buttonPress of
        Nothing -> initialState
        Just digit -> state <> digit

data CalcState = CalcState
  { _calcState_acc :: Double, -- accumulator
    _calcState_op :: Maybe Op, -- most recently requested operation
    _calcState_input :: Text -- current input
  }
  deriving (Show)

data Button
  = ButtonNumber Text
  | ButtonOp Op
  | ButtonEq
  | ButtonClear

initCalcState :: CalcState
initCalcState = CalcState 0 Nothing ""

updateCalcState :: CalcState -> Button -> CalcState
updateCalcState state@(CalcState acc mOp input) btn =
  case btn of
    ButtonNumber d ->
      if d == "." && T.find (== '.') input /= Nothing
        then state
        else CalcState acc mOp (input <> d)
    ButtonOp pushedOp -> applyOp state (Just pushedOp)
    ButtonEq -> applyOp state Nothing
    ButtonClear -> initCalcState

applyOp :: CalcState -> Maybe Op -> CalcState
applyOp state@(CalcState acc mOp input) mOp' =
  if T.null input
    then CalcState acc mOp' input
    else case readMaybe (unpack input) of
      Nothing -> state
      Just x -> case mOp of
        Nothing -> CalcState x mOp' ""
        Just op -> CalcState (runOp op acc x) mOp' ""

displayCalcState :: CalcState -> Text
displayCalcState (CalcState acc _op input) =
  if T.null input
    then pack (show acc)
    else input

tutorial9 ::
  ( DomBuilder t m,
    MonadHold t m,
    MonadFix m,
    PostBuild t m
  ) =>
  m ()
tutorial9 = el "div" $ do
  numberButtons <- numberPad
  bPeriod <- ("." <$) <$> button "."
  bPlus <- (Plus <$) <$> button "+"
  bMinus <- (Minus <$) <$> button "-"
  bTimes <- (Times <$) <$> button "*"
  bDivide <- (Divide <$) <$> button "/"
  let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
  bEq <- button "="
  bClear <- button "C"
  let buttons =
        leftmost
          [ ButtonNumber <$> numberButtons,
            ButtonNumber <$> bPeriod,
            ButtonOp <$> opButtons,
            ButtonEq <$ bEq,
            ButtonClear <$ bClear
          ]
  calcState <- accumDyn updateCalcState initCalcState buttons
  text " "
  dynText (displayCalcState <$> calcState)
