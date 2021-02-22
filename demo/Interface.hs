{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface (mkUpdateChan, runUi) where

import qualified Data.Text as Text

import Brick hiding (str)
import Brick.BChan
import Brick.Focus
import qualified Brick
import Brick.Forms
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E

import Lens.Micro.Platform

import Data.CARD
import Data.CARD.Store
import Lang.Carol
import Lang.Carol.Bank

type Ress' = Ress String Counter

data CustomEvent = StoreUpdate (Counter,Ress') | GotMessage deriving Show

data CommandForm = CommandForm { _cfCommand :: Text.Text } deriving Show

data NetStat = Online | Offline deriving (Show,Read,Eq,Ord)

makeLenses ''CommandForm

data Name = CommandField deriving (Show,Read,Eq,Ord)

mkForm :: CommandForm -> Form CommandForm e Name
mkForm = newForm [ editTextField cfCommand CommandField (Just 1) ]

type St e = (NetStat, (Counter,Ress'), Form CommandForm e Name)

draw :: String -> St e -> [Widget Name]
draw i (ns,(Counter b,r),f) = 
  [C.vCenter $ 
   C.hCenter (Brick.str $ "[ node " ++ i ++ " ]")
   <=> C.hCenter form
   <=> C.hCenter store
   <=> C.hCenter status
   <=> C.hCenter resStore]
  where form = renderForm f
        store = Brick.str $ "Balance: $" <> (show b)
        status = Brick.str $ "Network status: " <> (show ns)
        resStore = Brick.str $ "Res: " <> prettyRes (resLookup i r)

prettyRes :: [Effect Counter] -> String
prettyRes ((Effect [Add n]) :es) = "+" ++ show n ++ " " ++ prettyRes es
prettyRes ((Effect [Sub n]) :es) = "-" ++ show n ++ " " ++ prettyRes es
prettyRes (e:es) = show e ++ " " ++ prettyRes es
prettyRes [] = ""

thd (_,_,a) = a

app :: (CCarrier c Counter IO) 
    => String
    -> c
    -> App (St CustomEvent) CustomEvent Name
app i cc = App 
  { appDraw = draw i
  , appHandleEvent = \(ns,(c,r),f) ev -> case ev of
      VtyEvent (V.EvResize {}) -> continue (ns,(c,r),f)
      VtyEvent (V.EvKey V.KEsc []) -> halt (ns,(c,r),f)
      VtyEvent (V.EvKey V.KEnter []) -> suspendAndResume $ do
        case Text.words ((formState f) ^.cfCommand) of
          ["dp",v] -> do 
            carolAsync' cc $ deposit (read . Text.unpack $ v)
            return (ns,(c,r),mkForm $ CommandForm "")
          ["wd",v] -> do 
            carolAsync' cc $ withdraw (read . Text.unpack $ v)
            return (ns,(c,r),mkForm $ CommandForm "")
          ["audit"] -> do c' <- carol cc currentS
                          return (ns,(Counter c',r),mkForm $ CommandForm "")
          ["prod",v] -> do
            carolAsync' cc $ depositR (read . Text.unpack $ v)
            return (ns,(c,r),mkForm $ CommandForm "")
          ["cons"] -> do
            carolAsync' cc withdrawR
            return (ns,(c,r),mkForm $ CommandForm "")
          _ -> return (ns,(c,r),f)
      AppEvent (StoreUpdate (c',r')) -> continue (ns,(c',r'),f)
      AppEvent GotMessage -> continue (Online,(c,r),f)
      _ -> do
        f' <- handleFormEvent ev f
        continue (ns,(c,r),f')
  , appChooseCursor = focusRingCursor (formFocus . thd)
  , appStartEvent = return
  , appAttrMap = const theMap }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow) ]

mkUpdateChan :: IO (BChan CustomEvent, (Counter,Ress') -> IO (), IO ())
mkUpdateChan = do
  chan <- newBChan 100
  return (chan, writeBChan chan . StoreUpdate, writeBChan chan GotMessage)

runUi :: (CCarrier c Counter IO) => String -> Counter -> c -> BChan CustomEvent -> IO ()
runUi nodeId s0 conn chan = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      f = (Offline, (s0,mempty), mkForm (CommandForm ""))
  vty0 <- buildVty
  f' <- customMain vty0 buildVty (Just chan) (app nodeId conn) f
  return ()
