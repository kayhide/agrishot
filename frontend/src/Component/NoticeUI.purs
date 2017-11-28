module Component.NoticeUI where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(Milliseconds), delay, error)
import Control.Monad.Eff.Exception (Error)
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HM


data Notice = Info String | Alert String

noticeBody :: Notice -> String
noticeBody (Info s) = s
noticeBody (Alert s) = s


type ItemId = Int

type Item m =
  { notice :: Notice
  , id :: ItemId
  , animated :: Array String
  , pinned :: Boolean
  , canceler :: Error -> m Unit
  }

type State m =
  { items :: Array (Item m)
  , lastId :: ItemId
  }

data Query a
  = Post Notice a
  | Pin ItemId a
  | WaitAndClose ItemId Milliseconds a

type Input = Unit

data Message =
  Closed Int

ui :: forall eff. H.Component HH.HTML Query Input Message (Aff eff)
ui =
  H.component
    { initialState: const { items: [], lastId: 0 }
    , render
    , eval
    , receiver: const Nothing
    }

render :: forall m. State m -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.classes [ H.ClassName "fixed-bottom" ] ]
  [
    HH.div
    [ HP.class_ $ H.ClassName "container" ]
    $ renderItem <$> state.items
  ]

  where
    renderItem item =
      HH.div
      [ HP.classes $ H.ClassName <$> item.animated ]
      [
        HH.div
        [ classes item.notice ]
        [
          HH.div
          [ HP.class_ $ H.ClassName "position-relative" ]
          [
            HH.text $ noticeBody item.notice
          , HH.a
            [ HE.onClick $ HE.input_ $ Pin item.id
            , HP.href "#"
            , HP.class_ $ H.ClassName "position-absolute-right position-absolute-top alert-link"
            ]
            [
              renderPinIcon item.pinned
            ]
          ]
        ]
      ]

    classes (Info s) = HP.class_ $ H.ClassName "alert alert-info"
    classes (Alert s) = HP.class_ $ H.ClassName "alert alert-danger"

    renderPinIcon false =
      HH.i [ HP.class_ $ H.ClassName "notice-pin fa fa-map-pin fa-rotate-90" ] []
    renderPinIcon true =
      HH.i [ HP.class_ $ H.ClassName "notice-pin notice-pin-on fa fa-map-pin" ] []

eval :: forall eff. Query ~> H.ComponentDSL (State (Aff eff)) Query Message (Aff eff)
eval = case _ of
  Post notice next -> do
    item <- newItem notice <<< (_ + 1) =<< H.gets _.lastId
    addItem item
    void $ HM.fork do
      H.liftAff $ delay (Milliseconds 100.0)
      updateItem item.id _{ animated = ["notice", "notice-in"] }

    eval $ WaitAndClose item.id (Milliseconds 3000.0) next

  Pin id next -> do
    item <- findItem id
    case item of
      Just item_ -> do
        let pinned = not item_.pinned
        updateItem id _{ pinned = pinned }
        case pinned of
          false -> do
            eval $ WaitAndClose id (Milliseconds 1000.0) next
          true -> do
            H.liftAff $ item_.canceler (error "cancelled")
            pure next

      Nothing ->
        pure next


  WaitAndClose id ms next -> do
    item <- findItem id
    case item of
      Just item_ -> do
        H.liftAff $ item_.canceler (error "cancelled")
        canceler <- HM.fork do
          H.liftAff $ delay ms
          updateItem id _{ animated = ["notice", "notice-out"] }
          H.liftAff $ delay (Milliseconds 500.0)
          deleteItem id
        updateItem id _{ canceler = canceler }
        pure next
      Nothing ->
        pure next

  where
    newItem notice id = do
      canceler <- HM.fork $ pure unit
      pure { notice: notice
           , id: id
           , animated: ["notice", "notice-out"]
           , pinned: false
           , canceler: canceler
           }

    addItem item = do
      items <- H.gets _.items
      H.modify _{ items = items <> [item], lastId = item.id }

    findItem id = do
      items <- H.gets _.items
      pure $ Array.find ((==) id <<< _.id) items

    updateItem id updater = do
      items <- H.gets _.items
      let items_ = do
            i <- Array.findIndex ((==) id <<< _.id) items
            Array.modifyAt i updater items
      case items_ of
        Just items__ -> H.modify _{ items = items__ }
        Nothing -> pure unit

    deleteItem id = do
      items <- H.gets _.items
      let items_ = do
            i <- Array.findIndex ((==) id <<< _.id) items
            Array.deleteAt i items
      case items_ of
        Just items__ -> H.modify _{ items = items__ }
        Nothing -> pure unit
