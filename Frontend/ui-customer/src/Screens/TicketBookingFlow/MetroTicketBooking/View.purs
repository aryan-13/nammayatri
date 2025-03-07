{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketBooking.View where

import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.View as PrimaryEditText
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude
import Screens.TicketBookingFlow.MetroTicketBooking.Controller
import Screens.TicketBookingFlow.MetroTicketBooking.ComponentConfig
import Font.Style as FontStyle
import PrestoDOM.Animation as PrestoAnim
import Debug (spy)
import Engineering.Helpers.Commons as EHC
import Animation.Config
import JBridge as JB
import Data.Array
import Font.Size as FontSize
import Data.Maybe (maybe)
import Effect.Aff (launchAff)
import Services.API
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Types.App (GlobalState, defaultGlobalState, FlowBT)
import Screens.Types
import Services.Backend as Remote
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Data.Time.Duration (Milliseconds(..))
import Components.RequestInfoCard as InfoCard
import Language.Strings
import Language.Types
import Data.String as DS
import Data.Function.Uncurried (runFn1)
import Data.Maybe(Maybe(..))
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Control.Monad.Trans.Class (lift)
import Storage 

screen :: ST.MetroTicketBookingScreenState -> Screen Action ST.MetroTicketBookingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketBookingScreen"
  , globalEvents : [getQuotes]
  , eval : \action state -> do
        let _ = spy "MetroTicketBookingScreenState action " action
        let _ = spy "MetroTicketBookingScreenState state " state 
        case action of
          ShowMetroBookingTimeError _ -> pure unit
          _ -> void $ pure $ setValueToLocalStore METRO_PAYMENT_SDK_POLLING "false"
        eval action state
  }
  where
    getQuotes push = do
      let withinTimeRange = JB.withinTimeRange "04:30:00" "22:30:00" $ EHC.convertUTCtoISC (EHC.getCurrentUTC "") "HH:mm:ss"
      push $ ShowMetroBookingTimeError withinTimeRange
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ getQuotesPolling initialState.data.searchId 5 3000.0 initialState push GetMetroQuotesAction
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ getSDKPolling initialState.data.bookingId 3000.0 initialState push GetSDKPollingAC
      pure $ pure unit

getQuotesPolling :: forall action. String-> Int -> Number -> ST.MetroTicketBookingScreenState -> (action -> Effect Unit) -> (Array MetroQuote -> action) -> Flow GlobalState Unit
getQuotesPolling searchId count delayDuration state push action = do
  if state.props.currentStage == GetMetroQuote && searchId /= "" then do
    if count > 0 then do
      (getMetroQuotesResp) <- Remote.getMetroQuotes searchId
      case getMetroQuotesResp of
          Right (GetMetroQuotesRes resp) -> do
            if (not (null resp)) then do
                doAff do liftEffect $ push $ action resp
            else do
              if (count == 1) then do
                doAff do liftEffect $ push $ action resp
              else do
                void $ delay $ Milliseconds delayDuration
                getQuotesPolling searchId (count - 1) delayDuration state push action
          Left _ -> pure unit
      else 
        pure unit
  else pure unit

getSDKPolling :: forall action. String -> Number -> ST.MetroTicketBookingScreenState -> (action -> Effect Unit) -> (CreateOrderRes -> action) -> FlowBT String Unit
getSDKPolling bookingId delayDuration state push action = do
  let localPoolingStatus = getValueToLocalStore METRO_PAYMENT_SDK_POLLING
  if state.props.currentStage == PaymentSDKPooling && localPoolingStatus == "true" then do
      (GetMetroBookingStatusResp (MetroTicketBookingStatus metroTicketStatusResp)) <- Remote.getMetroStatusBT bookingId 
      let orderResp = metroTicketStatusResp.payment >>= \(FRFSBookingPaymentAPI paymentInfo) -> paymentInfo.paymentOrder 
      case orderResp of
        Just (CreateOrderRes createOrderResp) -> do
          liftFlowBT $ push $ action (CreateOrderRes createOrderResp)
        Nothing -> do
          void $ lift $ lift $ delay $ Milliseconds delayDuration
          getSDKPolling bookingId delayDuration state push action
  else pure unit

view :: forall w . (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
view push state =
    PrestoAnim.animationSet [Anim.fadeIn true]  $ frameLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black6000
    , onBackPressed push $ const BackPressed
    ] $ 
    [ linearLayout 
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.grey700
        , orientation VERTICAL
        ]
        [ headerView state push
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.greySmoke
          ][]
        , infoSelectioView state push
        ]
        , updateButtonView state push
    ] <> if state.props.showMetroBookingTimeError then [InfoCard.view (push <<< InfoCardAC) (metroTimeErrorPopupConfig state)] else [linearLayout [visibility GONE] []]

infoSelectioView :: forall w . ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
infoSelectioView state push =
    scrollView
      [ height if EHC.os == "IOS" then V (EHC.screenHeight unit) else MATCH_PARENT
      , width MATCH_PARENT    
      , padding $ PaddingBottom 75
      , scrollBarY false
      ] [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          [
            linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , margin (Margin 16 24 16 20)
                    , padding (Padding 20 20 20 20)
                    , background Color.white900
                    , cornerRadius 8.0
                    , orientation VERTICAL
                    ][ 
                      linearLayout
                        [ height MATCH_PARENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        , padding $ PaddingBottom 20
                        ][  linearLayout
                            [ width MATCH_PARENT
                            , height WRAP_CONTENT
                            , orientation HORIZONTAL
                            , padding (Padding 2 2 2 2)
                            , background Color.white900
                            , stroke $ "1," <> Color.grey900
                            , cornerRadius if EHC.os == "IOS" then 18.0 else 30.0
                            , gravity CENTER
                            ][ selectionTab (getString ONE_WAY_STR)  ST.ONE_WAY push state
                            , selectionTab (getString ROUND_TRIP_STR) ST.ROUND_TRIP push state
                            ]
                        ]
                    , srcTextView push state
                    , destTextView push state
                    , linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , gravity BOTTOM
                            ][ textView $ 
                                [ text $ getString UNCERTAIN_ABOUT_METRO_ROUTES
                                , color Color.black800
                                ] <> FontStyle.body1 TypoGraphy
                              , textView $ 
                                [ text $ " " <> (getString SEE_MAP) 
                                , color Color.blue900
                                , rippleColor Color.rippleShade
                                , onClick push $ const MetroRouteMapAction
                                ] <> FontStyle.subHeading1 TypoGraphy
                            ]
                    ]
                  , incrementDecrementView push state
                  , linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , gravity BOTTOM
                      , margin $ MarginHorizontal 16 16
                      , onClick push $ const ToggleTermsAndConditions
                          ][  imageView
                              [ height $ V 16
                              , width $ V 16 
                              , layoutGravity "center_vertical"
                              , margin $ MarginRight 8
                              , imageWithFallback $ fetchImage FF_COMMON_ASSET (if state.props.termsAndConditionsSelected then "ny_ic_checked" else "ny_ic_unchecked")
                              ]
                            , textView $ 
                              [ text $ getString I_AGREE_TO_THE
                              , color Color.black800
                              ] <> FontStyle.body1 TypoGraphy
                            , textView $ 
                              [ text $ " " <> (getString TERMS_AND_CONDITIONS)
                              , color Color.blue900
                              , onClick (\action -> do
                                      _<- push action
                                      _ <- JB.openUrlInApp $ "https://metro-terms.triffy.in/chennai/index.html"
                                      pure unit
                                      ) (const NoAction)
                              ] <> FontStyle.body1 TypoGraphy
                          ]
                  , termsAndConditionsView (getTermsAndConditions "") true
          ]
      ]

selectionTab :: forall w . String  -> ST.TicketType -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
selectionTab _text ticketType push state = 
  let ticketEnabled = ticketType == state.data.ticketType
  in
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text _text
  , background if ticketEnabled then Color.black900 else Color.white900
  , padding (Padding 8 8 8 8)
  , weight 1.0
  , gravity CENTER
  , color if ticketEnabled then Color.white900 else Color.black900
  , cornerRadius if EHC.os == "IOS" then 16.0 else 20.0
  , textSize FontSize.a_14
  , onClick (\action ->
              if state.data.ticketType /= ticketType then do
                _ <- push action
                pure unit
              else pure unit
            ) (const $ ChangeTicketTab ticketType)
  ]

termsAndConditionsView :: forall w . Array String -> Boolean -> PrestoDOM (Effect Unit) w
termsAndConditionsView termsAndConditions isMarginTop =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 16 13 16 0
  ] (mapWithIndex (\index item ->
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][ textView $
         [ textFromHtml $ " &#8226;&ensp; " <> item
         , color Color.black700
         ] <> FontStyle.tags TypoGraphy
      ]
  ) termsAndConditions )

getTermsAndConditions :: forall w . String -> Array String
getTermsAndConditions _ = [getString METRO_TERM_1 ,getString METRO_TERM_2]

headerView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (PaddingTop EHC.safeMarginTop)
    , background Color.white900
    ][  GenericHeader.view (push <<< GenericHeaderAC) (metroTicketBookingHeaderConfig state)
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , background Color.white900
        ][ linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , gravity CENTER
          , orientation VERTICAL
          , padding $ PaddingRight 16
          , background Color.white900
          ][ textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ getString MY_TICKETS
              , accessibilityHint $ "My Tickets : Button"
              , accessibility ENABLE
              , rippleColor Color.rippleShade
              , color Color.blueTextColor
              , onClick push (const $ MyMetroTicketAction)
              ] <> FontStyle.subHeading1 LanguageStyle
            ]
          ]
      ]

incrementDecrementView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
incrementDecrementView push state =
  let ticketLimit = if state.data.ticketType == ST.ROUND_TRIP then 6 else 6
      limitReached = (state.data.ticketType == ST.ROUND_TRIP && state.data.ticketCount >= ticketLimit) || (state.data.ticketType == ST.ONE_WAY && state.data.ticketCount >= ticketLimit)
  in 
  linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 0 16 16
        , cornerRadius 8.0
        , background Color.white900
        , orientation VERTICAL
        ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , cornerRadius 8.0
      , orientation VERTICAL
      , margin $ Margin 16 20 16 20
      ][  textView $
          [ text $ getString NO_OF_PASSENGERS
          , color Color.black800
          , margin $ MarginBottom 8
          ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , padding $ Padding 4 4 4 4
          , cornerRadius 8.0
          , background Color.white900
          , stroke $ "1," <> Color.grey900
          ][  textView $
              [ background Color.grey700
              , text "-"
              , gravity CENTER
              , cornerRadius 4.0
              , rippleColor Color.rippleShade
              , width WRAP_CONTENT
              , padding $ Padding 28 1 28 7
              , onClick push $ const (DecrementTicket)
              , height WRAP_CONTENT
              ] <> FontStyle.body10 TypoGraphy
            , textView $
              [ background Color.white900
              , text $ show state.data.ticketCount
              , height WRAP_CONTENT
              , color Color.black800
              , weight 1.0
              , gravity CENTER
              ] <> FontStyle.body13 TypoGraphy
            , textView $
              [ background Color.black900
              , text "+"
              , color Color.yellow900
              , padding $ Padding 28 1 28 7
              , cornerRadius 4.0
              , onClick push $ const (IncrementTicket )
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              , rippleColor Color.rippleShade
              , alpha $ if limitReached then 0.5 else 1.0
              , clickable $ if limitReached then false else true
              ] <> FontStyle.body10 TypoGraphy
          ]
        , linearLayout
                  [ height MATCH_PARENT
                  , width MATCH_PARENT
                  , orientation HORIZONTAL
                  , gravity CENTER_VERTICAL
                  ][  imageView $
                      [ width $ V 20
                      , height MATCH_PARENT
                      , padding $ PaddingVertical 5 3
                      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey" 
                      ]
                    , textView $
                      [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , text $ " " <> (getString MAXIMUM) <> " " <> (show ticketLimit) <> " " <> (getString TICKETS_ALLOWED_PER_USER)
                      , color Color.black600
                      , gravity LEFT
                      , singleLine true
                      , alpha 1.0
                      ]  <> FontStyle.body3 TypoGraphy
                  ]
      ]
  ]

updateButtonView :: forall w. ST.MetroTicketBookingScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateButtonView state push = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity BOTTOM
  , alignParentBottom "true,-1"
  , background Color.transparent
  ][ linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , padding $ PaddingVertical 5 24
      ][ PrimaryButton.view (push <<< UpdateButtonAction) (updateButtonConfig state)]
    ]

textViewForLocation :: forall w. String -> ST.LocationActionId -> (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
textViewForLocation label actionId push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginBottom 16
    ][ textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text label
          , color Color.black900
          , gravity LEFT
          , singleLine true
          , margin $ MarginBottom 10
          , alpha 1.0
          , accessibility ENABLE
          ] <> (FontStyle.getFontStyle FontStyle.Body3 LanguageStyle)
    , linearLayout
    [ height $ V 54
    , width MATCH_PARENT
    , background Color.white900
    , cornerRadius 5.0
    , gravity CENTER_VERTICAL
    , rippleColor Color.rippleShade
    , onClick push $ const (SelectLocation actionId)
    , stroke ("1," <> Color.borderColorLight)
    ][
      textView $ 
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , text $ if actionId == Src then 
                    if (DS.null state.data.srcLoc) then (getString STARTING_FROM) <> "?" else state.data.srcLoc
                  else 
                    if (DS.null state.data.destLoc) then (getString WHERE_TO) else state.data.destLoc
        , color Color.black800
        , gravity CENTER_VERTICAL
        , singleLine true
        , margin $ MarginHorizontal 20 10
        , alpha $ if actionId == Src then 
                    if (DS.null state.data.srcLoc) then 0.5 else 1.0
                  else 
                    if (DS.null state.data.destLoc) then 0.5 else 1.0
        ] <> (FontStyle.getFontStyle FontStyle.SubHeading1 LanguageStyle)
    ]
    ]

srcTextView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
srcTextView push state = textViewForLocation (getString FROM) Src push state

destTextView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketBookingScreenState -> PrestoDOM (Effect Unit) w
destTextView push state = textViewForLocation (getString TO) Dest push state
