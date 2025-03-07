{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroTicketBooking.ComponentConfig where

import Prelude
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton 
import Components.PrimaryEditText as PrimaryEditText
import PrestoDOM
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>))
import Common.Types.App(LazyCheck(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import Data.Maybe
import Font.Style as FontStyle
import JBridge as JB
import Screens.Types as ST
import Components.RequestInfoCard as InfoCard
import Language.Strings 
import Resources.Localizable.EN
import Language.Types


metroTicketBookingHeaderConfig :: ST.MetroTicketBookingScreenState -> GenericHeader.Config
metroTicketBookingHeaderConfig state = let
    config = GenericHeader.config
    genericHeaderConfig' = config 
        {
          height = WRAP_CONTENT
        , width = WRAP_CONTENT
        , prefixImageConfig {
           visibility = VISIBLE
          , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
          , height = V 25
          , width = V 25
          , margin = Margin 16 16 16 16
          } 
        , padding = PaddingVertical 5 5
        , textConfig {
            text = getString BUY_METRO_TICKETS
          , color = Color.darkCharcoal
          }
        , suffixImageConfig {
            visibility = GONE
          }
        }
    in genericHeaderConfig'

updateButtonConfig :: ST.MetroTicketBookingScreenState -> PrimaryButton.Config
updateButtonConfig state = let
    config = PrimaryButton.config
    updateButtonConfig' = config 
        { 
         textConfig{ text = if state.props.currentStage /= ST.MetroTicketSelection then ((getString PAY)<>" " <> " ₹" <> (show (state.data.ticketPrice * state.data.ticketCount )) )  else (getString GET_FARE)}
        , height = (V 48)
        , cornerRadius = 8.0
        , margin = (Margin 16 0 16 0)
        , id = "PrimaryButtonUpdate"
        , enableLoader = (JB.getBtnLoader "PrimaryButtonUpdate")
        , isClickable = state.props.termsAndConditionsSelected && state.props.isButtonActive
        , alpha = if (state.props.termsAndConditionsSelected && state.props.isButtonActive) then 1.0 else 0.5
        }
    in updateButtonConfig'



metroTimeErrorPopupConfig :: ST.MetroTicketBookingScreenState -> InfoCard.Config
metroTimeErrorPopupConfig state = let
  requestInfoCardConfig' =  InfoCard.config{
    title {
      text = getString METRO_BOOKING_TIMINGS,
      accessibilityHint = "Metro Booking Timings"
    }
  , primaryText {
      text = getString CHENNAI_METRO_TIME ,
      padding = Padding 16 16 0 0,
      textStyle = FontStyle.ParagraphText,
      color = Color.black700,
      accessibilityHint = "Chennai Metro allows QR ticket purchase from 4:30am to 22:30 PM on all days."
    }
  , secondaryText {
      text = getString PLEASE_COME_BACK_LATER_METRO,
      visibility = VISIBLE,
      padding = PaddingLeft 16,
      color = Color.black700,
      textStyle = FontStyle.ParagraphText,
      width = V $ JB.getWidthFromPercent 100,
      accessibilityHint = "Please come back later during the eligible time to purchase tickets."
    }
  , imageConfig {
      imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_red_warning_with_red_circle_bg",
      height = V 130,
      width = V 130,
      padding = Padding 0 2 2 0
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20,
      accessibilityHint = (getEN GOT_IT) <> " : Button"
    }
  }
  in requestInfoCardConfig'