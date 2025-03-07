{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReportIssueChatScreen.ComponentConfig where

import Prelude

import Components.AddAudioModel as AddAudioModel
import Components.AddImagesModel as AddImagesModel
import Components.ChatView.Controller as ChatView
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import Components.PrimaryEditText.Controller as PrimaryEditText
import Components.RecordAudioModel as RecordAudioModel
import Components.ViewImageModel as ViewImageModel
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth)
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Properties (singleLine)
import PrestoDOM.Types.DomAttributes (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types (ReportIssueChatScreenState)
import Styles.Colors (black500, black700, black900, primaryButtonColor, white900) as Color

primaryEditTextConfig :: ReportIssueChatScreenState -> PrimaryEditText.Config
primaryEditTextConfig _ = let
    primaryEditTextConfig' = PrimaryEditText.config
      { height = V 120
      , margin = Margin 0 0 0 0
      , id = getNewIDWithTag "submit_chat_edit_text"
      , editText
        { placeholder = getString $ REPORT_ISSUE_CHAT_PLACEHOLDER $ if EHC.os == "IOS" then "" else "REPORT_ISSUE_CHAT_PLACEHOLDER"
        , singleLine = false
        , pattern = Just "[^\n]*,255"
        }
      }
    in primaryEditTextConfig'

primaryButtonConfig :: String -> PrimaryButtonConfig.Config
primaryButtonConfig text = let
  primaryButtonConfig' = PrimaryButtonConfig.config
      {textConfig
      { text = text
      , color = Color.black700}
      , background = Color.white900
      , stroke = "1," <> Color.black500
      , width = V $ (screenWidth unit/2) - 30
      , id = "Button1"
      }
  in primaryButtonConfig'
    
doneButtonConfig :: ReportIssueChatScreenState -> PrimaryButton.Config
doneButtonConfig _ = let
    primaryButtonConfig' = PrimaryButton.config
      { textConfig
      { text = getString PLACE_CALL
      , color = Color.primaryButtonColor}
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 60
      , id = "doneButtonConfig"
      }
  in primaryButtonConfig'

cancelButtonConfig :: ReportIssueChatScreenState -> PrimaryButton.Config
cancelButtonConfig state = let
    primaryButtonConfig' = PrimaryButton.config
      { textConfig
      { text = getString CANCEL_BUTTON
      , color = Color.black700}
      , background = Color.white900
      , height = V 60
      , id = "cancelButtonConfig"
      }
  in primaryButtonConfig'

addAudioModelConfig :: ReportIssueChatScreenState -> AddAudioModel.AddAudioModelState
addAudioModelConfig state = let 
    addAudioModelConfig' =  AddAudioModel.config
      {
        addedVoiceNoteText = getString ADDED_VOICE_NOTE,
        noVoiceNoteAddedText = getString NO_VOICE_NOTE_ADDED,
        addVoiceNoteText = getString ADD_VOICE_NOTE,
        deleteText= getString DELETE,
        doneButtonText = getString DONE,
        audioFile = state.data.addAudioState.audioFile,
        stateChanged = state.data.addAudioState.stateChanged
      }
  in addAudioModelConfig'

recordAudioModelConfig :: ReportIssueChatScreenState -> RecordAudioModel.RecordAudioModelState
recordAudioModelConfig state = let 
    recordAudioModelConfig' = RecordAudioModel.config
      {
        recordVoiceNoteText = getString RECORD_VOICE_NOTE,
        timer = state.data.recordAudioState.timer,
        isRecording = state.data.recordAudioState.isRecording,
        isUploading = state.data.recordAudioState.isUploading,
        recordedFile = state.data.recordAudioState.recordedFile,
        recordingDone = state.data.recordAudioState.recordingDone,
        openAddAudioModel = state.data.recordAudioState.openAddAudioModel
      }
  in recordAudioModelConfig'

viewImageModelConfig :: ReportIssueChatScreenState -> ViewImageModel.ViewImageModelState
viewImageModelConfig state = let 
    viewImageModelConfig' = ViewImageModel.config
      {
        imagePreviewText = getString IMAGE_PREVIEW,
        image = state.data.viewImageState.image,
        imageName = state.data.viewImageState.imageName
      }
  in viewImageModelConfig'

addImageModelConfig :: ReportIssueChatScreenState -> AddImagesModel.AddImagesModelState
addImageModelConfig state = let 
    addImageModelConfig' = AddImagesModel.config
      {
        doneButtonText  = getString DONE,
        addedImagesText = getString ADDED_IMAGES,
        noImagesAddedText = getString NO_IMAGES_ADDED,
        viewText = getString VIEW,
        deleteText = getString DELETE,
        addAnotherText = getString ADD_ANOTHER,
        addImageText = getString ADD_IMAGE,
        images = state.data.addImagesState.images,
        stateChanged = state.data.addImagesState.stateChanged,
        isLoading = state.data.addImagesState.isLoading,
        imageMediaIds = state.data.addImagesState.imageMediaIds
      }
  in addImageModelConfig'

chatConfig :: ReportIssueChatScreenState -> ChatView.Config
chatConfig state = let 
    config = state.data.chatConfig
    chatConfig' = config
      {
        chatBodyPadding  = PaddingBottom if EHC.os == "IOS" then if state.props.isKeyboardOpen then 282 else 80 else 0
      , chatMargin =  MarginBottom if EHC.os == "IOS" then if state.props.showSubmitComp then 50 else 100 else 0
      }
  in chatConfig'
