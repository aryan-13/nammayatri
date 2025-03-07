window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "RC_VALIDATION_TEXT": "KL",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0",
    "APP_LINK": "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner",
    "USER_APP_LINK": "https://yatricustomer.page.link/pcJb",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8",
    "SPECIAL_ZONE_OTP_VIEW": "false",
    "StringKeys": ["NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "EARNED_ON_APP", "TRAVELLED_ON_APP", "REPORT_ISSUE_CHAT_PLACEHOLDER", "CORPORATE_ADDRESS", "CORPORATE_ADDRESS_DESCRIPTION", "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL", "REGISTERED_ADDRESS", "REGISTERED_ADDRESS_DESCRIPTION", "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL", "REFERRED_DRIVERS_INFO", "REFERRED_CUSTOMERS_INFO", "DOWNLOAD_NAMMA_YATRI"],
    "fontType": "Assets",
    "currency": "₹",
    "isGradient" : "false",
    "gradient": [],
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "popupBackground" : "#FFFFFF",
    "apiLoaderLottie": "primary_button_loader.json",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "showCorporateAddress" : false,
    "imageUploadOptional" : true,
    "showPaymentDetails" : false,
    "enableDriverReferral": false,
    "enableCustomerReferral": true,
    "BONUS_EARNED" : "false",
    "clientName" : "Yatri",
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subtitle": "ഇംഗ്ലീഷ്"
    },
    {
      "name": "മലയാളം",
      "value": "ML_IN",
      "subtitle": "Malayalam"
    }
    ],
    "engilshInNative" : "ഇംഗ്ലീഷ്",
    "englishStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "Yatri Driver collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently,We allow only Kerala registered number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "EARNED_ON_APP" : "Earned on Yatri",
      "TRAVELLED_ON_APP" : "Travelled On Yatri",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri will try to resolve it in under 24 hours.",
      "CORPORATE_ADDRESS" : "Corporate Address",
      "CORPORATE_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Girija Building, Number 817, Ganapathi Temple Rd, 8th Block, Koramangala, Bengaluru, Karnataka 560095, India.",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" :  "Website: <u>https://www.getyatri.com</u>",
      "REGISTERED_ADDRESS" : "Registered Address",
      "REGISTERED_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Stallion Business Centre, No. 444, 3rd & 4th Floor, 18th Main, 6th Block, Koramangala Bengaluru, Karnataka- 560095, India.",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://www.getyatri.com</u>",
      "DOWNLOAD_NAMMA_YATRI": "Download Yatri",
      "REFERRED_DRIVERS_INFO" : "Referred Drivers who have registered on Yatri",
      "REFERRED_CUSTOMERS_INFO" : "Referred Customers who have registered on Yatri",
    },
    "malayalamStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "ആപ്പ് അടച്ചിരിക്കുമ്പോഴും ഉപയോഗത്തിലില്ലെങ്കിലും ഡ്രൈവർ നിലവിലെ ലൊക്കേഷൻ നിരീക്ഷിക്കാൻ നിങ്ങളുടെ ലൊക്കേഷൻ പങ്കിടുന്നത് പ്രവർത്തനക്ഷമമാക്കാൻ യാത്രി പങ്കാളി ലൊക്കേഷൻ ഡാറ്റ ശേഖരിക്കുന്നു.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "നിലവിൽ കേരളത്തിൽ രജിസ്റ്റർ ചെയ്ത നമ്പർ മാത്രമേ ഞങ്ങൾ അനുവദിക്കൂ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "നിങ്ങളുടെ ലൊക്കേഷൻ ഞങ്ങളുടെ സിസ്റ്റത്തെ ടാക്സികൾ വഴി മാപ്പ് ചെയ്യാൻ സഹായിക്കുന്നു.",
      "EARNED_ON_APP" : "Y-ൽ നേടിയ വില",
      "TRAVELLED_ON_APP" : "യാത്രയിൽ യാത്ര ചെയ്ത ദൂരം",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "നിങ്ങളുടെ പ്രശ്നം വിവരിക്കുക. 24 മണിക്കൂറിനുള്ളിൽ അത് പരിഹരിക്കാൻ യാത്രി ശ്രമിക്കും.",
      "CORPORATE_ADDRESS" : "കോർപ്പറേറ്റ് വിലാസം",
      "CORPORATE_ADDRESS_DESCRIPTION" : "ജസ്‌പേ ടെക്‌നോളജീസ് പ്രൈവറ്റ് ലിമിറ്റഡ് <br> ഗിരിജ ബിൽഡിംഗ്, നമ്പർ 817, ഗണപതി ടെംപിൾ റോഡ്, എട്ടാം ബ്ലോക്ക്, കോറമംഗല, ബെംഗളൂരു, കർണാടക 560095, ഇന്ത്യ",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" :  "വെബ്സൈറ്റ്: <u>https://www.getyatri.com</u>",
      "REGISTERED_ADDRESS" : "രേഖപ്പെടുത്തിയ വിലാസം",
      "REGISTERED_ADDRESS_DESCRIPTION" : "ജസ്‌പേ ടെക്‌നോളജീസ് പ്രൈവറ്റ് ലിമിറ്റഡ് <br> സ്റ്റാലിയൻ ബിസിനസ് സെന്റർ, നമ്പർ 444, 3rd & 4th നിലകൾ, 18th മെയിൻ, 6th ബ്ലോക്ക്, കോറമംഗല ബെംഗളൂരു, കർണാടക- 560095, ഇന്ത്യ.",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "വെബ്സൈറ്റ്: <u>https://www.getyatri.com</u>",
      "DOWNLOAD_NAMMA_YATRI": "യാത്രാ ഡൗൺലോഡുചെയ്യുക",
      "REFERRED_DRIVERS_INFO": "യാത്രിയിൽ രജിസ്റ്റർ ചെയ്ത പരാമർശിച്ച ഡ്രൈവേഴ്സ്",
      "REFERRED_CUSTOMERS_INFO": "യാത്രിയിൽ രജിസ്റ്റർ ചെയ്ത പരാമർശിച്ച കസ്റ്റമേഴ്സ്",
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "fontName" : "PlusJakartaSans"
    , "fontKannada" : "NotoSansKannada"
    , "allowAllMobileNumber" : false
    , "showGenderBanner" : false
    , "defaultLanguage" : "EN_US"
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0"
    , "termsVersion" : 1.0
    , "privacyLink" : "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8"
    , "feature" : {
      "enableBonus" : false
      , "enableImageUpload" : true
      , "enableGender" : false
      , "enableOtpRide" : false
    }
    , "leaderBoard": {
      "isMaskedName": false
    }
    , "appData" : {
      "link" : "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner"
      , "name" : "Yatri"
    }
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+"
    , "autoPayBanner" : false
    , "referralType" : "QRScreen"
    , "enableMockLocation" : false
    , "vehicle" : {
      "validationPrefix" :  "KL"
    }
    , "banners" :{
      "autoPay" : false
    }
    , "referral": {
      "type": "LeaderBoard",
      "link" : "https://yatricustomer.page.link/pcJb",
      "customerAppId" : "net.openkochi.yatri",
      "driverAppId" : "net.openkochi.yatripartner"
    }
    , "flowConfig" : {
      "chooseCity" : {
        "runFlow" : false
      }
    }
    , "permissions" : {
      "locationPermission" : true,
      "notification" : true
    }
    , "bottomNavConfig" : {
      "subscription" :
        { "isVisible" : false
        },
        "referral" : 
        { 
          "showNew" : true
        }
    }
  , "profile" : {
      "checkRCStatusForBookingOption" : false
    }
  })
}