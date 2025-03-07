module Types.DBSync.Update where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.Beam.Postgres
import EulerHS.Prelude
import qualified IssueManagement.Storage.Beam.Issue.Comment as Comment
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as IssueCategory
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as IssueOption
import qualified IssueManagement.Storage.Beam.Issue.IssueReport as IssueReport
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as IssueTranslation
import qualified IssueManagement.Storage.Beam.MediaFile as MediaFile
import qualified Lib.Payment.Storage.Beam.PaymentOrder as PaymentOrder
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as PaymentTransaction
import Sequelize
import qualified "dynamic-offer-driver-app" Storage.Beam.BapMetadata as BapMetadata
import qualified "dynamic-offer-driver-app" Storage.Beam.BecknRequest as BecknRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.Booking as Booking
import qualified "dynamic-offer-driver-app" Storage.Beam.BookingCancellationReason as BookingCancellationReason
import qualified "dynamic-offer-driver-app" Storage.Beam.BusinessEvent as BusinessEvent
import qualified "dynamic-offer-driver-app" Storage.Beam.CallStatus as CallStatus
import qualified "dynamic-offer-driver-app" Storage.Beam.CancellationReason as CancellationReason
import qualified "dynamic-offer-driver-app" Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest as DriverGoHomeRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.Driver.GoHomeFeature.DriverHomeLocation as DriverHomeLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverBlockReason as DriverBlockReason
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverFee as DriverFee
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverInformation as DriverInformation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.AadhaarOtpReq as AadhaarOtpReq
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.AadhaarOtpVerify as AadhaarOtpVerify
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.AadhaarVerification as AadhaarVerification
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.DriverLicense as DriverLicense
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.DriverRCAssociation as DriverRCAssociation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.IdfyVerification as IdfyVerification
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.Image as Image
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as VehicleRegistrationCertificate
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverPlan as DriverPlan
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverPoolConfig as DriverPoolConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverQuote as DriverQuote
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverReferral as DriverReferral
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverStats as DriverStats
import qualified "dynamic-offer-driver-app" Storage.Beam.Estimate as Estimate
import qualified "dynamic-offer-driver-app" Storage.Beam.Exophone as Exophone
import qualified "dynamic-offer-driver-app" Storage.Beam.FareParameters as FareParameters
import qualified "dynamic-offer-driver-app" Storage.Beam.FareParameters.FareParametersProgressiveDetails as FareParametersProgressiveDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.FareParameters.FareParametersSlabDetails as FareParametersSlabDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy as FarePolicy
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.DriverExtraFeeBounds as DriverExtraFeeBounds
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as FarePolicyProgressiveDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as FarePolicyProgressiveDetailsPerExtraKmRateSection
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as FarePolicySlabDetailsSlab
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.RestrictedExtraFare as RestrictedExtraFare
import qualified "dynamic-offer-driver-app" Storage.Beam.FareProduct as FareProduct
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.Feedback as Feedback
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.FeedbackBadge as FeedbackBadge
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.FeedbackForm as FeedbackForm
import qualified "dynamic-offer-driver-app" Storage.Beam.FleetDriverAssociation as FleetDriverAssociation
import qualified "dynamic-offer-driver-app" Storage.Beam.Geometry as Geometry
import qualified "dynamic-offer-driver-app" Storage.Beam.GoHomeConfig as GoHomeConfig
import "dynamic-offer-driver-app" Storage.Beam.IssueManagement ()
import qualified "dynamic-offer-driver-app" Storage.Beam.Location as Location
import qualified "dynamic-offer-driver-app" Storage.Beam.LocationMapping as LocationMapping
import qualified "dynamic-offer-driver-app" Storage.Beam.Mandate as Mandate
import qualified "dynamic-offer-driver-app" Storage.Beam.Maps.PlaceNameCache as PlaceNameCache
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant as Merchant
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.DriverIntelligentPoolConfig as DriverIntelligentPoolConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.LeaderBoardConfig as MerchantLeaderBoardConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantMessage as MerchantMessage
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantPaymentMethod as MerchantPaymentMethod
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantServiceConfig as MerchantServiceConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantServiceUsageConfig as MerchantServiceUsageConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.OnboardingDocumentConfig as MerchantOnboardingDocumentConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.TransporterConfig as TransporterConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.Message as Message
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.MessageReport as MessageReport
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.MessageTranslation as MessageTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.MetaData as MetaData
import "dynamic-offer-driver-app" Storage.Beam.Payment ()
import qualified "dynamic-offer-driver-app" Storage.Beam.Person as Person
import qualified "dynamic-offer-driver-app" Storage.Beam.Quote as Quote
import qualified "dynamic-offer-driver-app" Storage.Beam.Rating as Rating
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistryMapFallback as RegistryMapFallback
import qualified "dynamic-offer-driver-app" Storage.Beam.Ride.Table as Ride
import qualified "dynamic-offer-driver-app" Storage.Beam.RideDetails as RideDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.RiderDetails as RiderDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestForDriver as SearchRequestForDriver
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchTry as SearchTry
import qualified "dynamic-offer-driver-app" Storage.Beam.Vehicle as Vehicle
import qualified "dynamic-offer-driver-app" Storage.Beam.Volunteer as Volunteer
import Utils.Parse

-- Each update option contains a list of (key, value) pairs to set during
-- the update and a list of (key, value) pairs to use as a where clause.
data UpdateModel
  = BapMetadataUpdate
  | BookingUpdate
  | BookingCancellationReasonUpdate
  | BusinessEventUpdate
  | CallStatusUpdate
  | CancellationReasonUpdate
  | DriverBlockReasonUpdate
  | FleetDriverAssociationUpdate
  | DriverFeeUpdate
  | DriverInformationUpdate
  | AadhaarOtpReqUpdate
  | AadhaarOtpVerifyUpdate
  | AadhaarVerificationUpdate
  | DriverLicenseUpdate
  | DriverRcAssociationUpdate
  | DriverPlanUpdate
  | IdfyVerificationUpdate
  | ImageUpdate
  | VehicleRegistrationCertificateUpdate
  | DriverQuoteUpdate
  | DriverReferralUpdate
  | DriverStatsUpdate
  | EstimateUpdate
  | ExophoneUpdate
  | FareParametersUpdate
  | FareParametersProgressiveDetailsUpdate
  | FareParametersSlabDetailsUpdate
  | FarePolicyUpdate
  | DriverExtraFeeBoundsUpdate
  | FarePolicyProgressiveDetailsUpdate
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate
  | FarePolicySlabDetailsSlabUpdate
  | RestrictedExtraFareUpdate
  | FareProductUpdate
  | GeometryUpdate
  | CommentUpdate
  | IssueCategoryUpdate
  | IssueOptionUpdate
  | IssueReportUpdate
  | IssueTranslationUpdate
  | PlaceNameCacheUpdate
  | MediaFileUpdate
  | MerchantUpdate
  | DriverIntelligentPoolConfigUpdate
  | DriverPoolConfigUpdate
  | MerchantLeaderBoardConfigUpdate
  | MandateUpdate
  | MerchantMessageUpdate
  | MerchantPaymentMethodUpdate
  | MerchantServiceConfigUpdate
  | MerchantServiceUsageConfigUpdate
  | MerchantOnboardingDocumentConfigUpdate
  | TransporterConfigUpdate
  | MessageUpdate
  | MessageReportUpdate
  | MessageTranslationUpdate
  | MetaDataUpdate
  | PersonUpdate
  | QuoteSpecialZoneUpdate
  | RatingUpdate
  | RegistrationTokenUpdate
  | RideUpdate
  | RideDetailsUpdate
  | RiderDetailsUpdate
  | SearchRequestUpdate
  | SearchRequestForDriverUpdate
  | SearchTryUpdate
  | VehicleUpdate
  | VolunteerUpdate
  | FeedbackFormUpdate
  | FeedbackUpdate
  | FeedbackBadgeUpdate
  | BecknRequestUpdate
  | RegistryMapFallbackUpdate
  | DriverGoHomeRequestUpdate
  | DriverHomeLocationUpdate
  | GoHomeConfigUpdate
  | LocationUpdate
  | LocationMappingUpdate
  | PaymentOrderUpdate
  | PaymentTransactionUpdate
  deriving (Generic, Show)

getTagUpdate :: UpdateModel -> Text
getTagUpdate BapMetadataUpdate = "BapMetadataOptions"
getTagUpdate BookingUpdate = "BookingOptions"
getTagUpdate BookingCancellationReasonUpdate = "BookingCancellationReasonOptions"
getTagUpdate BusinessEventUpdate = "BusinessEventOptions"
getTagUpdate CallStatusUpdate = "CallStatusOptions"
getTagUpdate CancellationReasonUpdate = "CancellationReasonOptions"
getTagUpdate DriverBlockReasonUpdate = "DriverBlockReasonOptions"
getTagUpdate FleetDriverAssociationUpdate = "FleetDriverAssociationOptions"
getTagUpdate DriverFeeUpdate = "DriverFeeOptions"
getTagUpdate DriverInformationUpdate = "DriverInformationOptions"
getTagUpdate DriverPlanUpdate = "DriverPlanOptions"
getTagUpdate AadhaarOtpReqUpdate = "AadhaarOtpReqOptions"
getTagUpdate AadhaarOtpVerifyUpdate = "AadhaarOtpVerifyOptions"
getTagUpdate AadhaarVerificationUpdate = "AadhaarVerificationOptions"
getTagUpdate DriverLicenseUpdate = "DriverLicenseOptions"
getTagUpdate DriverRcAssociationUpdate = "DriverRcAssociationOptions"
getTagUpdate IdfyVerificationUpdate = "IdfyVerificationOptions"
getTagUpdate ImageUpdate = "ImageOptions"
getTagUpdate VehicleRegistrationCertificateUpdate = "VehicleRegistrationCertificateOptions"
getTagUpdate DriverQuoteUpdate = "DriverQuoteOptions"
getTagUpdate DriverReferralUpdate = "DriverReferralOptions"
getTagUpdate DriverStatsUpdate = "DriverStatsOptions"
getTagUpdate EstimateUpdate = "EstimateOptions"
getTagUpdate ExophoneUpdate = "ExophoneOptions"
getTagUpdate FareParametersUpdate = "FareParametersOptions"
getTagUpdate FareParametersProgressiveDetailsUpdate = "FareParametersProgressiveDetailsOptions"
getTagUpdate FareParametersSlabDetailsUpdate = "FareParametersSlabDetailsOptions"
getTagUpdate FarePolicyUpdate = "FarePolicyOptions"
getTagUpdate DriverExtraFeeBoundsUpdate = "DriverExtraFeeBoundsOptions"
getTagUpdate FarePolicyProgressiveDetailsUpdate = "FarePolicyProgressiveDetailsOptions"
getTagUpdate FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate = "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions"
getTagUpdate FarePolicySlabDetailsSlabUpdate = "FarePolicySlabDetailsSlabOptions"
getTagUpdate RestrictedExtraFareUpdate = "RestrictedExtraFareOptions"
getTagUpdate FareProductUpdate = "FareProductOptions"
getTagUpdate GeometryUpdate = "GeometryOptions"
getTagUpdate CommentUpdate = "CommentOptions"
getTagUpdate IssueCategoryUpdate = "IssueCategoryOptions"
getTagUpdate IssueOptionUpdate = "IssueOptionOptions"
getTagUpdate IssueReportUpdate = "IssueReportOptions"
getTagUpdate IssueTranslationUpdate = "IssueTranslationOptions"
getTagUpdate PlaceNameCacheUpdate = "PlaceNameCacheOptions"
getTagUpdate MandateUpdate = "MandateOptions"
getTagUpdate MediaFileUpdate = "MediaFileOptions"
getTagUpdate MerchantUpdate = "MerchantOptions"
getTagUpdate DriverIntelligentPoolConfigUpdate = "DriverIntelligentPoolConfigOptions"
getTagUpdate DriverPoolConfigUpdate = "DriverPoolConfigOptions"
getTagUpdate MerchantLeaderBoardConfigUpdate = "MerchantLeaderBoardConfigOptions"
getTagUpdate MerchantMessageUpdate = "MerchantMessageOptions"
getTagUpdate MerchantPaymentMethodUpdate = "MerchantPaymentMethodOptions"
getTagUpdate MerchantServiceConfigUpdate = "MerchantServiceConfigOptions"
getTagUpdate MerchantServiceUsageConfigUpdate = "MerchantServiceUsageConfigOptions"
getTagUpdate MerchantOnboardingDocumentConfigUpdate = "MerchantOnboardingDocumentConfigOptions"
getTagUpdate TransporterConfigUpdate = "TransporterConfigOptions"
getTagUpdate MessageUpdate = "MessageOptions"
getTagUpdate MessageReportUpdate = "MessageReportOptions"
getTagUpdate MessageTranslationUpdate = "MessageTranslationOptions"
getTagUpdate MetaDataUpdate = "MetaDataOptions"
getTagUpdate PersonUpdate = "PersonOptions"
getTagUpdate QuoteSpecialZoneUpdate = "QuoteSpecialZoneOptions"
getTagUpdate RatingUpdate = "RatingOptions"
getTagUpdate RegistrationTokenUpdate = "RegistrationTokenOptions"
getTagUpdate RideUpdate = "RideOptions"
getTagUpdate RideDetailsUpdate = "RideDetailsOptions"
getTagUpdate RiderDetailsUpdate = "RiderDetailsOptions"
getTagUpdate SearchRequestUpdate = "SearchRequestOptions"
getTagUpdate SearchRequestForDriverUpdate = "SearchRequestForDriverOptions"
getTagUpdate SearchTryUpdate = "SearchTryOptions"
getTagUpdate VehicleUpdate = "VehicleOptions"
getTagUpdate VolunteerUpdate = "VolunteerOptions"
getTagUpdate FeedbackFormUpdate = "FeedbackFormOptions"
getTagUpdate FeedbackUpdate = "FeedbackOptions"
getTagUpdate FeedbackBadgeUpdate = "FeedbackBadgeOptions"
getTagUpdate BecknRequestUpdate = "BecknRequestOptions"
getTagUpdate RegistryMapFallbackUpdate = "RegistryMapFallbackOptions"
getTagUpdate DriverGoHomeRequestUpdate = "DriverGoHomeRequestOptions"
getTagUpdate DriverHomeLocationUpdate = "DriverHomeLocationOptions"
getTagUpdate GoHomeConfigUpdate = "GoHomeConfigOptions"
getTagUpdate LocationUpdate = "LocationOptions"
getTagUpdate LocationMappingUpdate = "LocationMappingOptions"
getTagUpdate PaymentOrderUpdate = "PaymentOrderOptions"
getTagUpdate PaymentTransactionUpdate = "PaymentTransactionOptions"

parseTagUpdate :: Text -> Parser UpdateModel
parseTagUpdate "BapMetadataOptions" = return BapMetadataUpdate
parseTagUpdate "BookingOptions" = return BookingUpdate
parseTagUpdate "BookingCancellationReasonOptions" = return BookingCancellationReasonUpdate
parseTagUpdate "BusinessEventOptions" = return BusinessEventUpdate
parseTagUpdate "CallStatusOptions" = return CallStatusUpdate
parseTagUpdate "CancellationReasonOptions" = return CancellationReasonUpdate
parseTagUpdate "DriverBlockReasonOptions" = return DriverBlockReasonUpdate
parseTagUpdate "DriverPlanOptions" = return DriverPlanUpdate
parseTagUpdate "FleetDriverAssociationOptions" = return FleetDriverAssociationUpdate
parseTagUpdate "DriverFeeOptions" = return DriverFeeUpdate
parseTagUpdate "DriverInformationOptions" = return DriverInformationUpdate
parseTagUpdate "AadhaarOtpReqOptions" = return AadhaarOtpReqUpdate
parseTagUpdate "AadhaarOtpVerifyOptions" = return AadhaarOtpVerifyUpdate
parseTagUpdate "AadhaarVerificationOptions" = return AadhaarVerificationUpdate
parseTagUpdate "DriverLicenseOptions" = return DriverLicenseUpdate
parseTagUpdate "DriverRcAssociationOptions" = return DriverRcAssociationUpdate
parseTagUpdate "IdfyVerificationOptions" = return IdfyVerificationUpdate
parseTagUpdate "ImageOptions" = return ImageUpdate
parseTagUpdate "VehicleRegistrationCertificateOptions" = return VehicleRegistrationCertificateUpdate
parseTagUpdate "DriverQuoteOptions" = return DriverQuoteUpdate
parseTagUpdate "DriverReferralOptions" = return DriverReferralUpdate
parseTagUpdate "DriverStatsOptions" = return DriverStatsUpdate
parseTagUpdate "EstimateOptions" = return EstimateUpdate
parseTagUpdate "ExophoneOptions" = return ExophoneUpdate
parseTagUpdate "FareParametersOptions" = return FareParametersUpdate
parseTagUpdate "FareParametersProgressiveDetailsOptions" = return FareParametersProgressiveDetailsUpdate
parseTagUpdate "FareParametersSlabDetailsOptions" = return FareParametersSlabDetailsUpdate
parseTagUpdate "FarePolicyOptions" = return FarePolicyUpdate
parseTagUpdate "DriverExtraFeeBoundsOptions" = return DriverExtraFeeBoundsUpdate
parseTagUpdate "FarePolicyProgressiveDetailsOptions" = return FarePolicyProgressiveDetailsUpdate
parseTagUpdate "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions" = return FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate
parseTagUpdate "FarePolicySlabDetailsSlabOptions" = return FarePolicySlabDetailsSlabUpdate
parseTagUpdate "RestrictedExtraFareOptions" = return RestrictedExtraFareUpdate
parseTagUpdate "FareProductOptions" = return FareProductUpdate
parseTagUpdate "GeometryOptions" = return GeometryUpdate
parseTagUpdate "CommentOptions" = return CommentUpdate
parseTagUpdate "IssueCategoryOptions" = return IssueCategoryUpdate
parseTagUpdate "IssueOptionOptions" = return IssueOptionUpdate
parseTagUpdate "IssueReportOptions" = return IssueReportUpdate
parseTagUpdate "IssueTranslationOptions" = return IssueTranslationUpdate
parseTagUpdate "PlaceNameCacheOptions" = return PlaceNameCacheUpdate
parseTagUpdate "MandateOptions" = return MandateUpdate
parseTagUpdate "MediaFileOptions" = return MediaFileUpdate
parseTagUpdate "MerchantOptions" = return MerchantUpdate
parseTagUpdate "DriverIntelligentPoolConfigOptions" = return DriverIntelligentPoolConfigUpdate
parseTagUpdate "DriverPoolConfigOptions" = return DriverPoolConfigUpdate
parseTagUpdate "MerchantLeaderBoardConfigOptions" = return MerchantLeaderBoardConfigUpdate
parseTagUpdate "MerchantMessageOptions" = return MerchantMessageUpdate
parseTagUpdate "MerchantPaymentMethodOptions" = return MerchantPaymentMethodUpdate
parseTagUpdate "MerchantServiceConfigOptions" = return MerchantServiceConfigUpdate
parseTagUpdate "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigUpdate
parseTagUpdate "MerchantOnboardingDocumentConfigOptions" = return MerchantOnboardingDocumentConfigUpdate
parseTagUpdate "TransporterConfigOptions" = return TransporterConfigUpdate
parseTagUpdate "MessageOptions" = return MessageUpdate
parseTagUpdate "MessageReportOptions" = return MessageReportUpdate
parseTagUpdate "MessageTranslationOptions" = return MessageTranslationUpdate
parseTagUpdate "MetaDataOptions" = return MetaDataUpdate
parseTagUpdate "PersonOptions" = return PersonUpdate
parseTagUpdate "QuoteSpecialZoneOptions" = return QuoteSpecialZoneUpdate
parseTagUpdate "RatingOptions" = return RatingUpdate
parseTagUpdate "RegistrationTokenOptions" = return RegistrationTokenUpdate
parseTagUpdate "RideOptions" = return RideUpdate
parseTagUpdate "RideDetailsOptions" = return RideDetailsUpdate
parseTagUpdate "RiderDetailsOptions" = return RiderDetailsUpdate
parseTagUpdate "SearchRequestOptions" = return SearchRequestUpdate
parseTagUpdate "SearchRequestForDriverOptions" = return SearchRequestForDriverUpdate
parseTagUpdate "SearchTryOptions" = return SearchTryUpdate
parseTagUpdate "VehicleOptions" = return VehicleUpdate
parseTagUpdate "VolunteerOptions" = return VolunteerUpdate
parseTagUpdate "FeedbackFormOptions" = return FeedbackFormUpdate
parseTagUpdate "FeedbackOptions" = return FeedbackUpdate
parseTagUpdate "FeedbackBadgeOptions" = return FeedbackBadgeUpdate
parseTagUpdate "BecknRequestOptions" = return BecknRequestUpdate
parseTagUpdate "RegistryMapFallbackOptions" = return RegistryMapFallbackUpdate
parseTagUpdate "DriverGoHomeRequestOptions" = return DriverGoHomeRequestUpdate
parseTagUpdate "DriverHomeLocationOptions" = return DriverHomeLocationUpdate
parseTagUpdate "GoHomeConfigOptions" = return GoHomeConfigUpdate
parseTagUpdate "LocationOptions" = return LocationUpdate
parseTagUpdate "LocationMappingOptions" = return LocationMappingUpdate
parseTagUpdate "PaymentOrderOptions" = return PaymentOrderUpdate
parseTagUpdate "PaymentTransactionOptions" = return PaymentTransactionUpdate
parseTagUpdate t = fail $ T.unpack ("Expected a UpdateModel but got '" <> t <> "'")

data DBUpdateObject
  = BapMetadataOptions UpdateModel [Set Postgres BapMetadata.BapMetadataT] (Where Postgres BapMetadata.BapMetadataT)
  | BookingOptions UpdateModel [Set Postgres Booking.BookingT] (Where Postgres Booking.BookingT)
  | BookingCancellationReasonOptions UpdateModel [Set Postgres BookingCancellationReason.BookingCancellationReasonT] (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | BusinessEventOptions UpdateModel [Set Postgres BusinessEvent.BusinessEventT] (Where Postgres BusinessEvent.BusinessEventT)
  | CallStatusOptions UpdateModel [Set Postgres CallStatus.CallStatusT] (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonOptions UpdateModel [Set Postgres CancellationReason.CancellationReasonT] (Where Postgres CancellationReason.CancellationReasonT)
  | DriverBlockReasonOptions UpdateModel [Set Postgres DriverBlockReason.DriverBlockReasonT] (Where Postgres DriverBlockReason.DriverBlockReasonT)
  | FleetDriverAssociationOptions UpdateModel [Set Postgres FleetDriverAssociation.FleetDriverAssociationT] (Where Postgres FleetDriverAssociation.FleetDriverAssociationT)
  | DriverFeeOptions UpdateModel [Set Postgres DriverFee.DriverFeeT] (Where Postgres DriverFee.DriverFeeT)
  | DriverInformationOptions UpdateModel [Set Postgres DriverInformation.DriverInformationT] (Where Postgres DriverInformation.DriverInformationT)
  | DriverPlanOptions UpdateModel [Set Postgres DriverPlan.DriverPlanT] (Where Postgres DriverPlan.DriverPlanT)
  | AadhaarOtpReqOptions UpdateModel [Set Postgres AadhaarOtpReq.AadhaarOtpReqT] (Where Postgres AadhaarOtpReq.AadhaarOtpReqT)
  | AadhaarOtpVerifyOptions UpdateModel [Set Postgres AadhaarOtpVerify.AadhaarOtpVerifyT] (Where Postgres AadhaarOtpVerify.AadhaarOtpVerifyT)
  | AadhaarVerificationOptions UpdateModel [Set Postgres AadhaarVerification.AadhaarVerificationT] (Where Postgres AadhaarVerification.AadhaarVerificationT)
  | DriverLicenseOptions UpdateModel [Set Postgres DriverLicense.DriverLicenseT] (Where Postgres DriverLicense.DriverLicenseT)
  | DriverRcAssociationOptions UpdateModel [Set Postgres DriverRCAssociation.DriverRCAssociationT] (Where Postgres DriverRCAssociation.DriverRCAssociationT)
  | IdfyVerificationOptions UpdateModel [Set Postgres IdfyVerification.IdfyVerificationT] (Where Postgres IdfyVerification.IdfyVerificationT)
  | ImageOptions UpdateModel [Set Postgres Image.ImageT] (Where Postgres Image.ImageT)
  | VehicleRegistrationCertificateOptions UpdateModel [Set Postgres VehicleRegistrationCertificate.VehicleRegistrationCertificateT] (Where Postgres VehicleRegistrationCertificate.VehicleRegistrationCertificateT)
  | DriverQuoteOptions UpdateModel [Set Postgres DriverQuote.DriverQuoteT] (Where Postgres DriverQuote.DriverQuoteT)
  | DriverReferralOptions UpdateModel [Set Postgres DriverReferral.DriverReferralT] (Where Postgres DriverReferral.DriverReferralT)
  | DriverStatsOptions UpdateModel [Set Postgres DriverStats.DriverStatsT] (Where Postgres DriverStats.DriverStatsT)
  | EstimateOptions UpdateModel [Set Postgres Estimate.EstimateT] (Where Postgres Estimate.EstimateT)
  | ExophoneOptions UpdateModel [Set Postgres Exophone.ExophoneT] (Where Postgres Exophone.ExophoneT)
  | FareParametersOptions UpdateModel [Set Postgres FareParameters.FareParametersT] (Where Postgres FareParameters.FareParametersT)
  | FareParametersProgressiveDetailsOptions UpdateModel [Set Postgres FareParametersProgressiveDetails.FareParametersProgressiveDetailsT] (Where Postgres FareParametersProgressiveDetails.FareParametersProgressiveDetailsT)
  | FareParametersSlabDetailsOptions UpdateModel [Set Postgres FareParametersSlabDetails.FareParametersSlabDetailsT] (Where Postgres FareParametersSlabDetails.FareParametersSlabDetailsT)
  | FarePolicyOptions UpdateModel [Set Postgres FarePolicy.FarePolicyT] (Where Postgres FarePolicy.FarePolicyT)
  | DriverExtraFeeBoundsOptions UpdateModel [Set Postgres DriverExtraFeeBounds.DriverExtraFeeBoundsT] (Where Postgres DriverExtraFeeBounds.DriverExtraFeeBoundsT)
  | FarePolicyProgressiveDetailsOptions UpdateModel [Set Postgres FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT] (Where Postgres FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT)
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions UpdateModel [Set Postgres FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSectionT] (Where Postgres FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSectionT)
  | FarePolicySlabDetailsSlabOptions UpdateModel [Set Postgres FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlabT] (Where Postgres FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlabT)
  | RestrictedExtraFareOptions UpdateModel [Set Postgres RestrictedExtraFare.RestrictedExtraFareT] (Where Postgres RestrictedExtraFare.RestrictedExtraFareT)
  | FareProductOptions UpdateModel [Set Postgres FareProduct.FareProductT] (Where Postgres FareProduct.FareProductT)
  | GeometryOptions UpdateModel [Set Postgres Geometry.GeometryT] (Where Postgres Geometry.GeometryT)
  | CommentOptions UpdateModel [Set Postgres Comment.CommentT] (Where Postgres Comment.CommentT)
  | IssueCategoryOptions UpdateModel [Set Postgres IssueCategory.IssueCategoryT] (Where Postgres IssueCategory.IssueCategoryT)
  | IssueOptionOptions UpdateModel [Set Postgres IssueOption.IssueOptionT] (Where Postgres IssueOption.IssueOptionT)
  | IssueReportOptions UpdateModel [Set Postgres IssueReport.IssueReportT] (Where Postgres IssueReport.IssueReportT)
  | IssueTranslationOptions UpdateModel [Set Postgres IssueTranslation.IssueTranslationT] (Where Postgres IssueTranslation.IssueTranslationT)
  | PlaceNameCacheOptions UpdateModel [Set Postgres PlaceNameCache.PlaceNameCacheT] (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MandateOptions UpdateModel [Set Postgres Mandate.MandateT] (Where Postgres Mandate.MandateT)
  | MediaFileOptions UpdateModel [Set Postgres MediaFile.MediaFileT] (Where Postgres MediaFile.MediaFileT)
  | MerchantOptions UpdateModel [Set Postgres Merchant.MerchantT] (Where Postgres Merchant.MerchantT)
  | DriverIntelligentPoolConfigOptions UpdateModel [Set Postgres DriverIntelligentPoolConfig.DriverIntelligentPoolConfigT] (Where Postgres DriverIntelligentPoolConfig.DriverIntelligentPoolConfigT)
  | DriverPoolConfigOptions UpdateModel [Set Postgres DriverPoolConfig.DriverPoolConfigT] (Where Postgres DriverPoolConfig.DriverPoolConfigT)
  | MerchantLeaderBoardConfigOptions UpdateModel [Set Postgres MerchantLeaderBoardConfig.LeaderBoardConfigsT] (Where Postgres MerchantLeaderBoardConfig.LeaderBoardConfigsT)
  | MerchantMessageOptions UpdateModel [Set Postgres MerchantMessage.MerchantMessageT] (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodOptions UpdateModel [Set Postgres MerchantPaymentMethod.MerchantPaymentMethodT] (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigOptions UpdateModel [Set Postgres MerchantServiceConfig.MerchantServiceConfigT] (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigOptions UpdateModel [Set Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT] (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantOnboardingDocumentConfigOptions UpdateModel [Set Postgres MerchantOnboardingDocumentConfig.OnboardingDocumentConfigT] (Where Postgres MerchantOnboardingDocumentConfig.OnboardingDocumentConfigT)
  | TransporterConfigOptions UpdateModel [Set Postgres TransporterConfig.TransporterConfigT] (Where Postgres TransporterConfig.TransporterConfigT)
  | MessageOptions UpdateModel [Set Postgres Message.MessageT] (Where Postgres Message.MessageT)
  | MessageReportOptions UpdateModel [Set Postgres MessageReport.MessageReportT] (Where Postgres MessageReport.MessageReportT)
  | MessageTranslationOptions UpdateModel [Set Postgres MessageTranslation.MessageTranslationT] (Where Postgres MessageTranslation.MessageTranslationT)
  | MetaDataOptions UpdateModel [Set Postgres MetaData.MetaDataT] (Where Postgres MetaData.MetaDataT)
  | PersonOptions UpdateModel [Set Postgres Person.PersonT] (Where Postgres Person.PersonT)
  | QuoteSpecialZoneOptions UpdateModel [Set Postgres Quote.QuoteSpecialZoneT] (Where Postgres Quote.QuoteSpecialZoneT)
  | RatingOptions UpdateModel [Set Postgres Rating.RatingT] (Where Postgres Rating.RatingT)
  | RegistrationTokenOptions UpdateModel [Set Postgres RegistrationToken.RegistrationTokenT] (Where Postgres RegistrationToken.RegistrationTokenT)
  | RideOptions UpdateModel [Set Postgres Ride.RideT] (Where Postgres Ride.RideT)
  | RideDetailsOptions UpdateModel [Set Postgres RideDetails.RideDetailsT] (Where Postgres RideDetails.RideDetailsT)
  | RiderDetailsOptions UpdateModel [Set Postgres RiderDetails.RiderDetailsT] (Where Postgres RiderDetails.RiderDetailsT)
  | SearchRequestOptions UpdateModel [Set Postgres SearchRequest.SearchRequestT] (Where Postgres SearchRequest.SearchRequestT)
  | SearchRequestForDriverOptions UpdateModel [Set Postgres SearchRequestForDriver.SearchRequestForDriverT] (Where Postgres SearchRequestForDriver.SearchRequestForDriverT)
  | SearchTryOptions UpdateModel [Set Postgres SearchTry.SearchTryT] (Where Postgres SearchTry.SearchTryT)
  | VehicleOptions UpdateModel [Set Postgres Vehicle.VehicleT] (Where Postgres Vehicle.VehicleT)
  | VolunteerOptions UpdateModel [Set Postgres Volunteer.VolunteerT] (Where Postgres Volunteer.VolunteerT)
  | FeedbackFormOptions UpdateModel [Set Postgres FeedbackForm.FeedbackFormT] (Where Postgres FeedbackForm.FeedbackFormT)
  | FeedbackOptions UpdateModel [Set Postgres Feedback.FeedbackT] (Where Postgres Feedback.FeedbackT)
  | FeedbackBadgeOptions UpdateModel [Set Postgres FeedbackBadge.FeedbackBadgeT] (Where Postgres FeedbackBadge.FeedbackBadgeT)
  | BecknRequestOptions UpdateModel [Set Postgres BecknRequest.BecknRequestT] (Where Postgres BecknRequest.BecknRequestT)
  | RegistryMapFallbackOptions UpdateModel [Set Postgres RegistryMapFallback.RegistryMapFallbackT] (Where Postgres RegistryMapFallback.RegistryMapFallbackT)
  | DriverGoHomeRequestOptions UpdateModel [Set Postgres DriverGoHomeRequest.DriverGoHomeRequestT] (Where Postgres DriverGoHomeRequest.DriverGoHomeRequestT)
  | DriverHomeLocationOptions UpdateModel [Set Postgres DriverHomeLocation.DriverHomeLocationT] (Where Postgres DriverHomeLocation.DriverHomeLocationT)
  | GoHomeConfigOptions UpdateModel [Set Postgres GoHomeConfig.GoHomeConfigT] (Where Postgres GoHomeConfig.GoHomeConfigT)
  | LocationOptions UpdateModel [Set Postgres Location.LocationT] (Where Postgres Location.LocationT)
  | LocationMappingOptions UpdateModel [Set Postgres LocationMapping.LocationMappingT] (Where Postgres LocationMapping.LocationMappingT)
  | PaymentOrderOptions UpdateModel [Set Postgres PaymentOrder.PaymentOrderT] (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionOptions UpdateModel [Set Postgres PaymentTransaction.PaymentTransactionT] (Where Postgres PaymentTransaction.PaymentTransactionT)

-------------------------------- ToJSON DBUpdateObject -------------------------------------
instance ToJSON DBUpdateObject where
  toJSON = error "ToJSON not implemented for DBUpdateObject - Use getDbUpdateCommandJson instead" -- Using getDbUpdateCommandJson instead of toJSON

-- -------------------------------- FromJSON DBUpdateObject -----------------------------------
instance FromJSON DBUpdateObject where
  parseJSON = A.withObject "DBUpdateObject" $ \o -> do
    contents <- o .: "contents"
    updateModel <- parseTagUpdate =<< (o .: "tag")
    case updateModel of
      BapMetadataUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BapMetadataOptions updateModel updVals whereClause
      BookingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingOptions updateModel updVals whereClause
      BookingCancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingCancellationReasonOptions updateModel updVals whereClause
      BusinessEventUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BusinessEventOptions updateModel updVals whereClause
      CallStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CallStatusOptions updateModel updVals whereClause
      CancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CancellationReasonOptions updateModel updVals whereClause
      DriverBlockReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverBlockReasonOptions updateModel updVals whereClause
      DriverPlanUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverPlanOptions updateModel updVals whereClause
      FleetDriverAssociationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FleetDriverAssociationOptions updateModel updVals whereClause
      DriverFeeUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverFeeOptions updateModel updVals whereClause
      DriverInformationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverInformationOptions updateModel updVals whereClause
      AadhaarOtpReqUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AadhaarOtpReqOptions updateModel updVals whereClause
      AadhaarOtpVerifyUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AadhaarOtpVerifyOptions updateModel updVals whereClause
      AadhaarVerificationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AadhaarVerificationOptions updateModel updVals whereClause
      DriverLicenseUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverLicenseOptions updateModel updVals whereClause
      DriverRcAssociationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverRcAssociationOptions updateModel updVals whereClause
      IdfyVerificationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IdfyVerificationOptions updateModel updVals whereClause
      ImageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ImageOptions updateModel updVals whereClause
      VehicleRegistrationCertificateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ VehicleRegistrationCertificateOptions updateModel updVals whereClause
      DriverQuoteUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverQuoteOptions updateModel updVals whereClause
      DriverReferralUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverReferralOptions updateModel updVals whereClause
      DriverStatsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverStatsOptions updateModel updVals whereClause
      EstimateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EstimateOptions updateModel updVals whereClause
      ExophoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ExophoneOptions updateModel updVals whereClause
      FareParametersUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareParametersOptions updateModel updVals whereClause
      FareParametersProgressiveDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareParametersProgressiveDetailsOptions updateModel updVals whereClause
      FareParametersSlabDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareParametersSlabDetailsOptions updateModel updVals whereClause
      FarePolicyUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicyOptions updateModel updVals whereClause
      DriverExtraFeeBoundsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverExtraFeeBoundsOptions updateModel updVals whereClause
      FarePolicyProgressiveDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicyProgressiveDetailsOptions updateModel updVals whereClause
      FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions updateModel updVals whereClause
      FarePolicySlabDetailsSlabUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicySlabDetailsSlabOptions updateModel updVals whereClause
      RestrictedExtraFareUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RestrictedExtraFareOptions updateModel updVals whereClause
      FareProductUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareProductOptions updateModel updVals whereClause
      GeometryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GeometryOptions updateModel updVals whereClause
      CommentUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CommentOptions updateModel updVals whereClause
      IssueCategoryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueCategoryOptions updateModel updVals whereClause
      IssueOptionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueOptionOptions updateModel updVals whereClause
      IssueReportUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueReportOptions updateModel updVals whereClause
      IssueTranslationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueTranslationOptions updateModel updVals whereClause
      PlaceNameCacheUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PlaceNameCacheOptions updateModel updVals whereClause
      MandateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MandateOptions updateModel updVals whereClause
      MediaFileUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MediaFileOptions updateModel updVals whereClause
      MerchantUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantOptions updateModel updVals whereClause
      DriverIntelligentPoolConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverIntelligentPoolConfigOptions updateModel updVals whereClause
      DriverPoolConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverPoolConfigOptions updateModel updVals whereClause
      MerchantLeaderBoardConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantLeaderBoardConfigOptions updateModel updVals whereClause
      MerchantMessageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantMessageOptions updateModel updVals whereClause
      MerchantPaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantPaymentMethodOptions updateModel updVals whereClause
      MerchantServiceConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantServiceConfigOptions updateModel updVals whereClause
      MerchantServiceUsageConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantServiceUsageConfigOptions updateModel updVals whereClause
      MerchantOnboardingDocumentConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantOnboardingDocumentConfigOptions updateModel updVals whereClause
      TransporterConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TransporterConfigOptions updateModel updVals whereClause
      MessageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MessageOptions updateModel updVals whereClause
      MessageReportUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MessageReportOptions updateModel updVals whereClause
      MessageTranslationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MessageTranslationOptions updateModel updVals whereClause
      MetaDataUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MetaDataOptions updateModel updVals whereClause
      PersonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonOptions updateModel updVals whereClause
      QuoteSpecialZoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ QuoteSpecialZoneOptions updateModel updVals whereClause
      RatingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RatingOptions updateModel updVals whereClause
      RegistrationTokenUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RegistrationTokenOptions updateModel updVals whereClause
      RideUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RideOptions updateModel updVals whereClause
      RideDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RideDetailsOptions updateModel updVals whereClause
      RiderDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RiderDetailsOptions updateModel updVals whereClause
      SearchRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestOptions updateModel updVals whereClause
      SearchRequestForDriverUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestForDriverOptions updateModel updVals whereClause
      SearchTryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchTryOptions updateModel updVals whereClause
      VehicleUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ VehicleOptions updateModel updVals whereClause
      VolunteerUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ VolunteerOptions updateModel updVals whereClause
      FeedbackFormUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FeedbackFormOptions updateModel updVals whereClause
      FeedbackUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FeedbackOptions updateModel updVals whereClause
      FeedbackBadgeUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FeedbackBadgeOptions updateModel updVals whereClause
      BecknRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BecknRequestOptions updateModel updVals whereClause
      RegistryMapFallbackUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RegistryMapFallbackOptions updateModel updVals whereClause
      DriverGoHomeRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverGoHomeRequestOptions updateModel updVals whereClause
      DriverHomeLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverHomeLocationOptions updateModel updVals whereClause
      GoHomeConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GoHomeConfigOptions updateModel updVals whereClause
      LocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ LocationOptions updateModel updVals whereClause
      LocationMappingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ LocationMappingOptions updateModel updVals whereClause
      PaymentOrderUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentOrderOptions updateModel updVals whereClause
      PaymentTransactionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentTransactionOptions updateModel updVals whereClause
