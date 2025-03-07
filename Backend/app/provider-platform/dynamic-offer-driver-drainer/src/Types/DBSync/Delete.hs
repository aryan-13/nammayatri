module Types.DBSync.Delete where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.Beam.Postgres (Postgres)
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

data DeleteModel
  = RegistrationTokenDelete
  | BapMetadataDelete
  | BookingDelete
  | BookingCancellationReasonDelete
  | BusinessEventDelete
  | CallStatusDelete
  | CancellationReasonDelete
  | DriverBlockReasonDelete
  | FleetDriverAssociationDelete
  | DriverFeeDelete
  | DriverInformationDelete
  | AadhaarOtpReqDelete
  | AadhaarOtpVerifyDelete
  | AadhaarVerificationDelete
  | DriverLicenseDelete
  | DriverRcAssociationDelete
  | IdfyVerificationDelete
  | ImageDelete
  | VehicleRegistrationCertificateDelete
  | DriverPlanDelete
  | DriverQuoteDelete
  | DriverReferralDelete
  | DriverStatsDelete
  | EstimateDelete
  | ExophoneDelete
  | FareParametersDelete
  | FareParametersProgressiveDetailsDelete
  | FareParametersSlabDetailsDelete
  | FarePolicyDelete
  | DriverExtraFeeBoundsDelete
  | FarePolicyProgressiveDetailsDelete
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete
  | FarePolicySlabDetailsSlabDelete
  | RestrictedExtraFareDelete
  | FareProductDelete
  | GeometryDelete
  | CommentDelete
  | IssueCategoryDelete
  | IssueOptionDelete
  | IssueReportDelete
  | IssueTranslationDelete
  | PlaceNameCacheDelete
  | MandateDelete
  | MediaFileDelete
  | MerchantDelete
  | DriverIntelligentPoolConfigDelete
  | DriverPoolConfigDelete
  | MerchantLeaderBoardConfigDelete
  | MerchantMessageDelete
  | MerchantPaymentMethodDelete
  | MerchantServiceConfigDelete
  | MerchantServiceUsageConfigDelete
  | MerchantOnboardingDocumentConfigDelete
  | TransporterConfigDelete
  | MessageDelete
  | MessageReportDelete
  | MessageTranslationDelete
  | MetaDataDelete
  | PersonDelete
  | QuoteSpecialZoneDelete
  | RatingDelete
  | RideDelete
  | RideDetailsDelete
  | RiderDetailsDelete
  | SearchRequestDelete
  | SearchRequestForDriverDelete
  | SearchTryDelete
  | VehicleDelete
  | VolunteerDelete
  | FeedbackFormDelete
  | FeedbackDelete
  | FeedbackBadgeDelete
  | BecknRequestDelete
  | RegistryMapFallbackDelete
  | DriverGoHomeRequestDelete
  | DriverHomeLocationDelete
  | GoHomeConfigDelete
  | LocationDelete
  | LocationMappingDelete
  | PaymentOrderDelete
  | PaymentTransactionDelete
  deriving (Generic, Show)

getTagDelete :: DeleteModel -> Text
getTagDelete RegistrationTokenDelete = "RegistrationTokenOptions"
getTagDelete BapMetadataDelete = "BapMetadataOptions"
getTagDelete BookingDelete = "BookingOptions"
getTagDelete BookingCancellationReasonDelete = "BookingCancellationReasonOptions"
getTagDelete BusinessEventDelete = "BusinessEventOptions"
getTagDelete CallStatusDelete = "CallStatusOptions"
getTagDelete CancellationReasonDelete = "CancellationReasonOptions"
getTagDelete DriverPlanDelete = "DriverPlanOptions"
getTagDelete DriverBlockReasonDelete = "DriverBlockReasonOptions"
getTagDelete FleetDriverAssociationDelete = "FleetDriverAssociationOptions"
getTagDelete DriverFeeDelete = "DriverFeeOptions"
getTagDelete DriverInformationDelete = "DriverInformationOptions"
getTagDelete AadhaarOtpReqDelete = "AadhaarOtpReqOptions"
getTagDelete AadhaarOtpVerifyDelete = "AadhaarOtpVerifyOptions"
getTagDelete AadhaarVerificationDelete = "AadhaarVerificationOptions"
getTagDelete DriverLicenseDelete = "DriverLicenseOptions"
getTagDelete DriverRcAssociationDelete = "DriverRcAssociationOptions"
getTagDelete IdfyVerificationDelete = "IdfyVerificationOptions"
getTagDelete ImageDelete = "ImageOptions"
getTagDelete VehicleRegistrationCertificateDelete = "VehicleRegistrationCertificateOptions"
getTagDelete DriverQuoteDelete = "DriverQuoteOptions"
getTagDelete DriverReferralDelete = "DriverReferralOptions"
getTagDelete DriverStatsDelete = "DriverStatsOptions"
getTagDelete EstimateDelete = "EstimateOptions"
getTagDelete ExophoneDelete = "ExophoneOptions"
getTagDelete FareParametersDelete = "FareParametersOptions"
getTagDelete FareParametersProgressiveDetailsDelete = "FareParametersProgressiveDetailsOptions"
getTagDelete FareParametersSlabDetailsDelete = "FareParametersSlabDetailsOptions"
getTagDelete FarePolicyDelete = "FarePolicyOptions"
getTagDelete DriverExtraFeeBoundsDelete = "DriverExtraFeeBoundsOptions"
getTagDelete FarePolicyProgressiveDetailsDelete = "FarePolicyProgressiveDetailsOptions"
getTagDelete FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete = "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions"
getTagDelete FarePolicySlabDetailsSlabDelete = "FarePolicySlabDetailsSlabOptions"
getTagDelete RestrictedExtraFareDelete = "RestrictedExtraFareOptions"
getTagDelete FareProductDelete = "FareProductOptions"
getTagDelete GeometryDelete = "GeometryOptions"
getTagDelete CommentDelete = "CommentOptions"
getTagDelete IssueCategoryDelete = "IssueCategoryOptions"
getTagDelete IssueOptionDelete = "IssueOptionOptions"
getTagDelete IssueReportDelete = "IssueReportOptions"
getTagDelete IssueTranslationDelete = "IssueTranslationOptions"
getTagDelete PlaceNameCacheDelete = "PlaceNameCacheOptions"
getTagDelete MediaFileDelete = "MediaFileOptions"
getTagDelete MerchantDelete = "MerchantOptions"
getTagDelete DriverIntelligentPoolConfigDelete = "DriverIntelligentPoolConfigOptions"
getTagDelete DriverPoolConfigDelete = "DriverPoolConfigOptions"
getTagDelete MandateDelete = "MandateOptions"
getTagDelete MerchantLeaderBoardConfigDelete = "MerchantLeaderBoardConfigOptions"
getTagDelete MerchantMessageDelete = "MerchantMessageOptions"
getTagDelete MerchantPaymentMethodDelete = "MerchantPaymentMethodOptions"
getTagDelete MerchantServiceConfigDelete = "MerchantServiceConfigOptions"
getTagDelete MerchantServiceUsageConfigDelete = "MerchantServiceUsageConfigOptions"
getTagDelete MerchantOnboardingDocumentConfigDelete = "MerchantOnboardingDocumentConfigOptions"
getTagDelete TransporterConfigDelete = "TransporterConfigOptions"
getTagDelete MessageDelete = "MessageOptions"
getTagDelete MessageReportDelete = "MessageReportOptions"
getTagDelete MessageTranslationDelete = "MessageTranslationOptions"
getTagDelete MetaDataDelete = "MetaDataOptions"
getTagDelete PersonDelete = "PersonOptions"
getTagDelete QuoteSpecialZoneDelete = "QuoteSpecialZoneOptions"
getTagDelete RatingDelete = "RatingOptions"
getTagDelete RideDelete = "RideOptions"
getTagDelete RideDetailsDelete = "RideDetailsOptions"
getTagDelete RiderDetailsDelete = "RiderDetailsOptions"
getTagDelete SearchRequestDelete = "SearchRequestOptions"
getTagDelete SearchRequestForDriverDelete = "SearchRequestForDriverOptions"
getTagDelete SearchTryDelete = "SearchTryOptions"
getTagDelete VehicleDelete = "VehicleOptions"
getTagDelete VolunteerDelete = "VolunteerOptions"
getTagDelete FeedbackFormDelete = "FeedbackFormOptions"
getTagDelete FeedbackDelete = "FeedbackOptions"
getTagDelete FeedbackBadgeDelete = "FeedbackBadgeOptions"
getTagDelete BecknRequestDelete = "BecknRequestOptions"
getTagDelete RegistryMapFallbackDelete = "RegistryMapFallbackOptions"
getTagDelete DriverGoHomeRequestDelete = "DriverGoHomeRequestOptions"
getTagDelete DriverHomeLocationDelete = "DriverHomeLocationOptions"
getTagDelete GoHomeConfigDelete = "GoHomeConfigOptions"
getTagDelete LocationDelete = "LocationOptions"
getTagDelete LocationMappingDelete = "LocationMappingOptions"
getTagDelete PaymentOrderDelete = "PaymentOrderOptions"
getTagDelete PaymentTransactionDelete = "PaymentTransactionOptions"

parseTagDelete :: Text -> Parser DeleteModel
parseTagDelete "RegistrationTokenOptions" = return RegistrationTokenDelete
parseTagDelete "BookingOptions" = return BookingDelete
parseTagDelete "BookingCancellationReasonOptions" = return BookingCancellationReasonDelete
parseTagDelete "BusinessEventOptions" = return BusinessEventDelete
parseTagDelete "CallStatusOptions" = return CallStatusDelete
parseTagDelete "CancellationReasonOptions" = return CancellationReasonDelete
parseTagDelete "DriverPlanOptions" = return DriverPlanDelete
parseTagDelete "DriverFeeOptions" = return DriverFeeDelete
parseTagDelete "DriverInformationOptions" = return DriverInformationDelete
parseTagDelete "AadhaarOtpReqOptions" = return AadhaarOtpReqDelete
parseTagDelete "AadhaarOtpVerifyOptions" = return AadhaarOtpVerifyDelete
parseTagDelete "AadhaarVerificationOptions" = return AadhaarVerificationDelete
parseTagDelete "DriverLicenseOptions" = return DriverLicenseDelete
parseTagDelete "DriverRcAssociationOptions" = return DriverRcAssociationDelete
parseTagDelete "IdfyVerificationOptions" = return IdfyVerificationDelete
parseTagDelete "ImageOptions" = return ImageDelete
parseTagDelete "VehicleRegistrationCertificateOptions" = return VehicleRegistrationCertificateDelete
parseTagDelete "DriverQuoteOptions" = return DriverQuoteDelete
parseTagDelete "DriverReferralOptions" = return DriverReferralDelete
parseTagDelete "DriverStatsOptions" = return DriverStatsDelete
parseTagDelete "EstimateOptions" = return EstimateDelete
parseTagDelete "ExophoneOptions" = return ExophoneDelete
parseTagDelete "FareParametersOptions" = return FareParametersDelete
parseTagDelete "FareParametersProgressiveDetailsOptions" = return FareParametersProgressiveDetailsDelete
parseTagDelete "FareParametersSlabDetailsOptions" = return FareParametersSlabDetailsDelete
parseTagDelete "FarePolicyOptions" = return FarePolicyDelete
parseTagDelete "DriverExtraFeeBoundsOptions" = return DriverExtraFeeBoundsDelete
parseTagDelete "FarePolicyProgressiveDetailsOptions" = return FarePolicyProgressiveDetailsDelete
parseTagDelete "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions" = return FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete
parseTagDelete "FarePolicySlabDetailsSlabOptions" = return FarePolicySlabDetailsSlabDelete
parseTagDelete "RestrictedExtraFareOptions" = return RestrictedExtraFareDelete
parseTagDelete "FareProductOptions" = return FareProductDelete
parseTagDelete "GeometryOptions" = return GeometryDelete
parseTagDelete "CommentOptions" = return CommentDelete
parseTagDelete "IssueCategoryOptions" = return IssueCategoryDelete
parseTagDelete "IssueOptionOptions" = return IssueOptionDelete
parseTagDelete "IssueReportOptions" = return IssueReportDelete
parseTagDelete "IssueTranslationOptions" = return IssueTranslationDelete
parseTagDelete "PlaceNameCacheOptions" = return PlaceNameCacheDelete
parseTagDelete "MandateOptions" = return MandateDelete
parseTagDelete "MediaFileOptions" = return MediaFileDelete
parseTagDelete "MerchantOptions" = return MerchantDelete
parseTagDelete "DriverIntelligentPoolConfigOptions" = return DriverIntelligentPoolConfigDelete
parseTagDelete "DriverPoolConfigOptions" = return DriverPoolConfigDelete
parseTagDelete "MerchantLeaderBoardConfigOptions" = return MerchantLeaderBoardConfigDelete
parseTagDelete "MerchantMessageOptions" = return MerchantMessageDelete
parseTagDelete "MerchantPaymentMethodOptions" = return MerchantPaymentMethodDelete
parseTagDelete "MerchantServiceConfigOptions" = return MerchantServiceConfigDelete
parseTagDelete "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigDelete
parseTagDelete "MerchantOnboardingDocumentConfigOptions" = return MerchantOnboardingDocumentConfigDelete
parseTagDelete "TransporterConfigOptions" = return TransporterConfigDelete
parseTagDelete "MessageOptions" = return MessageDelete
parseTagDelete "MessageReportOptions" = return MessageReportDelete
parseTagDelete "MessageTranslationOptions" = return MessageTranslationDelete
parseTagDelete "PersonOptions" = return PersonDelete
parseTagDelete "QuoteSpecialZoneOptions" = return QuoteSpecialZoneDelete
parseTagDelete "RatingOptions" = return RatingDelete
parseTagDelete "RideOptions" = return RideDelete
parseTagDelete "RideDetailsOptions" = return RideDetailsDelete
parseTagDelete "RiderDetailsOptions" = return RiderDetailsDelete
parseTagDelete "SearchRequestOptions" = return SearchRequestDelete
parseTagDelete "SearchRequestForDriverOptions" = return SearchRequestForDriverDelete
parseTagDelete "SearchTryOptions" = return SearchTryDelete
parseTagDelete "VehicleOptions" = return VehicleDelete
parseTagDelete "VolunteerOptions" = return VolunteerDelete
parseTagDelete "FeedbackFormOptions" = return FeedbackFormDelete
parseTagDelete "FeedbackOptions" = return FeedbackDelete
parseTagDelete "FeedbackBadgeOptions" = return FeedbackBadgeDelete
parseTagDelete "BecknRequestOptions" = return BecknRequestDelete
parseTagDelete "RegistryMapFallbackOptions" = return RegistryMapFallbackDelete
parseTagDelete "DriverGoHomeRequestOptions" = return DriverGoHomeRequestDelete
parseTagDelete "DriverHomeLocationOptions" = return DriverHomeLocationDelete
parseTagDelete "GoHomeConfigOptions" = return GoHomeConfigDelete
parseTagDelete "LocationOptions" = return LocationDelete
parseTagDelete "LocationMappingOptions" = return LocationMappingDelete
parseTagDelete "PaymentOrderOptions" = return PaymentOrderDelete
parseTagDelete "PaymentTransactionOptions" = return PaymentTransactionDelete
parseTagDelete t = fail $ T.unpack ("Expected a DeleteModel but got '" <> t <> "'")

data DBDeleteObject
  = RegistrationTokenDeleteOptions DeleteModel (Where Postgres RegistrationToken.RegistrationTokenT)
  | BapMetadataDeleteOptions DeleteModel (Where Postgres BapMetadata.BapMetadataT)
  | BookingDeleteOptions DeleteModel (Where Postgres Booking.BookingT)
  | BookingCancellationReasonDeleteOptions DeleteModel (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | BusinessEventDeleteOptions DeleteModel (Where Postgres BusinessEvent.BusinessEventT)
  | CallStatusDeleteOptions DeleteModel (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonDeleteOptions DeleteModel (Where Postgres CancellationReason.CancellationReasonT)
  | DriverBlockReasonDeleteOptions DeleteModel (Where Postgres DriverBlockReason.DriverBlockReasonT)
  | FleetDriverAssociationDeleteOptions DeleteModel (Where Postgres FleetDriverAssociation.FleetDriverAssociationT)
  | DriverFeeDeleteOptions DeleteModel (Where Postgres DriverFee.DriverFeeT)
  | DriverInformationDeleteOptions DeleteModel (Where Postgres DriverInformation.DriverInformationT)
  | DriverPlanDeleteOptions DeleteModel (Where Postgres DriverPlan.DriverPlanT)
  | AadhaarOtpReqDeleteOptions DeleteModel (Where Postgres AadhaarOtpReq.AadhaarOtpReqT)
  | AadhaarOtpVerifyDeleteOptions DeleteModel (Where Postgres AadhaarOtpVerify.AadhaarOtpVerifyT)
  | AadhaarVerificationDeleteOptions DeleteModel (Where Postgres AadhaarVerification.AadhaarVerificationT)
  | DriverLicenseDeleteOptions DeleteModel (Where Postgres DriverLicense.DriverLicenseT)
  | DriverRcAssociationDeleteOptions DeleteModel (Where Postgres DriverRCAssociation.DriverRCAssociationT)
  | IdfyVerificationDeleteOptions DeleteModel (Where Postgres IdfyVerification.IdfyVerificationT)
  | ImageDeleteOptions DeleteModel (Where Postgres Image.ImageT)
  | VehicleRegistrationCertificateDeleteOptions DeleteModel (Where Postgres VehicleRegistrationCertificate.VehicleRegistrationCertificateT)
  | DriverQuoteDeleteOptions DeleteModel (Where Postgres DriverQuote.DriverQuoteT)
  | DriverReferralDeleteOptions DeleteModel (Where Postgres DriverReferral.DriverReferralT)
  | DriverStatsDeleteOptions DeleteModel (Where Postgres DriverStats.DriverStatsT)
  | EstimateDeleteOptions DeleteModel (Where Postgres Estimate.EstimateT)
  | ExophoneDeleteOptions DeleteModel (Where Postgres Exophone.ExophoneT)
  | FareParametersDeleteOptions DeleteModel (Where Postgres FareParameters.FareParametersT)
  | FareParametersProgressiveDetailsDeleteOptions DeleteModel (Where Postgres FareParametersProgressiveDetails.FareParametersProgressiveDetailsT)
  | FareParametersSlabDetailsDeleteOptions DeleteModel (Where Postgres FareParametersSlabDetails.FareParametersSlabDetailsT)
  | FarePolicyDeleteOptions DeleteModel (Where Postgres FarePolicy.FarePolicyT)
  | DriverExtraFeeBoundsDeleteOptions DeleteModel (Where Postgres DriverExtraFeeBounds.DriverExtraFeeBoundsT)
  | FarePolicyProgressiveDetailsDeleteOptions DeleteModel (Where Postgres FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT)
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionDeleteOptions DeleteModel (Where Postgres FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSectionT)
  | FarePolicySlabDetailsSlabDeleteOptions DeleteModel (Where Postgres FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlabT)
  | RestrictedExtraFareDeleteOptions DeleteModel (Where Postgres RestrictedExtraFare.RestrictedExtraFareT)
  | FareProductDeleteOptions DeleteModel (Where Postgres FareProduct.FareProductT)
  | GeometryDeleteOptions DeleteModel (Where Postgres Geometry.GeometryT)
  | CommentDeleteOptions DeleteModel (Where Postgres Comment.CommentT)
  | IssueCategoryDeleteOptions DeleteModel (Where Postgres IssueCategory.IssueCategoryT)
  | IssueOptionDeleteOptions DeleteModel (Where Postgres IssueOption.IssueOptionT)
  | IssueReportDeleteOptions DeleteModel (Where Postgres IssueReport.IssueReportT)
  | IssueTranslationDeleteOptions DeleteModel (Where Postgres IssueTranslation.IssueTranslationT)
  | PlaceNameCacheDeleteOptions DeleteModel (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MediaFileDeleteOptions DeleteModel (Where Postgres MediaFile.MediaFileT)
  | MandateDeleteOptions DeleteModel (Where Postgres Mandate.MandateT)
  | MerchantDeleteOptions DeleteModel (Where Postgres Merchant.MerchantT)
  | DriverIntelligentPoolConfigDeleteOptions DeleteModel (Where Postgres DriverIntelligentPoolConfig.DriverIntelligentPoolConfigT)
  | DriverPoolConfigDeleteOptions DeleteModel (Where Postgres DriverPoolConfig.DriverPoolConfigT)
  | MerchantLeaderBoardConfigDeleteOptions DeleteModel (Where Postgres MerchantLeaderBoardConfig.LeaderBoardConfigsT)
  | MerchantMessageDeleteOptions DeleteModel (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodDeleteOptions DeleteModel (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantOnboardingDocumentConfigDeleteOptions DeleteModel (Where Postgres MerchantOnboardingDocumentConfig.OnboardingDocumentConfigT)
  | TransporterConfigDeleteOptions DeleteModel (Where Postgres TransporterConfig.TransporterConfigT)
  | MessageDeleteOptions DeleteModel (Where Postgres Message.MessageT)
  | MessageReportDeleteOptions DeleteModel (Where Postgres MessageReport.MessageReportT)
  | MessageTranslationDeleteOptions DeleteModel (Where Postgres MessageTranslation.MessageTranslationT)
  | MetaDataDeleteOptions DeleteModel (Where Postgres MetaData.MetaDataT)
  | PersonDeleteOptions DeleteModel (Where Postgres Person.PersonT)
  | QuoteSpecialZoneDeleteOptions DeleteModel (Where Postgres Quote.QuoteSpecialZoneT)
  | RatingDeleteOptions DeleteModel (Where Postgres Rating.RatingT)
  | RideDeleteOptions DeleteModel (Where Postgres Ride.RideT)
  | RideDetailsDeleteOptions DeleteModel (Where Postgres RideDetails.RideDetailsT)
  | RiderDetailsDeleteOptions DeleteModel (Where Postgres RiderDetails.RiderDetailsT)
  | SearchRequestDeleteOptions DeleteModel (Where Postgres SearchRequest.SearchRequestT)
  | SearchRequestForDriverDeleteOptions DeleteModel (Where Postgres SearchRequestForDriver.SearchRequestForDriverT)
  | SearchTryDeleteOptions DeleteModel (Where Postgres SearchTry.SearchTryT)
  | VehicleDeleteOptions DeleteModel (Where Postgres Vehicle.VehicleT)
  | VolunteerDeleteOptions DeleteModel (Where Postgres Volunteer.VolunteerT)
  | FeedbackFormDeleteOptions DeleteModel (Where Postgres FeedbackForm.FeedbackFormT)
  | FeedbackDeleteOptions DeleteModel (Where Postgres Feedback.FeedbackT)
  | FeedbackBadgeDeleteOptions DeleteModel (Where Postgres FeedbackBadge.FeedbackBadgeT)
  | BecknRequestDeleteOptions DeleteModel (Where Postgres BecknRequest.BecknRequestT)
  | RegistryMapFallbackDeleteOptions DeleteModel (Where Postgres RegistryMapFallback.RegistryMapFallbackT)
  | DriverGoHomeRequestDeleteOptions DeleteModel (Where Postgres DriverGoHomeRequest.DriverGoHomeRequestT)
  | DriverHomeLocationDeleteOptions DeleteModel (Where Postgres DriverHomeLocation.DriverHomeLocationT)
  | GoHomeConfigDeleteOptions DeleteModel (Where Postgres GoHomeConfig.GoHomeConfigT)
  | LocationDeleteOptions DeleteModel (Where Postgres Location.LocationT)
  | LocationMappingDeleteOptions DeleteModel (Where Postgres LocationMapping.LocationMappingT)
  | PaymentOrderDeleteOptions DeleteModel (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionDeleteOptions DeleteModel (Where Postgres PaymentTransaction.PaymentTransactionT)

instance ToJSON DBDeleteObject where
  toJSON = error "ToJSON not implemented for DBDeleteObject - Use getDbDeleteCommandJson instead" -- Using getDbDeleteCommandJson instead of toJSON

instance FromJSON DBDeleteObject where
  parseJSON = A.withObject "DBDeleteObject" $ \o -> do
    contents <- o .: "contents"
    deleteModel <- parseTagDelete =<< (o .: "tag")
    case deleteModel of
      RegistrationTokenDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RegistrationTokenDeleteOptions deleteModel whereClause
      BapMetadataDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BapMetadataDeleteOptions deleteModel whereClause
      BookingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingDeleteOptions deleteModel whereClause
      BookingCancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingCancellationReasonDeleteOptions deleteModel whereClause
      BusinessEventDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BusinessEventDeleteOptions deleteModel whereClause
      CallStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CallStatusDeleteOptions deleteModel whereClause
      CancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CancellationReasonDeleteOptions deleteModel whereClause
      DriverBlockReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverBlockReasonDeleteOptions deleteModel whereClause
      DriverPlanDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverPlanDeleteOptions deleteModel whereClause
      FleetDriverAssociationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FleetDriverAssociationDeleteOptions deleteModel whereClause
      DriverFeeDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverFeeDeleteOptions deleteModel whereClause
      DriverInformationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverInformationDeleteOptions deleteModel whereClause
      AadhaarOtpReqDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AadhaarOtpReqDeleteOptions deleteModel whereClause
      AadhaarOtpVerifyDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AadhaarOtpVerifyDeleteOptions deleteModel whereClause
      AadhaarVerificationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AadhaarVerificationDeleteOptions deleteModel whereClause
      DriverLicenseDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverLicenseDeleteOptions deleteModel whereClause
      DriverRcAssociationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverRcAssociationDeleteOptions deleteModel whereClause
      IdfyVerificationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IdfyVerificationDeleteOptions deleteModel whereClause
      ImageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ImageDeleteOptions deleteModel whereClause
      VehicleRegistrationCertificateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ VehicleRegistrationCertificateDeleteOptions deleteModel whereClause
      DriverQuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverQuoteDeleteOptions deleteModel whereClause
      DriverReferralDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverReferralDeleteOptions deleteModel whereClause
      DriverStatsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverStatsDeleteOptions deleteModel whereClause
      EstimateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EstimateDeleteOptions deleteModel whereClause
      ExophoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ExophoneDeleteOptions deleteModel whereClause
      FareParametersDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareParametersDeleteOptions deleteModel whereClause
      FareParametersProgressiveDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareParametersProgressiveDetailsDeleteOptions deleteModel whereClause
      FareParametersSlabDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareParametersSlabDetailsDeleteOptions deleteModel whereClause
      FarePolicyDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicyDeleteOptions deleteModel whereClause
      DriverExtraFeeBoundsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverExtraFeeBoundsDeleteOptions deleteModel whereClause
      FarePolicyProgressiveDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicyProgressiveDetailsDeleteOptions deleteModel whereClause
      FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicyProgressiveDetailsPerExtraKmRateSectionDeleteOptions deleteModel whereClause
      FarePolicySlabDetailsSlabDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicySlabDetailsSlabDeleteOptions deleteModel whereClause
      RestrictedExtraFareDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RestrictedExtraFareDeleteOptions deleteModel whereClause
      FareProductDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareProductDeleteOptions deleteModel whereClause
      GeometryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GeometryDeleteOptions deleteModel whereClause
      CommentDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CommentDeleteOptions deleteModel whereClause
      IssueCategoryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueCategoryDeleteOptions deleteModel whereClause
      IssueOptionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueOptionDeleteOptions deleteModel whereClause
      IssueReportDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueReportDeleteOptions deleteModel whereClause
      IssueTranslationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueTranslationDeleteOptions deleteModel whereClause
      PlaceNameCacheDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PlaceNameCacheDeleteOptions deleteModel whereClause
      MediaFileDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MediaFileDeleteOptions deleteModel whereClause
      MandateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MandateDeleteOptions deleteModel whereClause
      MerchantDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantDeleteOptions deleteModel whereClause
      DriverIntelligentPoolConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverIntelligentPoolConfigDeleteOptions deleteModel whereClause
      DriverPoolConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverPoolConfigDeleteOptions deleteModel whereClause
      MerchantLeaderBoardConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantLeaderBoardConfigDeleteOptions deleteModel whereClause
      MerchantMessageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantMessageDeleteOptions deleteModel whereClause
      MerchantPaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantPaymentMethodDeleteOptions deleteModel whereClause
      MerchantServiceConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantServiceConfigDeleteOptions deleteModel whereClause
      MerchantServiceUsageConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantServiceUsageConfigDeleteOptions deleteModel whereClause
      MerchantOnboardingDocumentConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantOnboardingDocumentConfigDeleteOptions deleteModel whereClause
      TransporterConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TransporterConfigDeleteOptions deleteModel whereClause
      MessageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MessageDeleteOptions deleteModel whereClause
      MessageReportDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MessageReportDeleteOptions deleteModel whereClause
      MessageTranslationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MessageTranslationDeleteOptions deleteModel whereClause
      MetaDataDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MetaDataDeleteOptions deleteModel whereClause
      PersonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonDeleteOptions deleteModel whereClause
      QuoteSpecialZoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ QuoteSpecialZoneDeleteOptions deleteModel whereClause
      RatingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RatingDeleteOptions deleteModel whereClause
      RideDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDeleteOptions deleteModel whereClause
      RideDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDetailsDeleteOptions deleteModel whereClause
      RiderDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RiderDetailsDeleteOptions deleteModel whereClause
      SearchRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestDeleteOptions deleteModel whereClause
      SearchRequestForDriverDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestForDriverDeleteOptions deleteModel whereClause
      SearchTryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchTryDeleteOptions deleteModel whereClause
      VehicleDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ VehicleDeleteOptions deleteModel whereClause
      VolunteerDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ VolunteerDeleteOptions deleteModel whereClause
      FeedbackFormDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackFormDeleteOptions deleteModel whereClause
      FeedbackDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackDeleteOptions deleteModel whereClause
      FeedbackBadgeDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackBadgeDeleteOptions deleteModel whereClause
      BecknRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BecknRequestDeleteOptions deleteModel whereClause
      RegistryMapFallbackDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RegistryMapFallbackDeleteOptions deleteModel whereClause
      DriverGoHomeRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverGoHomeRequestDeleteOptions deleteModel whereClause
      DriverHomeLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverHomeLocationDeleteOptions deleteModel whereClause
      GoHomeConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GoHomeConfigDeleteOptions deleteModel whereClause
      LocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LocationDeleteOptions deleteModel whereClause
      LocationMappingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LocationMappingDeleteOptions deleteModel whereClause
      PaymentOrderDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentOrderDeleteOptions deleteModel whereClause
      PaymentTransactionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentTransactionDeleteOptions deleteModel whereClause
