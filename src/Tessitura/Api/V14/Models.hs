{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tessitura.Api.V14.Models where

import Data.Aeson
import Data.Time.Clock

type SessionKey = String

data SessionRequest
  = SessionRequest
  { sessionrequestOrganization :: String
  , sessionrequestIpAddress :: String
  , sessionrequestBusinessUnitId :: Int
  }
  deriving (Eq)

instance FromJSON SessionRequest where
  parseJSON (Object req) = SessionRequest
    <$> req .: "Organization"
    <*> req .: "IpAddress"
    <*> req .: "BusinessUnitId"
  parseJSON _ = error "Invalid session request JSON"

data SessionResponse
  = SessionResponse
  { sessionresponseSessionKey :: SessionKey }
  deriving (Eq)

instance ToJSON SessionResponse where
  toJSON SessionResponse{..} = object
    [ "SessionKey" .= sessionresponseSessionKey ]

data Session
  = Session
  { sessionSessionKey :: SessionKey
  , sessionOriginalModeOfSaleId :: Int
  , sessionModeOfSaleId :: Int
  , sessionSourceId :: Int
  , sessionOrderId :: Int
  , sessionIsLoggedIn :: Bool
  , sessionCartInfo :: Maybe CartInfo
  , sessionBusinessFacing :: Bool
  , sessionLoginInfo :: Maybe SessionLoginInfo
  }
  deriving (Eq, Show)

instance ToJSON Session where
  toJSON Session{..} = object
    [ "OriginalModeOfSaleId" .= sessionOriginalModeOfSaleId
    , "ModeOfSaleId" .= sessionModeOfSaleId
    , "SourceId" .= sessionSourceId
    , "OrderId" .= sessionOrderId
    , "IsLoggedIn" .= sessionIsLoggedIn
    , "CartInfo" .= sessionCartInfo
    , "BusinessFacing" .= sessionBusinessFacing
    , "LoginInfo" .= sessionLoginInfo
    ]

data CartInfo
  = CartInfo
  { cartinfoContributionCount :: Int
  , cartinfoUserDefinedFeeCount :: Int
  , cartinfoPerformanceCount :: Int
  , cartinfoFirstSeatAddedDateTime :: UTCTime
  , cartinfoPaymentCount :: Int
  , cartinfoMembershipCount :: Int
  , cartinfoPackageCount :: Int
  , cartinfoGiftCertificateCount :: Int
  }
  deriving (Eq, Show)

instance ToJSON CartInfo where
  toJSON CartInfo{..} = object
    [ "ContributionCount" .= cartinfoContributionCount
    , "UserDefinedFeeCount" .= cartinfoUserDefinedFeeCount
    , "PerformanceCount" .= cartinfoPerformanceCount
    , "FirstSeatAddedDateTime" .= cartinfoFirstSeatAddedDateTime
    , "PaymentCount" .= cartinfoPaymentCount
    , "MembershipCount" .= cartinfoMembershipCount
    , "PackageCount" .= cartinfoPackageCount
    , "GiftCertificateCount" .= cartinfoGiftCertificateCount
    ]

data SessionLoginInfo
  = SessionLoginInfo
  { sessionlogininfoUserId :: String
  , sessionlogininfoConstituentId :: Int
  , sessionlogininfoStatus :: String
  , sessionlogininfoFailedAttempts :: Int
  , sessionlogininfoOriginalConstituentId :: Int
  , sessionlogininfoElectronicAddress :: String
  , sessionlogininfoLockedDate :: UTCTime
  }
  deriving (Eq, Show)

instance ToJSON SessionLoginInfo where
  toJSON SessionLoginInfo{..} = object
    [ "UserId" .= sessionlogininfoUserId
    , "ConstituentId" .= sessionlogininfoConstituentId
    , "Status" .= sessionlogininfoStatus
    , "FailedAttempts" .= sessionlogininfoFailedAttempts
    , "OriginalConstituentId" .= sessionlogininfoOriginalConstituentId
    , "ElectronicAddress" .= sessionlogininfoElectronicAddress
    , "LockedDate" .= sessionlogininfoLockedDate
    ]

data LoginRequest
  = LoginRequest
  { loginrequestPromotionCode :: String
  , loginrequestPassword :: String
  , loginrequestUserName :: String
  , loginrequestLoginTypeId :: Int
  }
  deriving (Eq)

instance FromJSON LoginRequest where
  parseJSON (Object req) = LoginRequest
    <$> req .: "PromotionCode"
    <*> req .: "Password"
    <*> req .: "UserName"
    <*> req .: "LoginTypeId"
  parseJSON _ = error "Invalid login request JSON"

data OnAccountBalance
  = OnAccountBalance
  { onaccountbalanceBalance :: Double
  , onaccountbalanceDescription :: String
  , onaccountbalancePaymentMethodId :: Int
  , onaccountbalanceConstituentId :: Int
  , onaccountbalanceCurrentBalance :: Double
  , onaccountbalanceUsedInSession :: Double
  }
  deriving (Eq)

instance ToJSON OnAccountBalance where
  toJSON OnAccountBalance{..} = object
    [ "Balance" .= onaccountbalanceBalance
    , "Description" .= onaccountbalanceDescription
    , "PaymentMethodId" .= onaccountbalancePaymentMethodId
    , "ConstituentId" .= onaccountbalanceConstituentId
    , "CurrentBalance" .= onaccountbalanceCurrentBalance
    , "UsedInSession" .= onaccountbalanceUsedInSession
    ]

data LoginEmailRequest
  = LoginEmailRequest
  { loginemailrequestPromotionCode :: String
  , loginemailrequestPassword :: String
  , loginemailrequestEmailAddress :: String
  , loginemailrequestLoginTypeId :: Int
  }
  deriving (Eq)

instance FromJSON LoginEmailRequest where
  parseJSON (Object req) = LoginEmailRequest
    <$> req .: "PromotionCode"
    <*> req .: "Password"
    <*> req .: "EmailAddress"
    <*> req .: "LoginTypeId"
  parseJSON _ = error "Invalid login email request JSON"

data Cart
  = Cart
  { cartPayments :: [CartPayment]
  , cartFirstSeatAddedDateTime :: UTCTime
  , cartCartWasPriced :: Bool
  , cartDbStatus :: Int
  , cartSource :: EntitySummary
  , cartBalanceToCharge :: Double
  , cartDeliveryMethod :: EntitySummary
  , cartInitiator :: ConstituentDisplaySummary
  , cartMessage :: [CartPricingRuleMessage]
  , cartOrderCategory :: OrderCategorySummary
  , cartConstituent :: ConstituentDisplaySummary
  , cartCartFirstChoiceAmount :: Double
  , cartAmountPaidPreviously :: Double
  , cartPaymentPlans :: [PaymentPlan]
  , cartCartPrimaryAmount :: Double
  , cartBookingId :: Int
  , cartId :: Int
  , cartAppeal :: EntitySummary
  , cartOrderFees :: [CartFeeDetail]
  , cartSessionKey :: SessionKey
  , cartBatchId :: Int
  , cartModeOfSale :: EntitySummary
  , cartOrderDateTime :: UTCTime
  , cartCartAmount :: Double
  , cartSolicitor :: String
  , cartFeesAmount :: Double
  , cartProducts :: [CartProduct]
  , cartSubTotal :: Double
  , cartAmountPaidNow :: Double
  , cartCustomDataItems :: [CustomDataItem]
  }
  deriving (Eq)

instance ToJSON Cart where
  toJSON Cart{..} = object
    [ "Payments" .= cartPayments
    , "FirstSeatAddedDateTime" .= cartFirstSeatAddedDateTime
    , "CartWasPriced" .= cartCartWasPriced
    , "DbStatus" .= cartDbStatus
    , "Source" .= cartSource
    , "BalanceToCharge" .= cartBalanceToCharge
    , "DeliveryMethod" .= cartDeliveryMethod
    , "Initiator" .= cartInitiator
    , "Message" .= cartMessage
    , "OrderCategory" .= cartOrderCategory
    , "Constituent" .= cartConstituent
    , "CartFirstChoiceAmount" .= cartCartFirstChoiceAmount
    , "AmountPaidPreviously" .= cartAmountPaidPreviously
    , "PaymentPlans" .= cartPaymentPlans
    , "CartPrimaryAmount" .= cartCartPrimaryAmount
    , "BookingId" .= cartBookingId
    , "Id" .= cartId
    , "Appeal" .= cartAppeal
    , "OrderFees" .= cartOrderFees
    , "SessionKey" .= cartSessionKey
    , "BatchId" .= cartBatchId
    , "ModeOfSale" .= cartModeOfSale
    , "OrderDateTime" .= cartOrderDateTime
    , "CartAmount" .= cartCartAmount
    , "Solicitor" .= cartSolicitor
    , "FeesAmount" .= cartFeesAmount
    , "Products" .= cartProducts
    , "SubTotal" .= cartSubTotal
    , "AmountPaidNow" .= cartAmountPaidNow
    , "CustomDataItems" .= cartCustomDataItems
    ]

data CartPayment
  = CartPayment
  { cartpaymentCheckNumber :: String
  , cartpaymentApplied :: Bool
  , cartpaymentPaymentMethod :: PaymentMethod
  , cartpaymentGiftCertificateNumber :: String
  , cartpaymentTenderedAmount :: Double
  , cartpaymentLastFourCreditCardNumber :: String
  , cartpaymentPayerName :: String
  , cartpaymentId :: Int
  , cartpaymentAmount :: Double
  , cartpaymentNotes :: String
  }
  deriving (Eq)

instance ToJSON CartPayment where
  toJSON CartPayment{..} = object
    [ "CheckNumber" .= cartpaymentCheckNumber
    , "Applied" .= cartpaymentApplied
    , "PaymentMethod" .= cartpaymentPaymentMethod
    , "GiftCertificateNumber" .= cartpaymentGiftCertificateNumber
    , "TenderedAmount" .= cartpaymentTenderedAmount
    , "LastFourCreditCardNumber" .= cartpaymentLastFourCreditCardNumber
    , "PayerName" .= cartpaymentPayerName
    , "Id" .= cartpaymentId
    , "Amount" .= cartpaymentAmount
    , "Notes" .= cartpaymentNotes
    ]

data PaymentMethod
  = PaymentMethod
  { paymentmethodNoCopiesOnAuth :: Int
  , paymentmethodRequireCvv :: Bool
  , paymentmethodOpenCashDrawer :: Bool
  , paymentmethodPaymentMethodGroup :: PaymentMethodGroupSummary
  , paymentmethodCanRefund :: Bool
  , paymentmethodCreateLocation :: String
  , paymentmethodControlGroup :: ControlGroupSummary
  , paymentmethodRequireCheckIndicator :: Bool
  , paymentmethodId :: Int
  , paymentmethodMerchantIdForSwipe :: String
  , paymentmethodInactive :: Bool
  , paymentmethodStoreTenderedAmount :: Bool
  , paymentmethodShortDesc :: String
  , paymentmethodDefaultIndicator :: Bool
  , paymentmethodAuthIndicator :: Bool
  , paymentmethodUpdatedBy :: String
  , paymentmethodGiftAidIndicator :: Bool
  , paymentmethodCreatedDateTime :: UTCTime
  , paymentmethodReceiptFormatId :: Int
  , paymentmethodDescription :: String
  , paymentmethodGlAccountId :: String
  , paymentmethodPaymentType :: PaymentTypeSummary
  , paymentmethodAccountType :: AccountTypeSummary
  , paymentmethodIncome :: Bool
  , paymentmethodUseWithCardReader :: Bool
  , paymentmethodMerchantId :: String
  , paymentmethodBusinessUnitId :: Int
  , paymentmethodUpdatedDateTime :: UTCTime
  , paymentmethodCreatedBy :: String
  , paymentmethodCurrencyTypeId :: Int
  , paymentmethodRequirePostalCode :: String
  , paymentmethodNoCopiesOnSave :: Int
  }
  deriving (Eq)

instance ToJSON PaymentMethod where
  toJSON PaymentMethod{..} = object
    [ "NoCopiesOnAuth" .= paymentmethodNoCopiesOnAuth
    , "RequireCvv" .= paymentmethodRequireCvv
    , "OpenCashDrawer" .= paymentmethodOpenCashDrawer
    , "PaymentMethodGroup" .= paymentmethodPaymentMethodGroup
    , "CanRefund" .= paymentmethodCanRefund
    , "CreateLocation" .= paymentmethodCreateLocation
    , "ControlGroup" .= paymentmethodControlGroup
    , "RequireCheckIndicator" .= paymentmethodRequireCheckIndicator
    , "Id" .= paymentmethodId
    , "MerchantIdForSwipe" .= paymentmethodMerchantIdForSwipe
    , "Inactive" .= paymentmethodInactive
    , "StoreTenderedAmount" .= paymentmethodStoreTenderedAmount
    , "ShortDesc" .= paymentmethodShortDesc
    , "DefaultIndicator" .= paymentmethodDefaultIndicator
    , "AuthIndicator" .= paymentmethodAuthIndicator
    , "UpdatedBy" .= paymentmethodUpdatedBy
    , "GiftAidIndicator" .= paymentmethodGiftAidIndicator
    , "CreatedDateTime" .= paymentmethodCreatedDateTime
    , "ReceiptFormatId" .= paymentmethodReceiptFormatId
    , "Description" .= paymentmethodDescription
    , "GlAccountId" .= paymentmethodGlAccountId
    , "PaymentType" .= paymentmethodPaymentType
    , "AccountType" .= paymentmethodAccountType
    , "Income" .= paymentmethodIncome
    , "UseWithCardReader" .= paymentmethodUseWithCardReader
    , "MerchantId" .= paymentmethodMerchantId
    , "BusinessUnitId" .= paymentmethodBusinessUnitId
    , "UpdatedDateTime" .= paymentmethodUpdatedDateTime
    , "CreatedBy" .= paymentmethodCreatedBy
    , "CurrencyTypeId" .= paymentmethodCurrencyTypeId
    , "RequirePostalCode" .= paymentmethodRequirePostalCode
    , "NoCopiesOnSave" .= paymentmethodNoCopiesOnSave
    ]

data PaymentMethodGroupSummary
  = PaymentMethodGroupSummary
  { paymentmethodgroupsummaryId :: Int
  , paymentmethodgroupsummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON PaymentMethodGroupSummary where
  toJSON PaymentMethodGroupSummary{..} = object
    [ "Id" .= paymentmethodgroupsummaryId
    , "Description" .= paymentmethodgroupsummaryDescription
    ]

data ControlGroupSummary
  = ControlGroupSummary
  { controlgroupsummaryId :: Int
  , controlgroupsummaryDescription :: String
  , controlgroupsummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON ControlGroupSummary where
  toJSON ControlGroupSummary{..} = object
    [ "Id" .= controlgroupsummaryId
    , "Description" .= controlgroupsummaryDescription
    , "Inactive" .= controlgroupsummaryInactive
    ]

data PaymentTypeSummary
  = PaymentTypeSummary
  { paymenttypesummaryId :: Int
  , paymenttypesummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON PaymentTypeSummary where
  toJSON PaymentTypeSummary{..} = object
    [ "Id" .= paymenttypesummaryId
    , "Description" .= paymenttypesummaryDescription
    ]

data AccountTypeSummary
  = AccountTypeSummary
  { accounttypesummaryEditMask :: String
  , accounttypesummaryCardtypeIndicator :: String
  , accounttypesummaryDescription :: String
  , accounttypesummaryId :: Int
  , accounttypesummaryCardPrefix :: String
  }
  deriving (Eq)

instance ToJSON AccountTypeSummary where
  toJSON AccountTypeSummary{..} = object
    [ "EditMask" .= accounttypesummaryEditMask
    , "CardtypeIndicator" .= accounttypesummaryCardtypeIndicator
    , "Description" .= accounttypesummaryDescription
    , "Id" .= accounttypesummaryId
    , "CardPrefix" .= accounttypesummaryCardPrefix
    ]

data EntitySummary
  = EntitySummary
  { entitysummaryId :: Int
  , entitysummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON EntitySummary where
  toJSON EntitySummary{..} = object
    [ "Id" .= entitysummaryId
    , "Description" .= entitysummaryDescription
    ]

data ConstituentDisplaySummary
  = ConstituentDisplaySummary
  { constituentdisplaysummaryDisplayName :: String
  , constituentdisplaysummarySortName :: String
  , constituentdisplaysummaryId :: Int
  }
  deriving (Eq)

instance ToJSON ConstituentDisplaySummary where
  toJSON ConstituentDisplaySummary{..} = object
    [ "DisplayName" .= constituentdisplaysummaryDisplayName
    , "SortName" .= constituentdisplaysummarySortName
    , "Id" .= constituentdisplaysummaryId
    ]

data CartPricingRuleMessage
  = CartPricingRuleMessage
  { cartpricingrulemessageId :: Int
  , cartpricingrulemessageIsMessageOnlyRule :: Bool
  , cartpricingrulemessagePricingRule :: CartPricingRuleSummary
  , cartpricingrulemessageMessage :: String
  , cartpricingrulemessageNewRuleIndicator :: String
  , cartpricingrulemessageMessageType :: EntitySummary
  }
  deriving (Eq)

instance ToJSON CartPricingRuleMessage where
  toJSON CartPricingRuleMessage{..} = object
    [ "Id" .= cartpricingrulemessageId
    , "IsMessageOnlyRule" .= cartpricingrulemessageIsMessageOnlyRule
    , "PricingRule" .= cartpricingrulemessagePricingRule
    , "Message" .= cartpricingrulemessageMessage
    , "NewRuleIndicator" .= cartpricingrulemessageNewRuleIndicator
    , "MessageType" .= cartpricingrulemessageMessageType
    ]

data CartPricingRuleSummary
  = CartPricingRuleSummary
  { cartpricingrulesummaryId :: Int
  , cartpricingrulesummaryDescription :: String
  , cartpricingrulesummaryRuleAction :: Int
  }
  deriving (Eq)

instance ToJSON CartPricingRuleSummary where
  toJSON CartPricingRuleSummary{..} = object
    [ "Id" .= cartpricingrulesummaryId
    , "Description" .= cartpricingrulesummaryDescription
    , "RuleAction" .= cartpricingrulesummaryRuleAction
    ]

data OrderCategorySummary
  = OrderCategorySummary
  { ordercategorysummaryId :: Int
  , ordercategorysummaryDescription :: String
  , ordercategorysummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON OrderCategorySummary where
  toJSON OrderCategorySummary{..} = object
    [ "Id" .= ordercategorysummaryId
    , "Description" .= ordercategorysummaryDescription
    , "Inactive" .= ordercategorysummaryInactive
    ]

data PaymentPlan
  = PaymentPlan
  { paymentplanAccountId :: Int
  , paymentplanId :: Int
  , paymentplanCard :: PaymentPlanCard
  , paymentplanAmountDue :: Double
  , paymentplanBillingType :: EntitySummary
  , paymentplanDateDue :: UTCTime
  }
  deriving (Eq)

instance ToJSON PaymentPlan where
  toJSON PaymentPlan{..} = object
    [ "AccountId" .= paymentplanAccountId
    , "Id" .= paymentplanId
    , "Card" .= paymentplanCard
    , "AmountDue" .= paymentplanAmountDue
    , "BillingType" .= paymentplanBillingType
    , "DateDue" .= paymentplanDateDue
    ]

data PaymentPlanCard
  = PaymentPlanCard
  { paymentplancardExpiryYear :: Int
  , paymentplancardNumber :: String
  , paymentplancardExpiryMonth :: Int
  , paymentplancardPaymentGroupId :: Int
  , paymentplancardName :: String
  }
  deriving (Eq)

instance ToJSON PaymentPlanCard where
  toJSON PaymentPlanCard{..} = object
    [ "ExpiryYear" .= paymentplancardExpiryYear
    , "Number" .= paymentplancardNumber
    , "ExpiryMonth" .= paymentplancardExpiryMonth
    , "PaymentGroupId" .= paymentplancardPaymentGroupId
    , "Name" .= paymentplancardName
    ]

data CartFeeDetail
  = CartFeeDetail
  { cartfeedetailLineItemId :: Int
  , cartfeedetailId :: Int
  , cartfeedetailAmount :: Double
  , cartfeedetailFeeSummary :: CartFeeSummary
  , cartfeedetailSubLineItemId :: Int
  , cartfeedetailDbStatus :: Int
  , cartfeedetailOverrideIndicator :: String
  , cartfeedetailOverrideAmount :: Double
  }
  deriving (Eq)

instance ToJSON CartFeeDetail where
  toJSON CartFeeDetail{..} = object
    [ "LineItemId" .= cartfeedetailLineItemId
    , "Id" .= cartfeedetailId
    , "Amount" .= cartfeedetailAmount
    , "FeeSummary" .= cartfeedetailFeeSummary
    , "SubLineItemId" .= cartfeedetailSubLineItemId
    , "DbStatus" .= cartfeedetailDbStatus
    , "OverrideIndicator" .= cartfeedetailOverrideIndicator
    , "OverrideAmount" .= cartfeedetailOverrideAmount
    ]

data CartFeeSummary
  = CartFeeSummary
  { cartfeesummaryFeeId :: Int
  , cartfeesummaryCategory :: EntitySummary
  , cartfeesummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON CartFeeSummary where
  toJSON CartFeeSummary{..} = object
    [ "FeeId" .= cartfeesummaryFeeId
    , "Category" .= cartfeesummaryCategory
    , "Description" .= cartfeesummaryDescription
    ]

data CartProduct
  = CartProduct
  { cartproductPackage :: CartProductPackage
  , cartproductProductClass :: EntitySummary
  , cartproductMembership :: CartMembership
  , cartproductContribution :: CartContribution
  , cartproductUserDefinedFee :: CartFeeDetail
  , cartproductPerformance :: CartProductPerformance
  , cartproductGiftCertificate :: CartGiftCertificate
  , cartproductProductGrouping :: String
  }
  deriving (Eq)

instance ToJSON CartProduct where
  toJSON CartProduct{..} = object
    [ "Package" .= cartproductPackage
    , "ProductClass" .= cartproductProductClass
    , "Membership" .= cartproductMembership
    , "Contribution" .= cartproductContribution
    , "UserDefinedFee" .= cartproductUserDefinedFee
    , "Performance" .= cartproductPerformance
    , "GiftCertificate" .= cartproductGiftCertificate
    , "ProductGrouping" .= cartproductProductGrouping
    ]

data CartProductPackage
  = CartProductPackage
  { cartproductpackageId :: Int
  , cartproductpackageLineItems :: [CartLineItem]
  }
  deriving (Eq)

instance ToJSON CartProductPackage where
  toJSON CartProductPackage{..} = object
    [ "Id" .= cartproductpackageId
    , "LineItems" .= cartproductpackageLineItems
    ]

data CartLineItem
  = CartLineItem
  { cartlineitemPackage :: CartPackage
  , cartlineitemSuperPackageId :: Int
  , cartlineitemLineItemGroupId :: Int
  , cartlineitemPrimary :: Bool
  , cartlineitemSpecialRequest :: SpecialRequest
  , cartlineitemSubLineItems :: [AlternateLineItem]
  , cartlineitemPackLineItemId :: Int
  , cartlineitemDbStatus :: Int
  , cartlineitemSource :: EntitySummary
  , cartlineitemAlternateUpgrade :: String
  , cartlineitemPerformance :: CartPerformance
  , cartlineitemDueAmount :: Double
  , cartlineitemTotalDue :: Double
  , cartlineitemId :: Int
  }
  deriving (Eq)

instance ToJSON CartLineItem where
  toJSON CartLineItem{..} = object
    [ "Package" .= cartlineitemPackage
    , "SuperPackageId" .= cartlineitemSuperPackageId
    , "LineItemGroupId" .= cartlineitemLineItemGroupId
    , "Primary" .= cartlineitemPrimary
    , "SpecialRequest" .= cartlineitemSpecialRequest
    , "SubLineItems" .= cartlineitemSubLineItems
    , "PackLineItemId" .= cartlineitemPackLineItemId
    , "DbStatus" .= cartlineitemDbStatus
    , "Source" .= cartlineitemSource
    , "AlternateUpgrade" .= cartlineitemAlternateUpgrade
    , "Performance" .= cartlineitemPerformance
    , "DueAmount" .= cartlineitemDueAmount
    , "TotalDue" .= cartlineitemTotalDue
    , "Id" .= cartlineitemId
    ]

data CartPackage
  = CartPackage
  { cartpackageNFSPackage :: Bool
  , cartpackageSubPackage :: Bool
  , cartpackageId :: Int
  , cartpackageSuperPackage :: Bool
  , cartpackageFixedPackage :: Bool
  , cartpackageDescription :: String
  , cartpackageType :: EntitySummary
  , cartpackageCode :: String
  , cartpackageDate :: UTCTime
  , cartpackageSeason :: EntitySummary
  }
  deriving (Eq)

instance ToJSON CartPackage where
  toJSON CartPackage{..} = object
    [ "NFSPackage" .= cartpackageNFSPackage
    , "SubPackage" .= cartpackageSubPackage
    , "Id" .= cartpackageId
    , "SuperPackage" .= cartpackageSuperPackage
    , "FixedPackage" .= cartpackageFixedPackage
    , "Description" .= cartpackageDescription
    , "Type" .= cartpackageType
    , "Code" .= cartpackageCode
    , "Date" .= cartpackageDate
    , "Season" .= cartpackageSeason
    ]

data SpecialRequest
  = SpecialRequest
  { specialrequestHoldCode :: Int
  , specialrequestNotes :: String
  , specialrequestStartingRow :: String
  , specialrequestEndingSeat :: String
  , specialrequestAisleSeat :: String
  , specialrequestWheelchairSeats :: Int
  , specialrequestStartPrice :: Double
  , specialrequestStartingSeat :: String
  , specialrequestLeaveSingleSeats :: Bool
  , specialrequestNoStairs :: Bool
  , specialrequestCategory :: EntitySummary
  , specialrequestEndPrice :: Double
  , specialrequestContiguousSeats :: Int
  , specialrequestEndingRow :: String
  }
  deriving (Eq)

instance ToJSON SpecialRequest where
  toJSON SpecialRequest{..} = object
    [ "HoldCode" .= specialrequestHoldCode
    , "Notes" .= specialrequestNotes
    , "StartingRow" .= specialrequestStartingRow
    , "EndingSeat" .= specialrequestEndingSeat
    , "AisleSeat" .= specialrequestAisleSeat
    , "WheelchairSeats" .= specialrequestWheelchairSeats
    , "StartPrice" .= specialrequestStartPrice
    , "StartingSeat" .= specialrequestStartingSeat
    , "LeaveSingleSeats" .= specialrequestLeaveSingleSeats
    , "NoStairs" .= specialrequestNoStairs
    , "Category" .= specialrequestCategory
    , "EndPrice" .= specialrequestEndPrice
    , "ContiguousSeats" .= specialrequestContiguousSeats
    , "EndingRow" .= specialrequestEndingRow
    ]

data AlternateLineItem
  = AlternateLineItem
  { alternatelineitemPrimary :: Bool
  , alternatelineitemSuperPackageId :: Int
  , alternatelineitemLineItemGroupId :: Int
  , alternatelineitemPackage :: CartPackage
  , alternatelineitemId :: Int
  , alternatelineitemTotalDue :: Double
  , alternatelineitemDueAmount :: Double
  , alternatelineitemPerformance :: CartPerformance
  , alternatelineitemAlternateUpgrade :: String
  , alternatelineitemDbStatus :: Int
  , alternatelineitemPackageLineItemId :: Int
  , alternatelineitemSubLineItems :: [CartSubLineItem]
  , alternatelineitemSpecialRequest :: SpecialRequest
  }
  deriving (Eq)

instance ToJSON AlternateLineItem where
  toJSON AlternateLineItem{..} = object
    [ "Primary" .= alternatelineitemPrimary
    , "SuperPackageId" .= alternatelineitemSuperPackageId
    , "LineItemGroupId" .= alternatelineitemLineItemGroupId
    , "Package" .= alternatelineitemPackage
    , "Id" .= alternatelineitemId
    , "TotalDue" .= alternatelineitemTotalDue
    , "DueAmount" .= alternatelineitemDueAmount
    , "Performance" .= alternatelineitemPerformance
    , "AlternateUpgrade" .= alternatelineitemAlternateUpgrade
    , "DbStatus" .= alternatelineitemDbStatus
    , "PackageLineItemId" .= alternatelineitemPackageLineItemId
    , "SubLineItems" .= alternatelineitemSubLineItems
    , "SpecialRequest" .= alternatelineitemSpecialRequest
    ]

data CartPerformance
  = CartPerformance
  { cartperformanceTimeSlot :: EntitySummary
  , cartperformanceDescription :: String
  , cartperformanceType :: EntitySummary
  , cartperformanceProductionSeason :: EntitySummary
  , cartperformanceCode :: String
  , cartperformanceFacility :: EntitySummary
  , cartperformanceSeason :: EntitySummary
  , cartperformanceZoneMap :: CartZoneMap
  , cartperformanceDuration :: Int
  , cartperformanceId :: Int
  , cartperformancePerformanceDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON CartPerformance where
  toJSON CartPerformance{..} = object
    [ "TimeSlot" .= cartperformanceTimeSlot
    , "Description" .= cartperformanceDescription
    , "Type" .= cartperformanceType
    , "ProductionSeason" .= cartperformanceProductionSeason
    , "Code" .= cartperformanceCode
    , "Facility" .= cartperformanceFacility
    , "Season" .= cartperformanceSeason
    , "ZoneMap" .= cartperformanceZoneMap
    , "Duration" .= cartperformanceDuration
    , "Id" .= cartperformanceId
    , "PerformanceDateTime" .= cartperformancePerformanceDateTime
    ]

data CartZoneMap
  = CartZoneMap
  { cartzonemapId :: Int
  , cartzonemapDescription :: String
  , cartzonemapSeatMap :: EntitySummary
  , cartzonemapInactive :: Bool
  }
  deriving (Eq)

instance ToJSON CartZoneMap where
  toJSON CartZoneMap{..} = object
    [ "Id" .= cartzonemapId
    , "Description" .= cartzonemapDescription
    , "SeatMap" .= cartzonemapSeatMap
    , "Inactive" .= cartzonemapInactive
    ]

data CartSubLineItem
  = CartSubLineItem
  { cartsublineitemApplyPricing :: Bool
  , cartsublineitemPaidAmount :: Double
  , cartsublineitemZone :: CartZone
  , cartsublineitemSubLineItemFees :: [CartFeeDetail]
  , cartsublineitemOriginalPriceType :: CartPriceType
  , cartsublineitemRecipient :: ConstituentDisplaySummary
  , cartsublineitemDbStatus :: String
  , cartsublineitemNewRuleIndicator :: String
  , cartsublineitemRuleIndicator :: String
  , cartsublineitemSubLineItemDetails :: [CartSubLineItemDetail]
  , cartsublineitemDueAmount :: Double
  , cartsublineitemPricingRule :: CartPricingRuleSummary
  , cartsublineitemSeat :: CartSeat
  , cartsublineitemId :: Int
  , cartsublineitemPriceType :: CartPriceType
  , cartsublineitemStatusId :: Int
  }
  deriving (Eq)

instance ToJSON CartSubLineItem where
  toJSON CartSubLineItem{..} = object
    [ "ApplyPricing" .= cartsublineitemApplyPricing
    , "PaidAmount" .= cartsublineitemPaidAmount
    , "Zone" .= cartsublineitemZone
    , "SubLineItemFees" .= cartsublineitemSubLineItemFees
    , "OriginalPriceType" .= cartsublineitemOriginalPriceType
    , "Recipient" .= cartsublineitemRecipient
    , "DbStatus" .= cartsublineitemDbStatus
    , "NewRuleIndicator" .= cartsublineitemNewRuleIndicator
    , "RuleIndicator" .= cartsublineitemRuleIndicator
    , "SubLineItemDetails" .= cartsublineitemSubLineItemDetails
    , "DueAmount" .= cartsublineitemDueAmount
    , "PricingRule" .= cartsublineitemPricingRule
    , "Seat" .= cartsublineitemSeat
    , "Id" .= cartsublineitemId
    , "PriceType" .= cartsublineitemPriceType
    , "StatusId" .= cartsublineitemStatusId
    ]

data CartZone
  = CartZone
  { cartzoneShortDescription :: String
  , cartzoneDescription :: String
  , cartzoneZoneTime :: String
  , cartzoneZoneGroupId :: Int
  , cartzoneId :: Int
  , cartzoneRank :: Int
  , cartzoneAbbreviation :: String
  }
  deriving (Eq)

data CartPriceType
  = CartPriceType
  { cartpricetypeDescription :: String
  , cartpricetypeShortDescription :: String
  , cartpricetypePriceTypeReason :: EntitySummary
  , cartpricetypeId :: Int
  , cartpricetypeCategory :: EntitySummary
  }
  deriving (Eq)

instance ToJSON CartPriceType where
  toJSON CartPriceType{..} = object
    [ "Description" .= cartpricetypeDescription
    , "ShortDescription" .= cartpricetypeShortDescription
    , "PriceTypeReason" .= cartpricetypePriceTypeReason
    , "Id" .= cartpricetypeId
    , "Category" .= cartpricetypeCategory
    ]

instance ToJSON CartZone where
  toJSON CartZone{..} = object
    [ "ShortDescription" .= cartzoneShortDescription
    , "Description" .= cartzoneDescription
    , "ZoneTime" .= cartzoneZoneTime
    , "ZoneGroupId" .= cartzoneZoneGroupId
    , "Id" .= cartzoneId
    , "Rank" .= cartzoneRank
    , "Abbreviation" .= cartzoneAbbreviation
    ]

data CartSubLineItemDetail
  = CartSubLineItemDetail
  { cartsublineitemdetailPaidAmount :: Double
  , cartsublineitemdetailBenevolentIndicator :: Bool
  , cartsublineitemdetailDiscountType :: EntitySummary
  , cartsublineitemdetailPerformancePriceTypeLayerId :: Int
  , cartsublineitemdetailCampaignId :: Int
  , cartsublineitemdetailId :: Int
  , cartsublineitemdetailOriginalPrice :: Double
  , cartsublineitemdetailDueAmount :: Double
  }
  deriving (Eq)

instance ToJSON CartSubLineItemDetail where
  toJSON CartSubLineItemDetail{..} = object
    [ "PaidAmount" .= cartsublineitemdetailPaidAmount
    , "BenevolentIndicator" .= cartsublineitemdetailBenevolentIndicator
    , "DiscountType" .= cartsublineitemdetailDiscountType
    , "PerformancePriceTypeLayerId" .= cartsublineitemdetailPerformancePriceTypeLayerId
    , "CampaignId" .= cartsublineitemdetailCampaignId
    , "Id" .= cartsublineitemdetailId
    , "OriginalPrice" .= cartsublineitemdetailOriginalPrice
    , "DueAmount" .= cartsublineitemdetailDueAmount
    ]

data CartSeat
  = CartSeat
  { cartseatId :: Int
  , cartseatSection :: CartSeatSection
  , cartseatNumber :: String
  , cartseatRow :: String
  }
  deriving (Eq)

instance ToJSON CartSeat where
  toJSON CartSeat{..} = object
    [ "Id" .= cartseatId
    , "Section" .= cartseatSection
    , "Number" .= cartseatNumber
    , "Row" .= cartseatRow
    ]

data CartSeatSection
  = CartSeatSection
  { cartseatsectionDescription :: String
  , cartseatsectionShortDescription :: String
  , cartseatsectionPrintDescription :: String
  , cartseatsectionId :: Int
  }
  deriving (Eq)

instance ToJSON CartSeatSection where
  toJSON CartSeatSection{..} = object
    [ "Description" .= cartseatsectionDescription
    , "ShortDescription" .= cartseatsectionShortDescription
    , "PrintDescription" .= cartseatsectionPrintDescription
    , "Id" .= cartseatsectionId
    ]

data CartMembership
  = CartMembership
  { cartmembershipCustomDataItems :: [CustomDataItem]
  , cartmembershipMembershipOrganization :: EntitySummary
  , cartmembershipOnAccountPaymentMethodId :: Int
  , cartmembershipMembershipLevel :: EntitySummary
  , cartmembershipAmount :: Double
  , cartmembershipDbStatus :: Int
  , cartmembershipId :: Int
  , cartmembershipFund :: EntitySummary
  }
  deriving (Eq)

instance ToJSON CartMembership where
  toJSON CartMembership{..} = object
    [ "CustomDataItems" .= cartmembershipCustomDataItems
    , "MembershipOrganization" .= cartmembershipMembershipOrganization
    , "OnAccountPaymentMethodId" .= cartmembershipOnAccountPaymentMethodId
    , "MembershipLevel" .= cartmembershipMembershipLevel
    , "Amount" .= cartmembershipAmount
    , "DbStatus" .= cartmembershipDbStatus
    , "Id" .= cartmembershipId
    , "Fund" .= cartmembershipFund
    ]

data CustomDataItem
  = CustomDataItem
  { customdataitemIndex :: Int
  , customdataitemDataType :: String
  , customdataitemIsDropdown :: Bool
  , customdataitemEditIndicator :: Bool
  , customdataitemDescription :: String
  , customdataitemKeywordId :: Int
  , customdataitemValue :: String
  , customdataitemName :: String
  }
  deriving (Eq)

instance ToJSON CustomDataItem where
  toJSON CustomDataItem{..} = object
    [ "Index" .= customdataitemIndex
    , "DataType" .= customdataitemDataType
    , "IsDropdown" .= customdataitemIsDropdown
    , "EditIndicator" .= customdataitemEditIndicator
    , "Description" .= customdataitemDescription
    , "KeywordId" .= customdataitemKeywordId
    , "Value" .= customdataitemValue
    , "Name" .= customdataitemName
    ]

data CartContribution
  = CartContribution
  { cartcontributionId :: Int
  , cartcontributionFund :: EntitySummary
  , cartcontributionOnAccountPaymentMethodId :: Int
  , cartcontributionDbStatus :: Int
  , cartcontributionAmount :: Double
  , cartcontributionCustomDataItems :: [CustomDataItem]
  }
  deriving (Eq)

instance ToJSON CartContribution where
  toJSON CartContribution{..} = object
    [ "Id" .= cartcontributionId
    , "Fund" .= cartcontributionFund
    , "OnAccountPaymentMethodId" .= cartcontributionOnAccountPaymentMethodId
    , "DbStatus" .= cartcontributionDbStatus
    , "Amount" .= cartcontributionAmount
    , "CustomDataItems" .= cartcontributionCustomDataItems
    ]

data CartProductPerformance
  = CartProductPerformance
  { cartproductperformanceId :: Int
  , cartproductperformanceLineItem :: CartLineItem
  }
  deriving (Eq)

instance ToJSON CartProductPerformance where
  toJSON CartProductPerformance{..} = object
    [ "Id" .= cartproductperformanceId
    , "LineItem" .= cartproductperformanceLineItem
    ]

data CartGiftCertificate
  = CartGiftCertificate
  { cartgiftcertificateGiftCertificateNumber :: String
  , cartgiftcertificatePaymentId :: Int
  , cartgiftcertificateName :: String
  , cartgiftcertificateNotes :: String
  , cartgiftcertificateAmount :: Double
  , cartgiftcertificateApplied :: Bool
  }
  deriving (Eq)

instance ToJSON CartGiftCertificate where
  toJSON CartGiftCertificate{..} = object
    [ "GiftCertificateNumber" .= cartgiftcertificateGiftCertificateNumber
    , "PaymentId" .= cartgiftcertificatePaymentId
    , "Name" .= cartgiftcertificateName
    , "Notes" .= cartgiftcertificateNotes
    , "Amount" .= cartgiftcertificateAmount
    , "Applied" .= cartgiftcertificateApplied
    ]

data ReprintRequest
  = ReprintRequest
  { reprintrequestEmailAddressId :: Int
  , reprintrequestOrderId :: Int
  }
  deriving (Eq)

instance FromJSON ReprintRequest where
  parseJSON (Object req) = ReprintRequest
    <$> req .: "EmailAddressId"
    <*> req .: "OrderId"
  parseJSON _ = error "Invalid reprint request JSON"

data NoBody = NoBody

instance ToJSON NoBody where
  toJSON _ = ""

data WebOrderSearchResponse
  = WebOrderSearchResponse
  { webordersearchresponseTotalDueAmount :: Double
  , webordersearchresponseLockedBySessionKey :: SessionKey
  , webordersearchresponseConstituentId :: Int
  , webordersearchresponseLockedInBatch :: Int
  , webordersearchresponseTotalPaidAmount :: Double
  , webordersearchresponseModeOfSaleId :: Int
  , webordersearchresponseIsRolloverOrder :: Bool
  , webordersearchresponseCreateDate :: UTCTime
  , webordersearchresponseOrderDate :: UTCTime
  , webordersearchresponseOrderId :: Int
  , webordersearchresponseNumberOfUnprintedSeats :: Int
  , webordersearchresponseIsOkToPrint :: Bool
  }
  deriving (Eq)

instance ToJSON WebOrderSearchResponse where
  toJSON WebOrderSearchResponse{..} = object
    [ "webordersearchresponseTotalDueAmount" .= webordersearchresponseTotalDueAmount
    , "LockedBySessionKey" .= webordersearchresponseLockedBySessionKey
    , "ConstituentId" .= webordersearchresponseConstituentId
    , "LockedInBatch" .= webordersearchresponseLockedInBatch
    , "TotalPaidAmount" .= webordersearchresponseTotalPaidAmount
    , "ModeOfSaleId" .= webordersearchresponseModeOfSaleId
    , "IsRolloverOrder" .= webordersearchresponseIsRolloverOrder
    , "CreateDate" .= webordersearchresponseCreateDate
    , "OrderDate" .= webordersearchresponseOrderDate
    , "OrderId" .= webordersearchresponseOrderId
    , "NumberOfUnprintedSeats" .= webordersearchresponseNumberOfUnprintedSeats
    , "IsOkToPrint" .= webordersearchresponseIsOkToPrint
    ]

data SessionVariable
  = SessionVariable
  { sessionvariableValue :: String
  , sessionvariableName :: String
  }
  deriving (Eq)

instance ToJSON SessionVariable where
  toJSON SessionVariable{..} = object
    [ "Value" .= sessionvariableValue
    , "Name" .= sessionvariableName
    ]

data WebPromoCodeRequest
  = WebPromoCodeRequest
  { webpromocoderequestPromoCodeString :: String
  , webpromocoderequestPromoCode :: Int
  }
  deriving (Eq)

instance FromJSON WebPromoCodeRequest where
  parseJSON (Object req) = WebPromoCodeRequest
    <$> req .: "PromoCodeString"
    <*> req .: "PromoCode"
  parseJSON _ = error "Invalid web promo code request JSON"

data WebPromoCode
  = WebPromoCode
  { webpromocodePromotionDate :: UTCTime
  , webpromocodePromoCode :: String
  , webpromocodeModeOfSaleId :: Int
  , webpromocodeSource :: SourceSummary
  , webpromocodeOverrideRankIndicator :: String
  , webpromocodeText1 :: String
  , webpromocodeText2 :: String
  , webpromocodeText3 :: String
  , webpromocodeText4 :: String
  , webpromocodeText5 :: String
  , webpromocodeText6 :: String
  }
  deriving (Eq)

instance ToJSON WebPromoCode where
  toJSON WebPromoCode{..} = object
    [ "PromotionDate" .= webpromocodePromotionDate
    , "PromoCode" .= webpromocodePromoCode
    , "ModeOfSaleId" .= webpromocodeModeOfSaleId
    , "Source" .= webpromocodeSource
    , "OverrideRankIndicator" .= webpromocodeOverrideRankIndicator
    , "Text1" .= webpromocodeText1
    , "Text2" .= webpromocodeText2
    , "Text3" .= webpromocodeText3
    , "Text4" .= webpromocodeText4
    , "Text5" .= webpromocodeText5
    , "Text6" .= webpromocodeText6
    ]

data SourceSummary
  = SourceSummary
  { sourcesummaryId :: Int
  , sourcesummaryAppeal :: AppealSummary
  , sourcesummaryDescription :: String
  , sourcesummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON SourceSummary where
  toJSON SourceSummary{..} = object
    [ "Id" .= sourcesummaryId
    , "Appeal" .= sourcesummaryAppeal
    , "Description" .= sourcesummaryDescription
    , "Inactive" .= sourcesummaryInactive
    ]

data AppealSummary
  = AppealSummary
  { appealsummaryId :: Int
  , appealsummaryDescription :: String
  , appealsummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON AppealSummary where
  toJSON AppealSummary{..} = object
    [ "Id" .= appealsummaryId
    , "Description" .= appealsummaryDescription
    , "Inactive" .= appealsummaryInactive
    ]

data SystemDefaultSummary
  = SystemDefaultSummary
  { systemdefaultsummaryValue :: String
  , systemdefaultsummaryParentTable :: String
  , systemdefaultsummaryInactive :: Bool
  , systemdefaultsummaryId :: Int
  , systemdefaultsummaryFieldName :: String
  }
  deriving (Eq)

instance ToJSON SystemDefaultSummary where
  toJSON SystemDefaultSummary{..} = object
    [ "Value" .= systemdefaultsummaryValue
    , "ParentTable" .= systemdefaultsummaryParentTable
    , "Inactive" .= systemdefaultsummaryInactive
    , "Id" .= systemdefaultsummaryId
    , "FieldName" .= systemdefaultsummaryFieldName
    ]
