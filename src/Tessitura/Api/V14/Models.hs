{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tessitura.Api.V14.Models where

import Data.Aeson
import Data.Time.Clock

type SessionKey = String

data SessionRequest
  = SessionRequest
  { sessionrequestBusinessUnitId :: Int
  , sessionrequestIpAddress :: String
  , sessionrequestOrganization :: String
  }
  deriving (Eq)

instance ToJSON SessionRequest where
  toJSON SessionRequest{..} = object
    [ "BusinessUnitId" .= sessionrequestBusinessUnitId
    , "IpAddress" .= sessionrequestIpAddress
    , "Organization" .= sessionrequestOrganization
    ]

instance FromJSON SessionRequest where
  parseJSON (Object o) = SessionRequest
    <$> o .: "BusinessUnitId"
    <*> o .: "IpAddress"
    <*> o .: "Organization"
  parseJSON _ = error "Invalid SessionRequest JSON"

data SessionResponse
  = SessionResponse
  { sessionresponseSessionKey :: SessionKey }
  deriving (Eq)

instance ToJSON SessionResponse where
  toJSON SessionResponse{..} = object
    [ "SessionKey" .= sessionresponseSessionKey ]

instance FromJSON SessionResponse where
  parseJSON (Object o) = SessionResponse
    <$> o .: "SessionKey"
  parseJSON _ = error "Invalid SessionResponse JSON"

data Session
  = Session
  { sessionBusinessFacing :: Bool
  , sessionCartInfo :: Maybe CartInfo
  , sessionIsLoggedIn :: Bool
  , sessionLoginInfo :: Maybe SessionLoginInfo
  , sessionModeOfSaleId :: Int
  , sessionOrderId :: Int
  , sessionOriginalModeOfSaleId :: Int
  , sessionSessionKey :: SessionKey
  , sessionSourceId :: Int
  }
  deriving (Eq, Show)

instance ToJSON Session where
  toJSON Session{..} = object
    [ "BusinessFacing" .= sessionBusinessFacing
    , "CartInfo" .= sessionCartInfo
    , "IsLoggedIn" .= sessionIsLoggedIn
    , "LoginInfo" .= sessionLoginInfo
    , "ModeOfSaleId" .= sessionModeOfSaleId
    , "OrderId" .= sessionOrderId
    , "OriginalModeOfSaleId" .= sessionOriginalModeOfSaleId
    , "SessionKey" .= sessionSessionKey
    , "SourceId" .= sessionSourceId
    ]

instance FromJSON Session where
  parseJSON (Object o) = Session
    <$> o .: "BusinessFacing"
    <*> o .: "CartInfo"
    <*> o .: "IsLoggedIn"
    <*> o .: "LoginInfo"
    <*> o .: "ModeOfSaleId"
    <*> o .: "OrderId"
    <*> o .: "OriginalModeOfSaleId"
    <*> o .: "SessionKey"
    <*> o .: "SourceId"
  parseJSON _ = error "Invalid Session JSON"

data CartInfo
  = CartInfo
  { cartinfoContributionCount :: Int
  , cartinfoFirstSeatAddedDateTime :: UTCTime
  , cartinfoGiftCertificateCount :: Int
  , cartinfoMembershipCount :: Int
  , cartinfoPackageCount :: Int
  , cartinfoPaymentCount :: Int
  , cartinfoPerformanceCount :: Int
  , cartinfoUserDefinedFeeCount :: Int
  }
  deriving (Eq, Show)

instance ToJSON CartInfo where
  toJSON CartInfo{..} = object
    [ "ContributionCount" .= cartinfoContributionCount
    , "FirstSeatAddedDateTime" .= cartinfoFirstSeatAddedDateTime
    , "GiftCertificateCount" .= cartinfoGiftCertificateCount
    , "MembershipCount" .= cartinfoMembershipCount
    , "PackageCount" .= cartinfoPackageCount
    , "PaymentCount" .= cartinfoPaymentCount
    , "PerformanceCount" .= cartinfoPerformanceCount
    , "UserDefinedFeeCount" .= cartinfoUserDefinedFeeCount
    ]

instance FromJSON CartInfo where
  parseJSON (Object o) = CartInfo
    <$> o .: "ContributionCount"
    <*> o .: "FirstSeatAddedDateTime"
    <*> o .: "GiftCertificateCount"
    <*> o .: "MembershipCount"
    <*> o .: "PackageCount"
    <*> o .: "PaymentCount"
    <*> o .: "PerformanceCount"
    <*> o .: "UserDefinedFeeCount"
  parseJSON _ = error "Invalid CartInfo JSON"

data SessionLoginInfo
  = SessionLoginInfo
  { sessionlogininfoConstituentId :: Int
  , sessionlogininfoElectronicAddress :: String
  , sessionlogininfoFailedAttempts :: Int
  , sessionlogininfoLockedDate :: UTCTime
  , sessionlogininfoOriginalConstituentId :: Int
  , sessionlogininfoStatus :: String
  , sessionlogininfoUserId :: String
  }
  deriving (Eq, Show)

instance ToJSON SessionLoginInfo where
  toJSON SessionLoginInfo{..} = object
    [ "ConstituentId" .= sessionlogininfoConstituentId
    , "ElectronicAddress" .= sessionlogininfoElectronicAddress
    , "FailedAttempts" .= sessionlogininfoFailedAttempts
    , "LockedDate" .= sessionlogininfoLockedDate
    , "OriginalConstituentId" .= sessionlogininfoOriginalConstituentId
    , "Status" .= sessionlogininfoStatus
    , "UserId" .= sessionlogininfoUserId
    ]

instance FromJSON SessionLoginInfo where
  parseJSON (Object o) = SessionLoginInfo
    <$> o .: "ConstituentId"
    <*> o .: "ElectronicAddress"
    <*> o .: "FailedAttempts"
    <*> o .: "LockedDate"
    <*> o .: "OriginalConstituentId"
    <*> o .: "Status"
    <*> o .: "UserId"
  parseJSON _ = error "Invalid SessionLoginInfo JSON"

data LoginRequest
  = LoginRequest
  { loginrequestLoginTypeId :: Int
  , loginrequestPassword :: String
  , loginrequestPromotionCode :: String
  , loginrequestUserName :: String
  }
  deriving (Eq)

instance ToJSON LoginRequest where
  toJSON LoginRequest{..} = object
    [ "LoginTypeId" .= loginrequestLoginTypeId
    , "Password" .= loginrequestPassword
    , "PromotionCode" .= loginrequestPromotionCode
    , "UserName" .= loginrequestUserName
    ]

instance FromJSON LoginRequest where
  parseJSON (Object req) = LoginRequest
    <$> req .: "LoginTypeId"
    <*> req .: "Password"
    <*> req .: "PromotionCode"
    <*> req .: "UserName"
  parseJSON _ = error "Invalid login request JSON"

data OnAccountBalance
  = OnAccountBalance
  { onaccountbalanceBalance :: Double
  , onaccountbalanceConstituentId :: Int
  , onaccountbalanceCurrentBalance :: Double
  , onaccountbalanceDescription :: String
  , onaccountbalancePaymentMethodId :: Int
  , onaccountbalanceUsedInSession :: Double
  }
  deriving (Eq)

instance ToJSON OnAccountBalance where
  toJSON OnAccountBalance{..} = object
    [ "Balance" .= onaccountbalanceBalance
    , "ConstituentId" .= onaccountbalanceConstituentId
    , "CurrentBalance" .= onaccountbalanceCurrentBalance
    , "Description" .= onaccountbalanceDescription
    , "PaymentMethodId" .= onaccountbalancePaymentMethodId
    , "UsedInSession" .= onaccountbalanceUsedInSession
    ]

instance FromJSON OnAccountBalance where
  parseJSON (Object o) = OnAccountBalance
    <$> o .: "Balance"
    <*> o .: "ConstituentId"
    <*> o .: "CurrentBalance"
    <*> o .: "Description"
    <*> o .: "PaymentMethodId"
    <*> o .: "UsedInSession"
  parseJSON _ = error "Invalid OnAccountBalance JSON"

data LoginEmailRequest
  = LoginEmailRequest
  { loginemailrequestEmailAddress :: String
  , loginemailrequestLoginTypeId :: Int
  , loginemailrequestPassword :: String
  , loginemailrequestPromotionCode :: String
  }
  deriving (Eq)

instance ToJSON LoginEmailRequest where
  toJSON LoginEmailRequest{..} = object
    [ "EmailAddress" .= loginemailrequestEmailAddress
    , "LoginTypeId" .= loginemailrequestLoginTypeId
    , "Password" .= loginemailrequestPassword
    , "PromotionCode" .= loginemailrequestPromotionCode
    ]

instance FromJSON LoginEmailRequest where
  parseJSON (Object req) = LoginEmailRequest
    <$> req .: "EmailAddress"
    <*> req .: "LoginTypeId"
    <*> req .: "Password"
    <*> req .: "PromotionCode"
  parseJSON _ = error "Invalid login email request JSON"

data Cart
  = Cart
  { cartId :: Int
  , cartAmountPaidNow :: Double
  , cartAmountPaidPreviously :: Double
  , cartAppeal :: EntitySummary
  , cartBalanceToCharge :: Double
  , cartBatchId :: Int
  , cartBookingId :: Int
  , cartCartAmount :: Double
  , cartCartFirstChoiceAmount :: Double
  , cartCartPrimaryAmount :: Double
  , cartCartWasPriced :: Bool
  , cartConstituent :: ConstituentDisplaySummary
  , cartCustomDataItems :: [CustomDataItem]
  , cartDbStatus :: Int
  , cartDeliveryMethod :: EntitySummary
  , cartFeesAmount :: Double
  , cartFirstSeatAddedDateTime :: UTCTime
  , cartInitiator :: ConstituentDisplaySummary
  , cartMessage :: [CartPricingRuleMessage]
  , cartModeOfSale :: EntitySummary
  , cartOrderCategory :: OrderCategorySummary
  , cartOrderDateTime :: UTCTime
  , cartOrderFees :: [CartFeeDetail]
  , cartPaymentPlans :: [PaymentPlan]
  , cartPayments :: [CartPayment]
  , cartProducts :: [CartProduct]
  , cartSessionKey :: SessionKey
  , cartSolicitor :: String
  , cartSource :: EntitySummary
  , cartSubTotal :: Double
  }
  deriving (Eq)

instance ToJSON Cart where
  toJSON Cart{..} = object
    [ "Id" .= cartId
    , "AmountPaidNow" .= cartAmountPaidNow
    , "AmountPaidPreviously" .= cartAmountPaidPreviously
    , "Appeal" .= cartAppeal
    , "BalanceToCharge" .= cartBalanceToCharge
    , "BatchId" .= cartBatchId
    , "BookingId" .= cartBookingId
    , "CartAmount" .= cartCartAmount
    , "CartFirstChoiceAmount" .= cartCartFirstChoiceAmount
    , "CartPrimaryAmount" .= cartCartPrimaryAmount
    , "CartWasPriced" .= cartCartWasPriced
    , "Constituent" .= cartConstituent
    , "CustomDataItems" .= cartCustomDataItems
    , "DbStatus" .= cartDbStatus
    , "DeliveryMethod" .= cartDeliveryMethod
    , "FeesAmount" .= cartFeesAmount
    , "FirstSeatAddedDateTime" .= cartFirstSeatAddedDateTime
    , "Initiator" .= cartInitiator
    , "Message" .= cartMessage
    , "ModeOfSale" .= cartModeOfSale
    , "OrderCategory" .= cartOrderCategory
    , "OrderDateTime" .= cartOrderDateTime
    , "OrderFees" .= cartOrderFees
    , "PaymentPlans" .= cartPaymentPlans
    , "Payments" .= cartPayments
    , "Products" .= cartProducts
    , "SessionKey" .= cartSessionKey
    , "Solicitor" .= cartSolicitor
    , "Source" .= cartSource
    , "SubTotal" .= cartSubTotal
    ]

instance FromJSON Cart where
  parseJSON (Object o) = Cart
    <$> o .: "Id"
    <*> o .: "AmountPaidNow"
    <*> o .: "AmountPaidPreviously"
    <*> o .: "Appeal"
    <*> o .: "BalanceToCharge"
    <*> o .: "BatchId"
    <*> o .: "BookingId"
    <*> o .: "CartAmount"
    <*> o .: "CartFirstChoiceAmount"
    <*> o .: "CartPrimaryAmount"
    <*> o .: "CartWasPriced"
    <*> o .: "Constituent"
    <*> o .: "CustomDataItems"
    <*> o .: "DbStatus"
    <*> o .: "DeliveryMethod"
    <*> o .: "FeesAmount"
    <*> o .: "FirstSeatAddedDateTime"
    <*> o .: "Initiator"
    <*> o .: "Message"
    <*> o .: "ModeOfSale"
    <*> o .: "OrderCategory"
    <*> o .: "OrderDateTime"
    <*> o .: "OrderFees"
    <*> o .: "PaymentPlans"
    <*> o .: "Payments"
    <*> o .: "Products"
    <*> o .: "SessionKey"
    <*> o .: "Solicitor"
    <*> o .: "Source"
    <*> o .: "SubTotal"
  parseJSON _ = error "Invalid Cart JSON"

data CartPayment
  = CartPayment
  { cartpaymentId :: Int
  , cartpaymentAmount :: Double
  , cartpaymentApplied :: Bool
  , cartpaymentCheckNumber :: String
  , cartpaymentGiftCertificateNumber :: String
  , cartpaymentLastFourCreditCardNumber :: String
  , cartpaymentNotes :: String
  , cartpaymentPayerName :: String
  , cartpaymentPaymentMethod :: PaymentMethod
  , cartpaymentTenderedAmount :: Double
  }
  deriving (Eq)

instance ToJSON CartPayment where
  toJSON CartPayment{..} = object
    [ "Id" .= cartpaymentId
    , "Amount" .= cartpaymentAmount
    , "Applied" .= cartpaymentApplied
    , "CheckNumber" .= cartpaymentCheckNumber
    , "GiftCertificateNumber" .= cartpaymentGiftCertificateNumber
    , "LastFourCreditCardNumber" .= cartpaymentLastFourCreditCardNumber
    , "Notes" .= cartpaymentNotes
    , "PayerName" .= cartpaymentPayerName
    , "PaymentMethod" .= cartpaymentPaymentMethod
    , "TenderedAmount" .= cartpaymentTenderedAmount
    ]

instance FromJSON CartPayment where
  parseJSON (Object o) = CartPayment
    <$> o .: "Id"
    <*> o .: "Amount"
    <*> o .: "Applied"
    <*> o .: "CheckNumber"
    <*> o .: "GiftCertificateNumber"
    <*> o .: "LastFourCreditCardNumber"
    <*> o .: "Notes"
    <*> o .: "PayerName"
    <*> o .: "PaymentMethod"
    <*> o .: "TenderedAmount"
  parseJSON _ = error "Invalid CartPayment JSON"

data PaymentMethod
  = PaymentMethod
  { paymentmethodId :: Int
  , paymentmethodAccountType :: AccountTypeSummary
  , paymentmethodAuthIndicator :: Bool
  , paymentmethodBusinessUnitId :: Int
  , paymentmethodCanRefund :: Bool
  , paymentmethodControlGroup :: ControlGroupSummary
  , paymentmethodCreateLocation :: String
  , paymentmethodCreatedBy :: String
  , paymentmethodCreatedDateTime :: UTCTime
  , paymentmethodCurrencyTypeId :: Int
  , paymentmethodDefaultIndicator :: Bool
  , paymentmethodDescription :: String
  , paymentmethodGiftAidIndicator :: Bool
  , paymentmethodGlAccountId :: String
  , paymentmethodInactive :: Bool
  , paymentmethodIncome :: Bool
  , paymentmethodMerchantId :: String
  , paymentmethodMerchantIdForSwipe :: String
  , paymentmethodNoCopiesOnAuth :: Int
  , paymentmethodNoCopiesOnSave :: Int
  , paymentmethodOpenCashDrawer :: Bool
  , paymentmethodPaymentMethodGroup :: PaymentMethodGroupSummary
  , paymentmethodPaymentType :: PaymentTypeSummary
  , paymentmethodReceiptFormatId :: Int
  , paymentmethodRequireCheckIndicator :: Bool
  , paymentmethodRequireCvv :: Bool
  , paymentmethodRequirePostalCode :: String
  , paymentmethodShortDesc :: String
  , paymentmethodStoreTenderedAmount :: Bool
  , paymentmethodUpdatedBy :: String
  , paymentmethodUpdatedDateTime :: UTCTime
  , paymentmethodUseWithCardReader :: Bool
  }
  deriving (Eq)

instance ToJSON PaymentMethod where
  toJSON PaymentMethod{..} = object
    [ "Id" .= paymentmethodId
    , "AccountType" .= paymentmethodAccountType
    , "AuthIndicator" .= paymentmethodAuthIndicator
    , "BusinessUnitId" .= paymentmethodBusinessUnitId
    , "CanRefund" .= paymentmethodCanRefund
    , "ControlGroup" .= paymentmethodControlGroup
    , "CreateLocation" .= paymentmethodCreateLocation
    , "CreatedBy" .= paymentmethodCreatedBy
    , "CreatedDateTime" .= paymentmethodCreatedDateTime
    , "CurrencyTypeId" .= paymentmethodCurrencyTypeId
    , "DefaultIndicator" .= paymentmethodDefaultIndicator
    , "Description" .= paymentmethodDescription
    , "GiftAidIndicator" .= paymentmethodGiftAidIndicator
    , "GlAccountId" .= paymentmethodGlAccountId
    , "Inactive" .= paymentmethodInactive
    , "Income" .= paymentmethodIncome
    , "MerchantId" .= paymentmethodMerchantId
    , "MerchantIdForSwipe" .= paymentmethodMerchantIdForSwipe
    , "NoCopiesOnAuth" .= paymentmethodNoCopiesOnAuth
    , "NoCopiesOnSave" .= paymentmethodNoCopiesOnSave
    , "OpenCashDrawer" .= paymentmethodOpenCashDrawer
    , "PaymentMethodGroup" .= paymentmethodPaymentMethodGroup
    , "PaymentType" .= paymentmethodPaymentType
    , "ReceiptFormatId" .= paymentmethodReceiptFormatId
    , "RequireCheckIndicator" .= paymentmethodRequireCheckIndicator
    , "RequireCvv" .= paymentmethodRequireCvv
    , "RequirePostalCode" .= paymentmethodRequirePostalCode
    , "ShortDesc" .= paymentmethodShortDesc
    , "StoreTenderedAmount" .= paymentmethodStoreTenderedAmount
    , "UpdatedBy" .= paymentmethodUpdatedBy
    , "UpdatedDateTime" .= paymentmethodUpdatedDateTime
    , "UseWithCardReader" .= paymentmethodUseWithCardReader
    ]

instance FromJSON PaymentMethod where
  parseJSON (Object o) = PaymentMethod
    <$> o .: "Id"
    <*> o .: "AccountType"
    <*> o .: "AuthIndicator"
    <*> o .: "BusinessUnitId"
    <*> o .: "CanRefund"
    <*> o .: "ControlGroup"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "CurrencyTypeId"
    <*> o .: "DefaultIndicator"
    <*> o .: "Description"
    <*> o .: "GiftAidIndicator"
    <*> o .: "GlAccountId"
    <*> o .: "Inactive"
    <*> o .: "Income"
    <*> o .: "MerchantId"
    <*> o .: "MerchantIdForSwipe"
    <*> o .: "NoCopiesOnAuth"
    <*> o .: "NoCopiesOnSave"
    <*> o .: "OpenCashDrawer"
    <*> o .: "PaymentMethodGroup"
    <*> o .: "PaymentType"
    <*> o .: "ReceiptFormatId"
    <*> o .: "RequireCheckIndicator"
    <*> o .: "RequireCvv"
    <*> o .: "RequirePostalCode"
    <*> o .: "ShortDesc"
    <*> o .: "StoreTenderedAmount"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
    <*> o .: "UseWithCardReader"
  parseJSON _ = error "Invalid PaymentMethod JSON"

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

instance FromJSON PaymentMethodGroupSummary where
  parseJSON (Object o) = PaymentMethodGroupSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid PaymentMethodGroupSummary JSON"

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

instance FromJSON ControlGroupSummary where
  parseJSON (Object o) = ControlGroupSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid ControlGroupSummary JSON"

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

instance FromJSON PaymentTypeSummary where
  parseJSON (Object o) = PaymentTypeSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid PaymentTypeSummary JSON"

data AccountTypeSummary
  = AccountTypeSummary
  { accounttypesummaryId :: Int
  , accounttypesummaryCardPrefix :: String
  , accounttypesummaryCardtypeIndicator :: String
  , accounttypesummaryDescription :: String
  , accounttypesummaryEditMask :: String
  }
  deriving (Eq)

instance ToJSON AccountTypeSummary where
  toJSON AccountTypeSummary{..} = object
    [ "Id" .= accounttypesummaryId
    , "CardPrefix" .= accounttypesummaryCardPrefix
    , "CardtypeIndicator" .= accounttypesummaryCardtypeIndicator
    , "Description" .= accounttypesummaryDescription
    , "EditMask" .= accounttypesummaryEditMask
    ]

instance FromJSON AccountTypeSummary where
  parseJSON (Object o) = AccountTypeSummary
    <$> o .: "Id"
    <*> o .: "CardPrefix"
    <*> o .: "CardtypeIndicator"
    <*> o .: "Description"
    <*> o .: "EditMask"
  parseJSON _ = error "Invalid AccountTypeSummary JSON"

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

instance FromJSON EntitySummary where
  parseJSON (Object o) = EntitySummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid EntitySummary JSON"

data ConstituentDisplaySummary
  = ConstituentDisplaySummary
  { constituentdisplaysummaryId :: Int
  , constituentdisplaysummaryDisplayName :: String
  , constituentdisplaysummarySortName :: String
  }
  deriving (Eq)

instance ToJSON ConstituentDisplaySummary where
  toJSON ConstituentDisplaySummary{..} = object
    [ "Id" .= constituentdisplaysummaryId
    , "DisplayName" .= constituentdisplaysummaryDisplayName
    , "SortName" .= constituentdisplaysummarySortName
    ]

instance FromJSON ConstituentDisplaySummary where
  parseJSON (Object o) = ConstituentDisplaySummary
    <$> o .: "Id"
    <*> o .: "DisplayName"
    <*> o .: "SortName"
  parseJSON _ = error "Invalid ConstituentDisplaySummary JSON"

data CartPricingRuleMessage
  = CartPricingRuleMessage
  { cartpricingrulemessageId :: Int
  , cartpricingrulemessageIsMessageOnlyRule :: Bool
  , cartpricingrulemessageMessage :: String
  , cartpricingrulemessageMessageType :: EntitySummary
  , cartpricingrulemessageNewRuleIndicator :: String
  , cartpricingrulemessagePricingRule :: CartPricingRuleSummary
  }
  deriving (Eq)

instance ToJSON CartPricingRuleMessage where
  toJSON CartPricingRuleMessage{..} = object
    [ "Id" .= cartpricingrulemessageId
    , "IsMessageOnlyRule" .= cartpricingrulemessageIsMessageOnlyRule
    , "Message" .= cartpricingrulemessageMessage
    , "MessageType" .= cartpricingrulemessageMessageType
    , "NewRuleIndicator" .= cartpricingrulemessageNewRuleIndicator
    , "PricingRule" .= cartpricingrulemessagePricingRule
    ]

instance FromJSON CartPricingRuleMessage where
  parseJSON (Object o) = CartPricingRuleMessage
    <$> o .: "Id"
    <*> o .: "IsMessageOnlyRule"
    <*> o .: "Message"
    <*> o .: "MessageType"
    <*> o .: "NewRuleIndicator"
    <*> o .: "PricingRule"
  parseJSON _ = error "Invalid CartPricingRuleMessage JSON"

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

instance FromJSON CartPricingRuleSummary where
  parseJSON (Object o) = CartPricingRuleSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "RuleAction"
  parseJSON _ = error "Invalid CartPricingRuleSummary JSON"

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

instance FromJSON OrderCategorySummary where
  parseJSON (Object o) = OrderCategorySummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid OrderCategorySummary JSON"

data PaymentPlan
  = PaymentPlan
  { paymentplanId :: Int
  , paymentplanAccountId :: Int
  , paymentplanAmountDue :: Double
  , paymentplanBillingType :: EntitySummary
  , paymentplanCard :: PaymentPlanCard
  , paymentplanDateDue :: UTCTime
  }
  deriving (Eq)

instance ToJSON PaymentPlan where
  toJSON PaymentPlan{..} = object
    [ "Id" .= paymentplanId
    , "AccountId" .= paymentplanAccountId
    , "AmountDue" .= paymentplanAmountDue
    , "BillingType" .= paymentplanBillingType
    , "Card" .= paymentplanCard
    , "DateDue" .= paymentplanDateDue
    ]

instance FromJSON PaymentPlan where
  parseJSON (Object o) = PaymentPlan
    <$> o .: "Id"
    <*> o .: "AccountId"
    <*> o .: "AmountDue"
    <*> o .: "BillingType"
    <*> o .: "Card"
    <*> o .: "DateDue"
  parseJSON _ = error "Invalid PaymentPlan JSON"

data PaymentPlanCard
  = PaymentPlanCard
  { paymentplancardExpiryMonth :: Int
  , paymentplancardExpiryYear :: Int
  , paymentplancardName :: String
  , paymentplancardNumber :: String
  , paymentplancardPaymentGroupId :: Int
  }
  deriving (Eq)

instance ToJSON PaymentPlanCard where
  toJSON PaymentPlanCard{..} = object
    [ "ExpiryMonth" .= paymentplancardExpiryMonth
    , "ExpiryYear" .= paymentplancardExpiryYear
    , "Name" .= paymentplancardName
    , "Number" .= paymentplancardNumber
    , "PaymentGroupId" .= paymentplancardPaymentGroupId
    ]

instance FromJSON PaymentPlanCard where
  parseJSON (Object o) = PaymentPlanCard
    <$> o .: "ExpiryMonth"
    <*> o .: "ExpiryYear"
    <*> o .: "Name"
    <*> o .: "Number"
    <*> o .: "PaymentGroupId"
  parseJSON _ = error "Invalid PaymentPlanCard JSON"

data CartFeeDetail
  = CartFeeDetail
  { cartfeedetailId :: Int
  , cartfeedetailAmount :: Double
  , cartfeedetailDbStatus :: Int
  , cartfeedetailFeeSummary :: CartFeeSummary
  , cartfeedetailLineItemId :: Int
  , cartfeedetailOverrideAmount :: Double
  , cartfeedetailOverrideIndicator :: String
  , cartfeedetailSubLineItemId :: Int
  }
  deriving (Eq)

instance ToJSON CartFeeDetail where
  toJSON CartFeeDetail{..} = object
    [ "Id" .= cartfeedetailId
    , "Amount" .= cartfeedetailAmount
    , "DbStatus" .= cartfeedetailDbStatus
    , "FeeSummary" .= cartfeedetailFeeSummary
    , "LineItemId" .= cartfeedetailLineItemId
    , "OverrideAmount" .= cartfeedetailOverrideAmount
    , "OverrideIndicator" .= cartfeedetailOverrideIndicator
    , "SubLineItemId" .= cartfeedetailSubLineItemId
    ]

instance FromJSON CartFeeDetail where
  parseJSON (Object o) = CartFeeDetail
    <$> o .: "Id"
    <*> o .: "Amount"
    <*> o .: "DbStatus"
    <*> o .: "FeeSummary"
    <*> o .: "LineItemId"
    <*> o .: "OverrideAmount"
    <*> o .: "OverrideIndicator"
    <*> o .: "SubLineItemId"
  parseJSON _ = error "Invalid CartFeeDetail JSON"

data CartFeeSummary
  = CartFeeSummary
  { cartfeesummaryCategory :: EntitySummary
  , cartfeesummaryDescription :: String
  , cartfeesummaryFeeId :: Int
  }
  deriving (Eq)

instance ToJSON CartFeeSummary where
  toJSON CartFeeSummary{..} = object
    [ "Category" .= cartfeesummaryCategory
    , "Description" .= cartfeesummaryDescription
    , "FeeId" .= cartfeesummaryFeeId
    ]

instance FromJSON CartFeeSummary where
  parseJSON (Object o) = CartFeeSummary
    <$> o .: "Category"
    <*> o .: "Description"
    <*> o .: "FeeId"
  parseJSON _ = error "Invalid CartFeeSummary JSON"

data CartProduct
  = CartProduct
  { cartproductContribution :: CartContribution
  , cartproductGiftCertificate :: CartGiftCertificate
  , cartproductMembership :: CartMembership
  , cartproductPackage :: CartProductPackage
  , cartproductPerformance :: CartProductPerformance
  , cartproductProductClass :: EntitySummary
  , cartproductProductGrouping :: String
  , cartproductUserDefinedFee :: CartFeeDetail
  }
  deriving (Eq)

instance ToJSON CartProduct where
  toJSON CartProduct{..} = object
    [ "Contribution" .= cartproductContribution
    , "GiftCertificate" .= cartproductGiftCertificate
    , "Membership" .= cartproductMembership
    , "Package" .= cartproductPackage
    , "Performance" .= cartproductPerformance
    , "ProductClass" .= cartproductProductClass
    , "ProductGrouping" .= cartproductProductGrouping
    , "UserDefinedFee" .= cartproductUserDefinedFee
    ]

instance FromJSON CartProduct where
  parseJSON (Object o) = CartProduct
    <$> o .: "Contribution"
    <*> o .: "GiftCertificate"
    <*> o .: "Membership"
    <*> o .: "Package"
    <*> o .: "Performance"
    <*> o .: "ProductClass"
    <*> o .: "ProductGrouping"
    <*> o .: "UserDefinedFee"
  parseJSON _ = error "Invalid CartProduct JSON"

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

instance FromJSON CartProductPackage where
  parseJSON (Object o) = CartProductPackage
    <$> o .: "Id"
    <*> o .: "LineItems"
  parseJSON _ = error "Invalid CartProductPackage JSON"

data CartLineItem
  = CartLineItem
  { cartlineitemId :: Int
  , cartlineitemAlternateUpgrade :: String
  , cartlineitemDbStatus :: Int
  , cartlineitemDueAmount :: Double
  , cartlineitemLineItemGroupId :: Int
  , cartlineitemPackLineItemId :: Int
  , cartlineitemPackage :: CartPackage
  , cartlineitemPerformance :: CartPerformance
  , cartlineitemPrimary :: Bool
  , cartlineitemSource :: EntitySummary
  , cartlineitemSpecialRequest :: SpecialRequest
  , cartlineitemSubLineItems :: [AlternateLineItem]
  , cartlineitemSuperPackageId :: Int
  , cartlineitemTotalDue :: Double
  }
  deriving (Eq)

instance ToJSON CartLineItem where
  toJSON CartLineItem{..} = object
    [ "Id" .= cartlineitemId
    , "AlternateUpgrade" .= cartlineitemAlternateUpgrade
    , "DbStatus" .= cartlineitemDbStatus
    , "DueAmount" .= cartlineitemDueAmount
    , "LineItemGroupId" .= cartlineitemLineItemGroupId
    , "PackLineItemId" .= cartlineitemPackLineItemId
    , "Package" .= cartlineitemPackage
    , "Performance" .= cartlineitemPerformance
    , "Primary" .= cartlineitemPrimary
    , "Source" .= cartlineitemSource
    , "SpecialRequest" .= cartlineitemSpecialRequest
    , "SubLineItems" .= cartlineitemSubLineItems
    , "SuperPackageId" .= cartlineitemSuperPackageId
    , "TotalDue" .= cartlineitemTotalDue
    ]

instance FromJSON CartLineItem where
  parseJSON (Object o) = CartLineItem
    <$> o .: "Id"
    <*> o .: "AlternateUpgrade"
    <*> o .: "DbStatus"
    <*> o .: "DueAmount"
    <*> o .: "LineItemGroupId"
    <*> o .: "PackLineItemId"
    <*> o .: "Package"
    <*> o .: "Performance"
    <*> o .: "Primary"
    <*> o .: "Source"
    <*> o .: "SpecialRequest"
    <*> o .: "SubLineItems"
    <*> o .: "SuperPackageId"
    <*> o .: "TotalDue"
  parseJSON _ = error "Invalid CartLineItem JSON"

data CartPackage
  = CartPackage
  { cartpackageId :: Int
  , cartpackageCode :: String
  , cartpackageDate :: UTCTime
  , cartpackageDescription :: String
  , cartpackageFixedPackage :: Bool
  , cartpackageNFSPackage :: Bool
  , cartpackageSeason :: EntitySummary
  , cartpackageSubPackage :: Bool
  , cartpackageSuperPackage :: Bool
  , cartpackageType :: EntitySummary
  }
  deriving (Eq)

instance ToJSON CartPackage where
  toJSON CartPackage{..} = object
    [ "Id" .= cartpackageId
    , "Code" .= cartpackageCode
    , "Date" .= cartpackageDate
    , "Description" .= cartpackageDescription
    , "FixedPackage" .= cartpackageFixedPackage
    , "NFSPackage" .= cartpackageNFSPackage
    , "Season" .= cartpackageSeason
    , "SubPackage" .= cartpackageSubPackage
    , "SuperPackage" .= cartpackageSuperPackage
    , "Type" .= cartpackageType
    ]

instance FromJSON CartPackage where
  parseJSON (Object o) = CartPackage
    <$> o .: "Id"
    <*> o .: "Code"
    <*> o .: "Date"
    <*> o .: "Description"
    <*> o .: "FixedPackage"
    <*> o .: "NFSPackage"
    <*> o .: "Season"
    <*> o .: "SubPackage"
    <*> o .: "SuperPackage"
    <*> o .: "Type"
  parseJSON _ = error "Invalid CartPackage JSON"

data SpecialRequest
  = SpecialRequest
  { specialrequestAisleSeat :: String
  , specialrequestCategory :: EntitySummary
  , specialrequestContiguousSeats :: Int
  , specialrequestEndPrice :: Double
  , specialrequestEndingRow :: String
  , specialrequestEndingSeat :: String
  , specialrequestHoldCode :: Int
  , specialrequestLeaveSingleSeats :: Bool
  , specialrequestNoStairs :: Bool
  , specialrequestNotes :: String
  , specialrequestStartPrice :: Double
  , specialrequestStartingRow :: String
  , specialrequestStartingSeat :: String
  , specialrequestWheelchairSeats :: Int
  }
  deriving (Eq)

instance ToJSON SpecialRequest where
  toJSON SpecialRequest{..} = object
    [ "AisleSeat" .= specialrequestAisleSeat
    , "Category" .= specialrequestCategory
    , "ContiguousSeats" .= specialrequestContiguousSeats
    , "EndPrice" .= specialrequestEndPrice
    , "EndingRow" .= specialrequestEndingRow
    , "EndingSeat" .= specialrequestEndingSeat
    , "HoldCode" .= specialrequestHoldCode
    , "LeaveSingleSeats" .= specialrequestLeaveSingleSeats
    , "NoStairs" .= specialrequestNoStairs
    , "Notes" .= specialrequestNotes
    , "StartPrice" .= specialrequestStartPrice
    , "StartingRow" .= specialrequestStartingRow
    , "StartingSeat" .= specialrequestStartingSeat
    , "WheelchairSeats" .= specialrequestWheelchairSeats
    ]

instance FromJSON SpecialRequest where
  parseJSON (Object o) = SpecialRequest
    <$> o .: "AisleSeat"
    <*> o .: "Category"
    <*> o .: "ContiguousSeats"
    <*> o .: "EndPrice"
    <*> o .: "EndingRow"
    <*> o .: "EndingSeat"
    <*> o .: "HoldCode"
    <*> o .: "LeaveSingleSeats"
    <*> o .: "NoStairs"
    <*> o .: "Notes"
    <*> o .: "StartPrice"
    <*> o .: "StartingRow"
    <*> o .: "StartingSeat"
    <*> o .: "WheelchairSeats"
  parseJSON _ = error "Invalid SpecialRequest JSON"

data AlternateLineItem
  = AlternateLineItem
  { alternatelineitemId :: Int
  , alternatelineitemAlternateUpgrade :: String
  , alternatelineitemDbStatus :: Int
  , alternatelineitemDueAmount :: Double
  , alternatelineitemLineItemGroupId :: Int
  , alternatelineitemPackage :: CartPackage
  , alternatelineitemPackageLineItemId :: Int
  , alternatelineitemPerformance :: CartPerformance
  , alternatelineitemPrimary :: Bool
  , alternatelineitemSpecialRequest :: SpecialRequest
  , alternatelineitemSubLineItems :: [CartSubLineItem]
  , alternatelineitemSuperPackageId :: Int
  , alternatelineitemTotalDue :: Double
  }
  deriving (Eq)

instance ToJSON AlternateLineItem where
  toJSON AlternateLineItem{..} = object
    [ "Id" .= alternatelineitemId
    , "AlternateUpgrade" .= alternatelineitemAlternateUpgrade
    , "DbStatus" .= alternatelineitemDbStatus
    , "DueAmount" .= alternatelineitemDueAmount
    , "LineItemGroupId" .= alternatelineitemLineItemGroupId
    , "Package" .= alternatelineitemPackage
    , "PackageLineItemId" .= alternatelineitemPackageLineItemId
    , "Performance" .= alternatelineitemPerformance
    , "Primary" .= alternatelineitemPrimary
    , "SpecialRequest" .= alternatelineitemSpecialRequest
    , "SubLineItems" .= alternatelineitemSubLineItems
    , "SuperPackageId" .= alternatelineitemSuperPackageId
    , "TotalDue" .= alternatelineitemTotalDue
    ]

instance FromJSON AlternateLineItem where
  parseJSON (Object o) = AlternateLineItem
    <$> o .: "Id"
    <*> o .: "AlternateUpgrade"
    <*> o .: "DbStatus"
    <*> o .: "DueAmount"
    <*> o .: "LineItemGroupId"
    <*> o .: "Package"
    <*> o .: "PackageLineItemId"
    <*> o .: "Performance"
    <*> o .: "Primary"
    <*> o .: "SpecialRequest"
    <*> o .: "SubLineItems"
    <*> o .: "SuperPackageId"
    <*> o .: "TotalDue"
  parseJSON _ = error "Invalid AlternateLineItem JSON"

data CartPerformance
  = CartPerformance
  { cartperformanceId :: Int
  , cartperformanceCode :: String
  , cartperformanceDescription :: String
  , cartperformanceDuration :: Int
  , cartperformanceFacility :: EntitySummary
  , cartperformancePerformanceDateTime :: UTCTime
  , cartperformanceProductionSeason :: EntitySummary
  , cartperformanceSeason :: EntitySummary
  , cartperformanceTimeSlot :: EntitySummary
  , cartperformanceType :: EntitySummary
  , cartperformanceZoneMap :: CartZoneMap
  }
  deriving (Eq)

instance ToJSON CartPerformance where
  toJSON CartPerformance{..} = object
    [ "Id" .= cartperformanceId
    , "Code" .= cartperformanceCode
    , "Description" .= cartperformanceDescription
    , "Duration" .= cartperformanceDuration
    , "Facility" .= cartperformanceFacility
    , "PerformanceDateTime" .= cartperformancePerformanceDateTime
    , "ProductionSeason" .= cartperformanceProductionSeason
    , "Season" .= cartperformanceSeason
    , "TimeSlot" .= cartperformanceTimeSlot
    , "Type" .= cartperformanceType
    , "ZoneMap" .= cartperformanceZoneMap
    ]

instance FromJSON CartPerformance where
  parseJSON (Object o) = CartPerformance
    <$> o .: "Id"
    <*> o .: "Code"
    <*> o .: "Description"
    <*> o .: "Duration"
    <*> o .: "Facility"
    <*> o .: "PerformanceDateTime"
    <*> o .: "ProductionSeason"
    <*> o .: "Season"
    <*> o .: "TimeSlot"
    <*> o .: "Type"
    <*> o .: "ZoneMap"
  parseJSON _ = error "Invalid CartPerformance JSON"

data CartZoneMap
  = CartZoneMap
  { cartzonemapId :: Int
  , cartzonemapDescription :: String
  , cartzonemapInactive :: Bool
  , cartzonemapSeatMap :: EntitySummary
  }
  deriving (Eq)

instance ToJSON CartZoneMap where
  toJSON CartZoneMap{..} = object
    [ "Id" .= cartzonemapId
    , "Description" .= cartzonemapDescription
    , "Inactive" .= cartzonemapInactive
    , "SeatMap" .= cartzonemapSeatMap
    ]

instance FromJSON CartZoneMap where
  parseJSON (Object o) = CartZoneMap
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "SeatMap"
  parseJSON _ = error "Invalid CartZoneMap JSON"

data CartSubLineItem
  = CartSubLineItem
  { cartsublineitemId :: Int
  , cartsublineitemApplyPricing :: Bool
  , cartsublineitemDbStatus :: String
  , cartsublineitemDueAmount :: Double
  , cartsublineitemNewRuleIndicator :: String
  , cartsublineitemOriginalPriceType :: CartPriceType
  , cartsublineitemPaidAmount :: Double
  , cartsublineitemPriceType :: CartPriceType
  , cartsublineitemPricingRule :: CartPricingRuleSummary
  , cartsublineitemRecipient :: ConstituentDisplaySummary
  , cartsublineitemRuleIndicator :: String
  , cartsublineitemSeat :: CartSeat
  , cartsublineitemStatusId :: Int
  , cartsublineitemSubLineItemDetails :: [CartSubLineItemDetail]
  , cartsublineitemSubLineItemFees :: [CartFeeDetail]
  , cartsublineitemZone :: CartZone
  }
  deriving (Eq)

instance ToJSON CartSubLineItem where
  toJSON CartSubLineItem{..} = object
    [ "Id" .= cartsublineitemId
    , "ApplyPricing" .= cartsublineitemApplyPricing
    , "DbStatus" .= cartsublineitemDbStatus
    , "DueAmount" .= cartsublineitemDueAmount
    , "NewRuleIndicator" .= cartsublineitemNewRuleIndicator
    , "OriginalPriceType" .= cartsublineitemOriginalPriceType
    , "PaidAmount" .= cartsublineitemPaidAmount
    , "PriceType" .= cartsublineitemPriceType
    , "PricingRule" .= cartsublineitemPricingRule
    , "Recipient" .= cartsublineitemRecipient
    , "RuleIndicator" .= cartsublineitemRuleIndicator
    , "Seat" .= cartsublineitemSeat
    , "StatusId" .= cartsublineitemStatusId
    , "SubLineItemDetails" .= cartsublineitemSubLineItemDetails
    , "SubLineItemFees" .= cartsublineitemSubLineItemFees
    , "Zone" .= cartsublineitemZone
    ]

instance FromJSON CartSubLineItem where
  parseJSON (Object o) = CartSubLineItem
    <$> o .: "Id"
    <*> o .: "ApplyPricing"
    <*> o .: "DbStatus"
    <*> o .: "DueAmount"
    <*> o .: "NewRuleIndicator"
    <*> o .: "OriginalPriceType"
    <*> o .: "PaidAmount"
    <*> o .: "PriceType"
    <*> o .: "PricingRule"
    <*> o .: "Recipient"
    <*> o .: "RuleIndicator"
    <*> o .: "Seat"
    <*> o .: "StatusId"
    <*> o .: "SubLineItemDetails"
    <*> o .: "SubLineItemFees"
    <*> o .: "Zone"
  parseJSON _ = error "Invalid CartSubLineItem JSON"

data CartZone
  = CartZone
  { cartzoneId :: Int
  , cartzoneAbbreviation :: String
  , cartzoneDescription :: String
  , cartzoneRank :: Int
  , cartzoneShortDescription :: String
  , cartzoneZoneGroupId :: Int
  , cartzoneZoneTime :: String
  }
  deriving (Eq)

instance ToJSON CartZone where
  toJSON CartZone{..} = object
    [ "Id" .= cartzoneId
    , "Abbreviation" .= cartzoneAbbreviation
    , "Description" .= cartzoneDescription
    , "Rank" .= cartzoneRank
    , "ShortDescription" .= cartzoneShortDescription
    , "ZoneGroupId" .= cartzoneZoneGroupId
    , "ZoneTime" .= cartzoneZoneTime
    ]

instance FromJSON CartZone where
  parseJSON (Object o) = CartZone
    <$> o .: "Id"
    <*> o .: "Abbreviation"
    <*> o .: "Description"
    <*> o .: "Rank"
    <*> o .: "ShortDescription"
    <*> o .: "ZoneGroupId"
    <*> o .: "ZoneTime"
  parseJSON _ = error "Invalid CartZone JSON"

data CartPriceType
  = CartPriceType
  { cartpricetypeId :: Int
  , cartpricetypeCategory :: EntitySummary
  , cartpricetypeDescription :: String
  , cartpricetypePriceTypeReason :: EntitySummary
  , cartpricetypeShortDescription :: String
  }
  deriving (Eq)

instance ToJSON CartPriceType where
  toJSON CartPriceType{..} = object
    [ "Id" .= cartpricetypeId
    , "Category" .= cartpricetypeCategory
    , "Description" .= cartpricetypeDescription
    , "PriceTypeReason" .= cartpricetypePriceTypeReason
    , "ShortDescription" .= cartpricetypeShortDescription
    ]

instance FromJSON CartPriceType where
  parseJSON (Object o) = CartPriceType
    <$> o .: "Id"
    <*> o .: "Category"
    <*> o .: "Description"
    <*> o .: "PriceTypeReason"
    <*> o .: "ShortDescription"
  parseJSON _ = error "Invalid CartPriceType JSON"

data CartSubLineItemDetail
  = CartSubLineItemDetail
  { cartsublineitemdetailId :: Int
  , cartsublineitemdetailBenevolentIndicator :: Bool
  , cartsublineitemdetailCampaignId :: Int
  , cartsublineitemdetailDiscountType :: EntitySummary
  , cartsublineitemdetailDueAmount :: Double
  , cartsublineitemdetailOriginalPrice :: Double
  , cartsublineitemdetailPaidAmount :: Double
  , cartsublineitemdetailPerformancePriceTypeLayerId :: Int
  }
  deriving (Eq)

instance ToJSON CartSubLineItemDetail where
  toJSON CartSubLineItemDetail{..} = object
    [ "Id" .= cartsublineitemdetailId
    , "BenevolentIndicator" .= cartsublineitemdetailBenevolentIndicator
    , "CampaignId" .= cartsublineitemdetailCampaignId
    , "DiscountType" .= cartsublineitemdetailDiscountType
    , "DueAmount" .= cartsublineitemdetailDueAmount
    , "OriginalPrice" .= cartsublineitemdetailOriginalPrice
    , "PaidAmount" .= cartsublineitemdetailPaidAmount
    , "PerformancePriceTypeLayerId" .= cartsublineitemdetailPerformancePriceTypeLayerId
    ]

instance FromJSON CartSubLineItemDetail where
  parseJSON (Object o) = CartSubLineItemDetail
    <$> o .: "Id"
    <*> o .: "BenevolentIndicator"
    <*> o .: "CampaignId"
    <*> o .: "DiscountType"
    <*> o .: "DueAmount"
    <*> o .: "OriginalPrice"
    <*> o .: "PaidAmount"
    <*> o .: "PerformancePriceTypeLayerId"
  parseJSON _ = error "Invalid CartSubLineItemDetail JSON"

data CartSeat
  = CartSeat
  { cartseatId :: Int
  , cartseatNumber :: String
  , cartseatRow :: String
  , cartseatSection :: CartSeatSection
  }
  deriving (Eq)

instance ToJSON CartSeat where
  toJSON CartSeat{..} = object
    [ "Id" .= cartseatId
    , "Number" .= cartseatNumber
    , "Row" .= cartseatRow
    , "Section" .= cartseatSection
    ]

instance FromJSON CartSeat where
  parseJSON (Object o) = CartSeat
    <$> o .: "Id"
    <*> o .: "Number"
    <*> o .: "Row"
    <*> o .: "Section"
  parseJSON _ = error "Invalid CartSeat JSON"

data CartSeatSection
  = CartSeatSection
  { cartseatsectionId :: Int
  , cartseatsectionDescription :: String
  , cartseatsectionPrintDescription :: String
  , cartseatsectionShortDescription :: String
  }
  deriving (Eq)

instance ToJSON CartSeatSection where
  toJSON CartSeatSection{..} = object
    [ "Id" .= cartseatsectionId
    , "Description" .= cartseatsectionDescription
    , "PrintDescription" .= cartseatsectionPrintDescription
    , "ShortDescription" .= cartseatsectionShortDescription
    ]

instance FromJSON CartSeatSection where
  parseJSON (Object o) = CartSeatSection
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "PrintDescription"
    <*> o .: "ShortDescription"
  parseJSON _ = error "Invalid CartSeatSection JSON"

data CartMembership
  = CartMembership
  { cartmembershipId :: Int
  , cartmembershipAmount :: Double
  , cartmembershipCustomDataItems :: [CustomDataItem]
  , cartmembershipDbStatus :: Int
  , cartmembershipFund :: EntitySummary
  , cartmembershipMembershipLevel :: EntitySummary
  , cartmembershipMembershipOrganization :: EntitySummary
  , cartmembershipOnAccountPaymentMethodId :: Int
  }
  deriving (Eq)

instance ToJSON CartMembership where
  toJSON CartMembership{..} = object
    [ "Id" .= cartmembershipId
    , "Amount" .= cartmembershipAmount
    , "CustomDataItems" .= cartmembershipCustomDataItems
    , "DbStatus" .= cartmembershipDbStatus
    , "Fund" .= cartmembershipFund
    , "MembershipLevel" .= cartmembershipMembershipLevel
    , "MembershipOrganization" .= cartmembershipMembershipOrganization
    , "OnAccountPaymentMethodId" .= cartmembershipOnAccountPaymentMethodId
    ]

instance FromJSON CartMembership where
  parseJSON (Object o) = CartMembership
    <$> o .: "Id"
    <*> o .: "Amount"
    <*> o .: "CustomDataItems"
    <*> o .: "DbStatus"
    <*> o .: "Fund"
    <*> o .: "MembershipLevel"
    <*> o .: "MembershipOrganization"
    <*> o .: "OnAccountPaymentMethodId"
  parseJSON _ = error "Invalid CartMembership JSON"

data CustomDataItem
  = CustomDataItem
  { customdataitemDataType :: String
  , customdataitemDescription :: String
  , customdataitemEditIndicator :: Bool
  , customdataitemIndex :: Int
  , customdataitemIsDropdown :: Bool
  , customdataitemKeywordId :: Int
  , customdataitemName :: String
  , customdataitemValue :: String
  }
  deriving (Eq)

instance ToJSON CustomDataItem where
  toJSON CustomDataItem{..} = object
    [ "DataType" .= customdataitemDataType
    , "Description" .= customdataitemDescription
    , "EditIndicator" .= customdataitemEditIndicator
    , "Index" .= customdataitemIndex
    , "IsDropdown" .= customdataitemIsDropdown
    , "KeywordId" .= customdataitemKeywordId
    , "Name" .= customdataitemName
    , "Value" .= customdataitemValue
    ]

instance FromJSON CustomDataItem where
  parseJSON (Object o) = CustomDataItem
    <$> o .: "DataType"
    <*> o .: "Description"
    <*> o .: "EditIndicator"
    <*> o .: "Index"
    <*> o .: "IsDropdown"
    <*> o .: "KeywordId"
    <*> o .: "Name"
    <*> o .: "Value"
  parseJSON _ = error "Invalid CustomDataItem JSON"

data CartContribution
  = CartContribution
  { cartcontributionId :: Int
  , cartcontributionAmount :: Double
  , cartcontributionCustomDataItems :: [CustomDataItem]
  , cartcontributionDbStatus :: Int
  , cartcontributionFund :: EntitySummary
  , cartcontributionOnAccountPaymentMethodId :: Int
  }
  deriving (Eq)

instance ToJSON CartContribution where
  toJSON CartContribution{..} = object
    [ "Id" .= cartcontributionId
    , "Amount" .= cartcontributionAmount
    , "CustomDataItems" .= cartcontributionCustomDataItems
    , "DbStatus" .= cartcontributionDbStatus
    , "Fund" .= cartcontributionFund
    , "OnAccountPaymentMethodId" .= cartcontributionOnAccountPaymentMethodId
    ]

instance FromJSON CartContribution where
  parseJSON (Object o) = CartContribution
    <$> o .: "Id"
    <*> o .: "Amount"
    <*> o .: "CustomDataItems"
    <*> o .: "DbStatus"
    <*> o .: "Fund"
    <*> o .: "OnAccountPaymentMethodId"
  parseJSON _ = error "Invalid CartContribution JSON"

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

instance FromJSON CartProductPerformance where
  parseJSON (Object o) = CartProductPerformance
    <$> o .: "Id"
    <*> o .: "LineItem"
  parseJSON _ = error "Invalid CartProductPerformance JSON"

data CartGiftCertificate
  = CartGiftCertificate
  { cartgiftcertificateAmount :: Double
  , cartgiftcertificateApplied :: Bool
  , cartgiftcertificateGiftCertificateNumber :: String
  , cartgiftcertificateName :: String
  , cartgiftcertificateNotes :: String
  , cartgiftcertificatePaymentId :: Int
  }
  deriving (Eq)

instance ToJSON CartGiftCertificate where
  toJSON CartGiftCertificate{..} = object
    [ "Amount" .= cartgiftcertificateAmount
    , "Applied" .= cartgiftcertificateApplied
    , "GiftCertificateNumber" .= cartgiftcertificateGiftCertificateNumber
    , "Name" .= cartgiftcertificateName
    , "Notes" .= cartgiftcertificateNotes
    , "PaymentId" .= cartgiftcertificatePaymentId
    ]

instance FromJSON CartGiftCertificate where
  parseJSON (Object o) = CartGiftCertificate
    <$> o .: "Amount"
    <*> o .: "Applied"
    <*> o .: "GiftCertificateNumber"
    <*> o .: "Name"
    <*> o .: "Notes"
    <*> o .: "PaymentId"
  parseJSON _ = error "Invalid CartGiftCertificate JSON"

data ReprintRequest
  = ReprintRequest
  { reprintrequestEmailAddressId :: Int
  , reprintrequestOrderId :: Int
  }
  deriving (Eq)

instance ToJSON ReprintRequest where
  toJSON ReprintRequest{..} = object
    [ "EmailAddressId" .= reprintrequestEmailAddressId
    , "OrderId" .= reprintrequestOrderId
    ]

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
  { webordersearchresponseConstituentId :: Int
  , webordersearchresponseCreateDate :: UTCTime
  , webordersearchresponseIsOkToPrint :: Bool
  , webordersearchresponseIsRolloverOrder :: Bool
  , webordersearchresponseLockedBySessionKey :: SessionKey
  , webordersearchresponseLockedInBatch :: Int
  , webordersearchresponseModeOfSaleId :: Int
  , webordersearchresponseNumberOfUnprintedSeats :: Int
  , webordersearchresponseOrderDate :: UTCTime
  , webordersearchresponseOrderId :: Int
  , webordersearchresponseTotalDueAmount :: Double
  , webordersearchresponseTotalPaidAmount :: Double
  }
  deriving (Eq)

instance ToJSON WebOrderSearchResponse where
  toJSON WebOrderSearchResponse{..} = object
    [ "ConstituentId" .= webordersearchresponseConstituentId
    , "CreateDate" .= webordersearchresponseCreateDate
    , "IsOkToPrint" .= webordersearchresponseIsOkToPrint
    , "IsRolloverOrder" .= webordersearchresponseIsRolloverOrder
    , "LockedBySessionKey" .= webordersearchresponseLockedBySessionKey
    , "LockedInBatch" .= webordersearchresponseLockedInBatch
    , "ModeOfSaleId" .= webordersearchresponseModeOfSaleId
    , "NumberOfUnprintedSeats" .= webordersearchresponseNumberOfUnprintedSeats
    , "OrderDate" .= webordersearchresponseOrderDate
    , "OrderId" .= webordersearchresponseOrderId
    , "TotalPaidAmount" .= webordersearchresponseTotalPaidAmount
    , "TotalDueAmount" .= webordersearchresponseTotalDueAmount
    ]

instance FromJSON WebOrderSearchResponse where
  parseJSON (Object o) = WebOrderSearchResponse
    <$> o .: "ConstituentId"
    <*> o .: "CreateDate"
    <*> o .: "IsOkToPrint"
    <*> o .: "IsRolloverOrder"
    <*> o .: "LockedBySessionKey"
    <*> o .: "LockedInBatch"
    <*> o .: "ModeOfSaleId"
    <*> o .: "NumberOfUnprintedSeats"
    <*> o .: "OrderDate"
    <*> o .: "OrderId"
    <*> o .: "TotalPaidAmount"
    <*> o .: "TotalDueAmount"
  parseJSON _ = error "Invalid WebOrderSearchResponse JSON"

data SessionVariable
  = SessionVariable
  { sessionvariableName :: String
  , sessionvariableValue :: String
  }
  deriving (Eq)

instance ToJSON SessionVariable where
  toJSON SessionVariable{..} = object
    [ "Name" .= sessionvariableName
    , "Value" .= sessionvariableValue
    ]

instance FromJSON SessionVariable where
  parseJSON (Object o) = SessionVariable
    <$> o .: "Name"
    <*> o .: "Value"
  parseJSON _ = error "Invalid SessionVariable JSON"

data WebPromoCodeRequest
  = WebPromoCodeRequest
  { webpromocoderequestPromoCode :: Int
  , webpromocoderequestPromoCodeString :: String
  }
  deriving (Eq)

instance ToJSON WebPromoCodeRequest where
  toJSON WebPromoCodeRequest{..} = object
    [ "PromoCode" .= webpromocoderequestPromoCode
    , "PromoCodeString" .= webpromocoderequestPromoCodeString
    ]

instance FromJSON WebPromoCodeRequest where
  parseJSON (Object req) = WebPromoCodeRequest
    <$> req .: "PromoCode"
    <*> req .: "PromoCodeString"
  parseJSON _ = error "Invalid web promo code request JSON"

data WebPromoCode
  = WebPromoCode
  { webpromocodeModeOfSaleId :: Int
  , webpromocodeOverrideRankIndicator :: String
  , webpromocodePromoCode :: String
  , webpromocodePromotionDate :: UTCTime
  , webpromocodeSource :: SourceSummary
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
    [ "ModeOfSaleId" .= webpromocodeModeOfSaleId
    , "OverrideRankIndicator" .= webpromocodeOverrideRankIndicator
    , "PromoCode" .= webpromocodePromoCode
    , "PromotionDate" .= webpromocodePromotionDate
    , "Source" .= webpromocodeSource
    , "Text1" .= webpromocodeText1
    , "Text2" .= webpromocodeText2
    , "Text3" .= webpromocodeText3
    , "Text4" .= webpromocodeText4
    , "Text5" .= webpromocodeText5
    , "Text6" .= webpromocodeText6
    ]

instance FromJSON WebPromoCode where
  parseJSON (Object o) = WebPromoCode
    <$> o .: "ModeOfSaleId"
    <*> o .: "OverrideRankIndicator"
    <*> o .: "PromoCode"
    <*> o .: "PromotionDate"
    <*> o .: "Source"
    <*> o .: "Text1"
    <*> o .: "Text2"
    <*> o .: "Text3"
    <*> o .: "Text4"
    <*> o .: "Text5"
    <*> o .: "Text6"
  parseJSON _ = error "Invalid WebPromoCode JSON"

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

instance FromJSON SourceSummary where
  parseJSON (Object o) = SourceSummary
    <$> o .: "Id"
    <*> o .: "Appeal"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid SourceSummary JSON"

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

instance FromJSON AppealSummary where
  parseJSON (Object o) = AppealSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid AppealSummary JSON"

data SystemDefaultSummary
  = SystemDefaultSummary
  { systemdefaultsummaryId :: Int
  , systemdefaultsummaryFieldName :: String
  , systemdefaultsummaryInactive :: Bool
  , systemdefaultsummaryParentTable :: String
  , systemdefaultsummaryValue :: String
  }
  deriving (Eq)

instance ToJSON SystemDefaultSummary where
  toJSON SystemDefaultSummary{..} = object
    [ "Id" .= systemdefaultsummaryId
    , "FieldName" .= systemdefaultsummaryFieldName
    , "Inactive" .= systemdefaultsummaryInactive
    , "ParentTable" .= systemdefaultsummaryParentTable
    , "Value" .= systemdefaultsummaryValue
    ]

instance FromJSON SystemDefaultSummary where
  parseJSON (Object o) = SystemDefaultSummary
    <$> o .: "Id"
    <*> o .: "FieldName"
    <*> o .: "Inactive"
    <*> o .: "ParentTable"
    <*> o .: "Value"
  parseJSON _ = error "Invalid SystemDefaultSummary JSON"

type TitleId = Int

data Title
  = Title
  { titleId :: TitleId
  , titleAuthor :: String
  , titleComposer :: ComposerSummary
  , titleCreateLocation :: String
  , titleCreatedBy :: String
  , titleCreatedDateTime :: UTCTime
  , titleDescription :: String
  , titleEra :: EraSummary
  , titleFullText :: String
  , titleFulltextCompleteDateTime :: UTCTime
  , titleFulltextRequestDateTime :: UTCTime
  , titleInvetoryType :: String
  , titleLib :: String
  , titleOriginalLanguage :: LanguageSummary
  , titleOriginalSynposis :: String
  , titleShortName :: String
  , titleText1 :: String
  , titleText2 :: String
  , titleText3 :: String
  , titleText4 :: String
  , titleUpdatedBy :: String
  , titleUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Title where
  toJSON Title{..} = object
    [ "Id" .= titleId
    , "Author" .= titleAuthor
    , "Composer" .= titleComposer
    , "CreateLocation" .= titleCreateLocation
    , "CreatedBy" .= titleCreatedBy
    , "CreatedDateTime" .= titleCreatedDateTime
    , "Description" .= titleDescription
    , "Era" .= titleEra
    , "FullText" .= titleFullText
    , "FulltextCompleteDateTime" .= titleFulltextCompleteDateTime
    , "FulltextRequestDateTime" .= titleFulltextRequestDateTime
    , "InvetoryType" .= titleInvetoryType
    , "Lib" .= titleLib
    , "OriginalLanguage" .= titleOriginalLanguage
    , "OriginalSynposis" .= titleOriginalSynposis
    , "ShortName" .= titleShortName
    , "Text1" .= titleText1
    , "Text2" .= titleText2
    , "Text3" .= titleText3
    , "Text4" .= titleText4
    , "UpdatedBy" .= titleUpdatedBy
    , "UpdatedDateTime" .= titleUpdatedDateTime
    ]

instance FromJSON Title where
  parseJSON (Object o) = Title
    <$> o .: "Id"
    <*> o .: "Author"
    <*> o .: "Composer"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Era"
    <*> o .: "FullText"
    <*> o .: "FulltextCompleteDateTime"
    <*> o .: "FulltextRequestDateTime"
    <*> o .: "InvetoryType"
    <*> o .: "Lib"
    <*> o .: "OriginalLanguage"
    <*> o .: "OriginalSynposis"
    <*> o .: "ShortName"
    <*> o .: "Text1"
    <*> o .: "Text2"
    <*> o .: "Text3"
    <*> o .: "Text4"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Title JSON"

data TitleSummary
  = TitleSummary
  { titlesummaryId :: TitleId
  , titlesummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON TitleSummary where
  toJSON TitleSummary{..} = object
    [ "Id" .= titlesummaryId
    , "Description" .= titlesummaryDescription
    ]

instance FromJSON TitleSummary where
  parseJSON (Object o) = TitleSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid TitleSummary JSON"

type ComposerId = Int

data Composer
  = Composer
  { composerId :: ComposerId
  , composerBio :: String
  , composerCreateLocation :: String
  , composerCreatedBy :: String
  , composerCreatedDateTime :: UTCTime
  , composerFirstName :: String
  , composerInactive :: Bool
  , composerLastName :: String
  , composerMiddleName :: String
  , composerUpdatedBy :: String
  , composerUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Composer where
  toJSON Composer{..} = object
    [ "Id" .= composerId
    , "Bio" .= composerBio
    , "CreateLocation" .= composerCreateLocation
    , "CreatedBy" .= composerCreatedBy
    , "CreatedDateTime" .= composerCreatedDateTime
    , "FirstName" .= composerFirstName
    , "Inactive" .= composerInactive
    , "LastName" .= composerLastName
    , "MiddleName" .= composerMiddleName
    , "UpdatedBy" .= composerUpdatedBy
    , "UpdatedDateTime" .= composerUpdatedDateTime
    ]

instance FromJSON Composer where
  parseJSON (Object o) = Composer
    <$> o .: "Id"
    <*> o .: "Bio"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "FirstName"
    <*> o .: "Inactive"
    <*> o .: "LastName"
    <*> o .: "MiddleName"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Composer JSON"

data ComposerSummary
  = ComposerSummary
  { composersummaryId :: ComposerId
  , composersummaryDescription :: String
  , composersummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON ComposerSummary where
  toJSON ComposerSummary{..} = object
    [ "Id" .= composersummaryId
    , "Description" .= composersummaryDescription
    , "Invalid" .= composersummaryInactive
    ]

instance FromJSON ComposerSummary where
  parseJSON (Object o) = ComposerSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Invalid"
  parseJSON _ = error "Invalid ComposerSummary JSON"

type LanguageId = Int

data Language
  = Language
  { languageId :: LanguageId
  , languageCreateLocation :: String
  , languageCreatedBy :: String
  , languageCreatedDateTime :: UTCTime
  , languageDescription :: String
  , languageInactive :: Bool
  , languageUpdatedBy :: String
  , languageUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Language where
  toJSON Language{..} = object
    [ "Id" .= languageId
    , "CreateLocation" .= languageCreateLocation
    , "CreatedBy" .= languageCreatedBy
    , "CreatedDateTime" .= languageCreatedDateTime
    , "Description" .= languageDescription
    , "Inactive" .= languageInactive
    , "UpdatedBy" .= languageUpdatedBy
    , "UpdatedDateTime" .= languageUpdatedDateTime
    ]

instance FromJSON Language where
  parseJSON (Object o) = Language
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Language JSON"

data LanguageSummary
  = LanguageSummary
  { languagesummaryId :: LanguageId
  , languagesummaryDescription :: String
  , languagesummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON LanguageSummary where
  toJSON LanguageSummary{..} = object
    [ "Id" .= languagesummaryId
    , "Description" .= languagesummaryDescription
    , "Inactive" .= languagesummaryInactive
    ]

instance FromJSON LanguageSummary where
  parseJSON (Object o) = LanguageSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid LanguageSummary JSON"

type EraId = Int

data Era
  = Era
  { eraId :: EraId
  , eraCreateLocation :: String
  , eraCreatedBy :: String
  , eraCreatedDateTime :: UTCTime
  , eraDescription :: String
  , eraInactive :: Bool
  , eraUpdatedBy :: String
  , eraUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Era where
  toJSON Era{..} = object
    [ "Id" .= eraId
    , "CreateLocation" .= eraCreateLocation
    , "CreatedBy" .= eraCreatedBy
    , "CreatedDateTime" .= eraCreatedDateTime
    , "Description" .= eraDescription
    , "Inactive" .= eraInactive
    , "UpdatedBy" .= eraUpdatedBy
    , "UpdatedDateTime" .= eraUpdatedDateTime
    ]

instance FromJSON Era where
  parseJSON (Object o) = Era
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Era JSON"

data EraSummary
  = EraSummary
  { erasummaryId :: EraId
  , erasummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON EraSummary where
  toJSON EraSummary{..} = object
    [ "Id" .= erasummaryId
    , "Description" .= erasummaryDescription
    ]

instance FromJSON EraSummary where
  parseJSON (Object o) = EraSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid EraSummary JSON"

type ProductionId = Int

data Production
  = Production
  { productionId :: ProductionId
  , productionCost :: Double
  , productionCreateLocation :: String
  , productionCreatedBy :: String
  , productionCreatedDateTime :: UTCTime
  , productionDescription :: String
  , productionDuration :: Int
  , productionFulltext :: String
  , productionFulltextCompleteDateTime :: UTCTime
  , productionFulltextRequestDateTime :: UTCTime
  , productionInventoryType :: String
  , productionLanguage :: LanguageSummary
  , productionNumberOfActs :: Int
  , productionShortName :: String
  , productionSynopsis :: String
  , productionText1 :: String
  , productionText2 :: String
  , productionText3 :: String
  , productionText4 :: String
  , productionTitle :: TitleSummary
  , productionUpdatedBy :: String
  , productionUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Production where
  toJSON Production{..} = object
    [ "Id" .= productionId
    , "Cost" .= productionCost
    , "CreateLocation" .= productionCreateLocation
    , "CreatedBy" .= productionCreatedBy
    , "CreatedDateTime" .= productionCreatedDateTime
    , "Description" .= productionDescription
    , "Duration" .= productionDuration
    , "Fulltext" .= productionFulltext
    , "FulltextCompleteDateTime" .= productionFulltextCompleteDateTime
    , "FulltextRequestDateTime" .= productionFulltextRequestDateTime
    , "InventoryType" .= productionInventoryType
    , "Language" .= productionLanguage
    , "NumberOfActs" .= productionNumberOfActs
    , "ShortName" .= productionShortName
    , "Synopsis" .= productionSynopsis
    , "Text1" .= productionText1
    , "Text2" .= productionText2
    , "Text3" .= productionText3
    , "Text4" .= productionText4
    , "Title" .= productionTitle
    , "UpdatedBy" .= productionUpdatedBy
    , "UpdatedDateTime" .= productionUpdatedDateTime
    ]

instance FromJSON Production where
  parseJSON (Object o) = Production
    <$> o .: "Id"
    <*> o .: "Cost"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Duration"
    <*> o .: "Fulltext"
    <*> o .: "FulltextCompleteDateTime"
    <*> o .: "FulltextRequestDateTime"
    <*> o .: "InventoryType"
    <*> o .: "Language"
    <*> o .: "NumberOfActs"
    <*> o .: "ShortName"
    <*> o .: "Synopsis"
    <*> o .: "Text1"
    <*> o .: "Text2"
    <*> o .: "Text3"
    <*> o .: "Text4"
    <*> o .: "Title"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Production JSON"

data ProductionSummary
  = ProductionSummary
  { productionsummaryId :: ProductionId
  , productionsummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON ProductionSummary where
  toJSON ProductionSummary{..} = object
    [ "Id" .= productionsummaryId
    , "Description" .= productionsummaryDescription
    ]

instance FromJSON ProductionSummary where
  parseJSON (Object o) = ProductionSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid ProductionSummary JSON"

type ProductionSeasonId = Int

data ProductionSeason
  = ProductionSeason
  { productionseasonId :: ProductionSeasonId
  , productionseasonContactPermissionType :: ContactPermissionTypeSummary
  , productionseasonCreateLocation :: String
  , productionseasonCreatedBy :: String
  , productionseasonCreatedDateTime :: UTCTime
  , productionseasonDescription :: String
  , productionseasonFirstPerformanceDate :: UTCTime
  , productionseasonFulltext :: String
  , productionseasonFulltextCompleteDateTime :: UTCTime
  , productionseasonFulltextRequestDateTime :: UTCTime
  , productionseasonInventoryType :: String
  , productionseasonLastPerformanceDate :: UTCTime
  , productionseasonPremiere :: PremiereSummary
  , productionseasonProduction :: ProductionSummary
  , productionseasonSeason :: SeasonSummary
  , productionseasonShortName :: String
  , productionseasonText1 :: String
  , productionseasonText2 :: String
  , productionseasonText3 :: String
  , productionseasonText4 :: String
  , productionseasonUpdatedBy :: String
  , productionseasonUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON ProductionSeason where
  toJSON ProductionSeason{..} = object
    [ "Id" .= productionseasonId
    , "ContactPermissionType" .= productionseasonContactPermissionType
    , "CreateLocation" .= productionseasonCreateLocation
    , "CreatedBy" .= productionseasonCreatedBy
    , "CreatedDateTime" .= productionseasonCreatedDateTime
    , "Description" .= productionseasonDescription
    , "FirstPerformanceDate" .= productionseasonFirstPerformanceDate
    , "Fulltext" .= productionseasonFulltext
    , "FulltextCompleteDateTime" .= productionseasonFulltextCompleteDateTime
    , "FulltextRequestDateTime" .= productionseasonFulltextRequestDateTime
    , "InventoryType" .= productionseasonInventoryType
    , "LastPerformanceDate" .= productionseasonLastPerformanceDate
    , "Premiere" .= productionseasonPremiere
    , "Production" .= productionseasonProduction
    , "Season" .= productionseasonSeason
    , "ShortName" .= productionseasonShortName
    , "Text1" .= productionseasonText1
    , "Text2" .= productionseasonText2
    , "Text3" .= productionseasonText3
    , "Text4" .= productionseasonText4
    , "UpdatedBy" .= productionseasonUpdatedBy
    , "UpdatedDateTime" .= productionseasonUpdatedDateTime
    ]

instance FromJSON ProductionSeason where
  parseJSON (Object o) = ProductionSeason
    <$> o .: "Id"
    <*> o .: "ContactPermissionType"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "FirstPerformanceDate"
    <*> o .: "Fulltext"
    <*> o .: "FulltextCompleteDateTime"
    <*> o .: "FulltextRequestDateTime"
    <*> o .: "InventoryType"
    <*> o .: "LastPerformanceDate"
    <*> o .: "Premiere"
    <*> o .: "Production"
    <*> o .: "Season"
    <*> o .: "ShortName"
    <*> o .: "Text1"
    <*> o .: "Text2"
    <*> o .: "Text3"
    <*> o .: "Text4"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid ProductionSeason JSON"

data ProductionSeasonSummary
  = ProductionSeasonSummary
  { productionseasonsummaryId :: ProductionSeasonId
  , productionseasonsummaryDescription :: String
  , productionseasonsummaryProduction :: ProductionSummary
  , productionseasonsummarySeason :: SeasonSummary
  }
  deriving (Eq)

instance ToJSON ProductionSeasonSummary where
  toJSON ProductionSeasonSummary{..} = object
    [ "Id" .= productionseasonsummaryId
    , "Description" .= productionseasonsummaryDescription
    , "Production" .= productionseasonsummaryProduction
    , "Season" .= productionseasonsummarySeason
    ]

instance FromJSON ProductionSeasonSummary where
  parseJSON (Object o) = ProductionSeasonSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Production"
    <*> o .: "Season"
  parseJSON _ = error "Invalid ProductionSeasonSummary JSON"

type PerformanceId = Int

data Performance
  = Performance
  { performanceId :: PerformanceId
  , performanceAvailSaleIndicator :: Bool
  , performanceBestSeatMap :: BestSeatMapSummary
  , performanceBudgetAmount :: Double
  , performanceCode :: String
  , performanceCreateLocation :: String
  , performanceCreatedBy :: String
  , performanceCreatedDateTime :: UTCTime
  , performanceDate :: UTCTime
  , performanceDefaultEndSaleDateTime :: UTCTime
  , performanceDefaultStartSaleDateTime :: UTCTime
  , performanceDescription :: String
  , performanceDoorsClose :: UTCTime
  , performanceDoorsOpen :: UTCTime
  , performanceDuration :: Int
  , performanceEditIndicator :: Bool
  , performanceFacility :: FacilitySummary
  , performanceProductionSeason :: ProductionSeasonSummary
  , performancePublishClientEndDate :: UTCTime
  , performancePublishClientStartDate :: UTCTime
  , performancePublishWebApiEndDate :: UTCTime
  , performancePublishWebApiStartDate :: UTCTime
  , performanceRankType :: RankTypeSummary
  , performanceSalesNotes :: String
  , performanceSalesNotesRequired :: Bool
  , performanceSeason :: SeasonSummary
  , performanceShortName :: String
  , performanceStatus :: PerformanceStatusSummary
  , performanceText1 :: String
  , performanceText2 :: String
  , performanceText3 :: String
  , performanceText4 :: String
  , performanceTimeSlot :: TimeSlotSummary
  , performanceTvIndicator :: Bool
  , performanceType :: PerformanceTypeSummary
  , performanceUpdatedBy :: String
  , performanceUpdatedDateTime :: UTCTime
  , performanceZoneMap :: ZoneMapSummary
  }
  deriving (Eq)

instance ToJSON Performance where
  toJSON Performance{..} = object
    [ "Id" .= performanceId
    , "AvailSaleIndicator" .= performanceAvailSaleIndicator
    , "BestSeatMap" .= performanceBestSeatMap
    , "BudgetAmount" .= performanceBudgetAmount
    , "Code" .= performanceCode
    , "CreateLocation" .= performanceCreateLocation
    , "CreatedBy" .= performanceCreatedBy
    , "CreatedDateTime" .= performanceCreatedDateTime
    , "Date" .= performanceDate
    , "DefaultEndSaleDateTime" .= performanceDefaultEndSaleDateTime
    , "DefaultStartSaleDateTime" .= performanceDefaultStartSaleDateTime
    , "Description" .= performanceDescription
    , "DoorsClose" .= performanceDoorsClose
    , "DoorsOpen" .= performanceDoorsOpen
    , "Duration" .= performanceDuration
    , "EditIndicator" .= performanceEditIndicator
    , "Facility" .= performanceFacility
    , "ProductionSeason" .= performanceProductionSeason
    , "PublishClientEndDate" .= performancePublishClientEndDate
    , "PublishClientStartDate" .= performancePublishClientStartDate
    , "PublishWebApiEndDate" .= performancePublishWebApiEndDate
    , "PublishWebApiStartDate" .= performancePublishWebApiStartDate
    , "RankType" .= performanceRankType
    , "SalesNotes" .= performanceSalesNotes
    , "SalesNotesRequired" .= performanceSalesNotesRequired
    , "Season" .= performanceSeason
    , "ShortName" .= performanceShortName
    , "Status" .= performanceStatus
    , "Text1" .= performanceText1
    , "Text2" .= performanceText2
    , "Text3" .= performanceText3
    , "Text4" .= performanceText4
    , "TimeSlot" .= performanceTimeSlot
    , "TvIndicator" .= performanceTvIndicator
    , "Type" .= performanceType
    , "UpdatedBy" .= performanceUpdatedBy
    , "UpdatedDateTime" .= performanceUpdatedDateTime
    , "ZoneMap" .= performanceZoneMap
    ]

instance FromJSON Performance where
  parseJSON (Object o) = Performance
    <$> o .: "Id"
    <*> o .: "AvailSaleIndicator"
    <*> o .: "BestSeatMap"
    <*> o .: "BudgetAmount"
    <*> o .: "Code"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Date"
    <*> o .: "EndSaleDateTime"
    <*> o .: "DefaultStartSaleDateTime"
    <*> o .: "Description"
    <*> o .: "DoorsClose"
    <*> o .: "DoorsOpen"
    <*> o .: "Duration"
    <*> o .: "EditIndicator"
    <*> o .: "Facility"
    <*> o .: "ProductionSeason"
    <*> o .: "PublishClientEndDate"
    <*> o .: "PublishClientStartDate"
    <*> o .: "PublishWebApiEndDate"
    <*> o .: "PublishWebApiStartDate"
    <*> o .: "RankType"
    <*> o .: "SalesNotes"
    <*> o .: "SalesNotesRequired"
    <*> o .: "Season"
    <*> o .: "ShortName"
    <*> o .: "Status"
    <*> o .: "Text1"
    <*> o .: "Text2"
    <*> o .: "Text3"
    <*> o .: "Text4"
    <*> o .: "TimeSlot"
    <*> o .: "TvIndicator"
    <*> o .: "Type"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
    <*> o .: "ZoneMap"
  parseJSON _ = error "Invalid Performance JSON"

data PerformanceSummary
  = PerformanceSummary
  { performancesummaryId :: PerformanceId
  , performancesummaryCode :: String
  , performancesummaryDefaultEndSaleDateTime :: UTCTime
  , performancesummaryDefaultStartSaleDateTime :: UTCTime
  , performancesummaryDescription :: String
  , performancesummaryDuration :: Int
  , performancesummaryFacilityId :: FacilityId
  , performancesummaryPerformanceDateTime :: UTCTime
  , performancesummaryProductionSeason :: Entity
  , performancesummarySeason :: SeasonSummary
  , performancesummaryTimeSlot :: TimeSlotSummary
  , performancesummaryZoneMapId :: ZoneMapId
  }
  deriving (Eq)

instance ToJSON PerformanceSummary where
  toJSON PerformanceSummary{..} = object
    [ "Id" .= performancesummaryId
    , "Code" .= performancesummaryCode
    , "DefaultEndSaleDateTime" .= performancesummaryDefaultEndSaleDateTime
    , "DefaultStartSaleDateTime" .= performancesummaryDefaultStartSaleDateTime
    , "Description" .= performancesummaryDescription
    , "Duration" .= performancesummaryDuration
    , "FacilityId" .= performancesummaryFacilityId
    , "PerformanceDateTime" .= performancesummaryPerformanceDateTime
    , "ProductionSeason" .= performancesummaryProductionSeason
    , "Season" .= performancesummarySeason
    , "TimeSlot" .= performancesummaryTimeSlot
    , "ZoneMapId" .= performancesummaryZoneMapId
    ]

instance FromJSON PerformanceSummary where
  parseJSON (Object o) = PerformanceSummary
    <$> o .: "Id"
    <*> o .: "Code"
    <*> o .: "DefaultEndSaleDateTime"
    <*> o .: "DefaultStartSaleDateTime"
    <*> o .: "Description"
    <*> o .: "Duration"
    <*> o .: "FacilityId"
    <*> o .: "PerformanceDateTime"
    <*> o .: "ProductionSeason"
    <*> o .: "Season"
    <*> o .: "TimeSlot"
    <*> o .: "ZoneMapId"
  parseJSON _ = error "Invalid PerformanceSummary JSON"

type SeasonId = Int

data Season
  = Season
  { seasonId :: SeasonId
  , seasonConfirmationNoticeFormat :: Int
  , seasonControlGroup :: ControlGroupSummary
  , seasonCreateLocation :: String
  , seasonCreatedBy :: String
  , seasonCreatedDateTime :: UTCTime
  , seasonDefaultIndicator :: Bool
  , seasonDescription :: String
  , seasonDisplayInSeasonOverview :: Bool
  , seasonEndDateTime :: UTCTime
  , seasonFYear :: Int
  , seasonInactive :: Bool
  , seasonRenewalNoticeFormat :: Int
  , seasonStartDateTime :: UTCTime
  , seasonSubscriptionFund1 :: Int
  , seasonSubscriptionFund2 :: Int
  , seasonType :: SeasonTypeSummary
  , seasonUpdatedBy :: String
  , seasonUpdatedDateTime :: UTCTime
  , seasonYearlySeason :: Int
  }
  deriving (Eq)

instance ToJSON Season where
  toJSON Season{..} = object
    [ "Id" .= seasonId
    , "ConfirmationNoticeFormat" .= seasonConfirmationNoticeFormat
    , "ControlGroup" .= seasonControlGroup
    , "CreateLocation" .= seasonCreateLocation
    , "CreatedBy" .= seasonCreatedBy
    , "CreatedDateTime" .= seasonCreatedDateTime
    , "DefaultIndicator" .= seasonDefaultIndicator
    , "Description" .= seasonDescription
    , "DisplayInSeasonOverview" .= seasonDisplayInSeasonOverview
    , "EndDateTime" .= seasonEndDateTime
    , "FYear" .= seasonFYear
    , "Inactive" .= seasonInactive
    , "RenewalNoticeFormat" .= seasonRenewalNoticeFormat
    , "StartDateTime" .= seasonStartDateTime
    , "SubscriptionFund1" .= seasonSubscriptionFund1
    , "SubscriptionFund2" .= seasonSubscriptionFund2
    , "Type" .= seasonType
    , "UpdatedBy" .= seasonUpdatedBy
    , "UpdatedDateTime" .= seasonUpdatedDateTime
    , "YearlySeason" .= seasonYearlySeason
    ]

instance FromJSON Season where
  parseJSON (Object o) = Season
    <$> o .: "Id"
    <*> o .: "ConfirmationNoticeFormat"
    <*> o .: "ControlGroup"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "DefaultIndicator"
    <*> o .: "Description"
    <*> o .: "DisplayInSeasonOverview"
    <*> o .: "EndDateTime"
    <*> o .: "FYear"
    <*> o .: "Inactive"
    <*> o .: "RenewalNoticeFormat"
    <*> o .: "StartDateTime"
    <*> o .: "SubscriptionFund1"
    <*> o .: "SubscriptionFund2"
    <*> o .: "Type"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
    <*> o .: "YearlySeason"
  parseJSON _ = error "Invalid Season JSON"

data SeasonSummary
  = SeasonSummary
  { seasonsummaryId :: SeasonId
  , seasonsummaryDescription :: String
  , seasonsummaryFYear :: Int
  , seasonsummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON SeasonSummary where
  toJSON SeasonSummary{..} = object
    [ "Id" .= seasonsummaryId
    , "Description" .= seasonsummaryDescription
    , "FYear" .= seasonsummaryFYear
    , "Inactive" .= seasonsummaryInactive
    ]

instance FromJSON SeasonSummary where
  parseJSON (Object o) = SeasonSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "FYear"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid SeasonSummary JSON"

type ContactPermissionTypeId = Int

data ContactPermissionType
  = ContactPermissionType
  { contactpermissiontypeId :: ContactPermissionTypeId
  , contactpermissiontypeCategory :: ContactPermissionCategory
  , contactpermissiontypeCreateLocation :: String
  , contactpermissiontypeCreatedBy :: String
  , contactpermissiontypeCreatedDateTime :: UTCTime
  , contactpermissiontypeDefaultValueForAdd :: String
  , contactpermissiontypeDescription :: String
  , contactpermissiontypeEditIndicator :: Bool
  , contactpermissiontypeInactive :: Bool
  , contactpermissiontypePresenter :: Bool
  , contactpermissiontypeRank :: Int
  , contactpermissiontypeShortDescription :: String
  , contactpermissiontypeUpdatedBy :: String
  , contactpermissiontypeUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON ContactPermissionType where
  toJSON ContactPermissionType{..} = object
    [ "Id" .= contactpermissiontypeId
    , "Category" .= contactpermissiontypeCategory
    , "CreateLocation" .= contactpermissiontypeCreateLocation
    , "CreatedBy" .= contactpermissiontypeCreatedBy
    , "CreatedDateTime" .= contactpermissiontypeCreatedDateTime
    , "DefaultValueForAdd" .= contactpermissiontypeDefaultValueForAdd
    , "Description" .= contactpermissiontypeDescription
    , "EditIndicator" .= contactpermissiontypeEditIndicator
    , "Inactive" .= contactpermissiontypeInactive
    , "Presenter" .= contactpermissiontypePresenter
    , "Rank" .= contactpermissiontypeRank
    , "ShortDescription" .= contactpermissiontypeShortDescription
    , "UpdatedBy" .= contactpermissiontypeUpdatedBy
    , "UpdatedDateTime" .= contactpermissiontypeUpdatedDateTime
    ]

instance FromJSON ContactPermissionType where
  parseJSON (Object o) = ContactPermissionType
    <$> o .: "Id"
    <*> o .: "Category"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "DefaultValueForAdd"
    <*> o .: "Description"
    <*> o .: "EditIndicator"
    <*> o .: "Inactive"
    <*> o .: "Presenter"
    <*> o .: "Rank"
    <*> o .: "ShortDescription"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid ContactPermissionType JSON"

data ContactPermissionTypeSummary
  = ContactPermissionTypeSummary
  { contactpermissiontypesummaryId :: ContactPermissionTypeId
  , contactpermissiontypesummaryCategory :: ContactPermissionCategorySummary
  , contactpermissiontypesummaryDefaultValueForAdd :: String
  , contactpermissiontypesummaryDescription :: String
  , contactpermissiontypesummaryEditIndicator :: Bool
  , contactpermissiontypesummaryInactive :: Bool
  , contactpermissiontypesummaryPresenter :: Bool
  , contactpermissiontypesummaryRank :: Int
  }
  deriving (Eq)

instance ToJSON ContactPermissionTypeSummary where
  toJSON ContactPermissionTypeSummary{..} = object
    [ "Id" .= contactpermissiontypesummaryId
    , "Category" .= contactpermissiontypesummaryCategory
    , "DefaultValueForAdd" .= contactpermissiontypesummaryDefaultValueForAdd
    , "Description" .= contactpermissiontypesummaryDescription
    , "EditIndicator" .= contactpermissiontypesummaryEditIndicator
    , "Inactive" .= contactpermissiontypesummaryInactive
    , "Presenter" .= contactpermissiontypesummaryPresenter
    , "Rank" .= contactpermissiontypesummaryRank
    ]

instance FromJSON ContactPermissionTypeSummary where
  parseJSON (Object o) = ContactPermissionTypeSummary
    <$> o .: "Id"
    <*> o .: "Category"
    <*> o .: "DefaultValueForAdd"
    <*> o .: "Description"
    <*> o .: "EditIndicator"
    <*> o .: "Inactive"
    <*> o .: "Presenter"
    <*> o .:"Rank"
  parseJSON _ = error "Invalid ContactPermissionTypeSummary JSON"

type FacilityId = Int

data Facility
  = Facility
  { facilityId :: FacilityId
  , facilityControlGroup :: ControlGroupSummary
  , facilityCreateLocation :: String
  , facilityCreatedBy :: String
  , facilityCreatedDateTime :: UTCTime
  , facilityDefaultBestSeatMapId :: SeatMapId
  , facilityDefaultZoneMapId :: Int
  , facilityDescription :: String
  , facilitySeatMap :: SeatMap
  , facilityTheater :: TheaterSummary
  , facilityUpdatedBy :: String
  , facilityUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Facility where
  toJSON Facility {..} = object
    [ "Id" .= facilityId
    , "ControlGroup" .= facilityControlGroup
    , "CreateLocation" .= facilityCreateLocation
    , "CreatedBy" .= facilityCreatedBy
    , "CreatedDateTime" .= facilityCreatedDateTime
    , "DefaultBestSeatMapId" .= facilityDefaultBestSeatMapId
    , "DefaultZoneMapId" .= facilityDefaultZoneMapId
    , "Description" .= facilityDescription
    , "SeatMap" .= facilitySeatMap
    , "Theater" .= facilityTheater
    , "UpdatedBy" .= facilityUpdatedBy
    , "UpdatedDateTime" .= facilityUpdatedDateTime
    ]

instance FromJSON Facility where
  parseJSON (Object o) = Facility
    <$> o .: "Id"
    <*> o .: "ControlGroup"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "DefaultBestSeatMapId"
    <*> o .: "DefaultZoneMapId"
    <*> o .: "Description"
    <*> o .: "SeatMap"
    <*> o .: "Theater"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Facility JSON"

data FacilitySummary
  = FacilitySummary
  { facilitysummaryId :: FacilityId
  , facilitysummaryDescription :: String
  , facilitysummarySeatMap :: SeatMapSummary
  }
  deriving (Eq)

instance ToJSON FacilitySummary where
  toJSON FacilitySummary{..} = object
    [ "Id" .= facilitysummaryId
    , "Description" .= facilitysummaryDescription
    , "SeatMap" .= facilitysummarySeatMap
    ]

instance FromJSON FacilitySummary where
  parseJSON (Object o) = FacilitySummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "SeatMap"
  parseJSON _ = error "Invalid FacilitySummary JSON"

type PremiereId = Int

data Premiere
  = Premiere
  { premiereId :: PremiereId
  , premiereCreateLocation :: String
  , premiereCreatedBy :: String
  , premiereCreatedDateTime :: UTCTime
  , premiereDescription :: String
  , premiereInactive :: Bool
  , premiereUpdatedBy :: String
  , premiereUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Premiere where
  toJSON Premiere{..} = object
    [ "Id" .= premiereId
    , "CreateLocation" .= premiereCreateLocation
    , "CreatedBy" .= premiereCreatedBy
    , "CreatedDateTime" .= premiereCreatedDateTime
    , "Description" .= premiereDescription
    , "Inactive" .= premiereInactive
    , "UpdatedBy" .= premiereUpdatedBy
    , "UpdatedDateTime" .= premiereUpdatedDateTime
    ]

instance FromJSON Premiere where
  parseJSON (Object o) = Premiere
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Premiere JSON"

data PremiereSummary
  = PremiereSummary
  { premieresummaryId :: PremiereId
  , premieresummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON PremiereSummary where
  toJSON PremiereSummary{..} = object
    [ "Id" .= premieresummaryId
    , "Description" .= premieresummaryDescription
    ]

instance FromJSON PremiereSummary where
  parseJSON (Object o) = PremiereSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid PremiereSummary JSON"

type SeatMapId = Int

data SeatMap
  = SeatMap
  { seatmapId :: SeatMapId
  , seatmapCreateLocation :: String
  , seatmapCreatedBy :: String
  , seatmapCreatedDateTime :: UTCTime
  , seatmapDescription :: String
  , seatmapUpdatedBy :: String
  , seatmapUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON SeatMap where
  toJSON SeatMap{..} = object
    [ "Id" .= seatmapId
    , "CreateLocation" .= seatmapCreateLocation
    , "CreatedBy" .= seatmapCreatedBy
    , "CreatedDateTime" .= seatmapCreatedDateTime
    , "Description" .= seatmapDescription
    , "UpdatedBy" .= seatmapUpdatedBy
    , "UpdatedDateTime" .= seatmapUpdatedDateTime
    ]

instance FromJSON SeatMap where
  parseJSON (Object o) = SeatMap
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid SeatMap JSON"

data SeatMapSummary
  = SeatMapSummary
  { seatmapsummaryId :: SeatMapId
  , seatmapsummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON SeatMapSummary where
  toJSON SeatMapSummary{..} = object
    [ "Id" .= seatmapsummaryId
    , "Description" .= seatmapsummaryDescription
    ]

instance FromJSON SeatMapSummary where
  parseJSON (Object o) = SeatMapSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid SeatMapSummary JSON"

data BestSeatMapSummary
  = BestSeatMapSummary
  { bestseatmapsummaryId :: SeatMapId
  , bestseatmapsummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON BestSeatMapSummary where
  toJSON BestSeatMapSummary{..} = object
    [ "Id" .= bestseatmapsummaryId
    , "Description" .= bestseatmapsummaryDescription
    ]

instance FromJSON BestSeatMapSummary where
  parseJSON (Object o) = BestSeatMapSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid SeatMapSummary JSON"

type RankTypeId = Int

data RankType
  = RankType
  { ranktypeId :: RankTypeId
  , ranktypeControlGroup :: ControlGroupSummary
  , ranktypeCreateLocation :: String
  , ranktypeCreatedBy :: String
  , ranktypeCreatedDateTime :: UTCTime
  , ranktypeDescription :: String
  , ranktypeInactive :: Bool
  , ranktypeUpdatedBy :: String
  , ranktypeUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON RankType where
  toJSON RankType{..} = object
    [ "Id" .= ranktypeId
    , "ControlGroup" .= ranktypeControlGroup
    , "CreateLocation" .= ranktypeCreateLocation
    , "CreatedBy" .= ranktypeCreatedBy
    , "CreatedDateTime" .= ranktypeCreatedDateTime
    , "Description" .= ranktypeDescription
    , "Inactive" .= ranktypeInactive
    , "UpdatedBy" .= ranktypeUpdatedBy
    , "UpdatedDateTime" .= ranktypeUpdatedDateTime
    ]

instance FromJSON RankType where
  parseJSON (Object o) = RankType
    <$> o .: "Id"
    <*> o .: "ControlGroup"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid RankType JSON"

data RankTypeSummary
  = RankTypeSummary
  { ranktypesummaryId :: RankTypeId
  , ranktypesummaryControlGroup :: ConstituentGroupSummary
  , ranktypesummaryDescription :: String
  , ranktypesummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON RankTypeSummary where
  toJSON RankTypeSummary{..} = object
    [ "Id" .= ranktypesummaryId
    , "ControlGroup" .= ranktypesummaryControlGroup
    , "Description" .= ranktypesummaryDescription
    , "Inactive" .= ranktypesummaryInactive
    ]

instance FromJSON RankTypeSummary where
  parseJSON (Object o) = RankTypeSummary
    <$> o .: "Id"
    <*> o .: "ControlGroup"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid RankTypeSummary JSON"

type PerformanceStatusId = Int

data PerformanceStatus
  = PerformanceStatus
  { performancestatusId :: PerformanceStatusId
  , performancestatusCreateLocation :: String
  , performancestatusCreatedBy :: String
  , performancestatusCreatedDateTime :: UTCTime
  , performancestatusDescription :: String
  , performancestatusInactive :: Bool
  , performancestatusUpdatedBy :: String
  , performancestatusUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON PerformanceStatus where
  toJSON PerformanceStatus{..} = object
    [ "Id" .= performancestatusId
    , "CreateLocation" .= performancestatusCreateLocation
    , "CreatedBy" .= performancestatusCreatedBy
    , "CreatedDateTime" .= performancestatusCreatedDateTime
    , "Description" .= performancestatusDescription
    , "Inactive" .= performancestatusInactive
    , "UpdatedBy" .= performancestatusUpdatedBy
    , "UpdatedDateTime" .= performancestatusUpdatedDateTime
    ]

instance FromJSON PerformanceStatus where
  parseJSON (Object o) = PerformanceStatus
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid PerformanceStatus JSON"

data PerformanceStatusSummary
  = PerformanceStatusSummary
  { performancestatussummaryId :: PerformanceStatusId
  , performancestatussummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON PerformanceStatusSummary where
  toJSON PerformanceStatusSummary{..} = object
    [ "Id" .= performancestatussummaryId
    , "Description" .= performancestatussummaryDescription
    ]

instance FromJSON PerformanceStatusSummary where
  parseJSON (Object o) = PerformanceStatusSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid PerformanceStatusSummary JSON"

type TimeSlotId = Int

data TimeSlot
  = TimeSlot
  { timeslotId :: TimeSlotId
  , timeslotCreateLocation :: String
  , timeslotCreatedBy :: String
  , timeslotCreatedDateTime :: UTCTime
  , timeslotDescription :: String
  , timeslotEndTime :: UTCTime
  , timeslotInactive :: Bool
  , timeslotStartTime :: UTCTime
  , timeslotUpdatedBy :: String
  , timeslotUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON TimeSlot where
  toJSON TimeSlot{..} = object
    [ "Id" .= timeslotId
    , "CreateLocation" .= timeslotCreateLocation
    , "CreatedBy" .= timeslotCreatedBy
    , "CreatedDateTime" .= timeslotCreatedDateTime
    , "Description" .= timeslotDescription
    , "EndTime" .= timeslotEndTime
    , "Inactive" .= timeslotInactive
    , "StartTime" .= timeslotStartTime
    , "UpdatedBy" .= timeslotUpdatedBy
    , "UpdatedDateTime" .= timeslotUpdatedDateTime
    ]

instance FromJSON TimeSlot where
  parseJSON (Object o) = TimeSlot
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "EndTime"
    <*> o .: "Inactive"
    <*> o .: "StartTime"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid TimeSlot JSON"

data TimeSlotSummary
  = TimeSlotSummary
  { timeslotsummaryId :: TimeSlotId
  , timeslotsummaryDescription :: String
  , timeslotsummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON TimeSlotSummary where
  toJSON TimeSlotSummary{..} = object
    [ "Id" .= timeslotsummaryId
    , "Description" .= timeslotsummaryDescription
    , "Inactive" .= timeslotsummaryInactive
    ]

instance FromJSON TimeSlotSummary where
  parseJSON (Object o) = TimeSlotSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid TimeSlotSummary JSON"

type PerformanceTypeId = Int

data PerformanceType
  = PerformanceType
  { performancetypeId :: PerformanceTypeId
  , performancetypeCreateLocation :: String
  , performancetypeCreatedBy :: String
  , performancetypeCreatedDateTime :: UTCTime
  , performancetypeDescription :: String
  , performancetypeInactive :: Bool
  , performancetypeUpdatedBy :: String
  , performancetypeUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON PerformanceType where
  toJSON PerformanceType{..} = object
    [ "Id" .= performancetypeId
    , "CreateLocation" .= performancetypeCreateLocation
    , "CreatedBy" .= performancetypeCreatedBy
    , "CreatedDateTime" .= performancetypeCreatedDateTime
    , "Description" .= performancetypeDescription
    , "Inactive" .= performancetypeInactive
    , "UpdatedBy" .= performancetypeUpdatedBy
    , "UpdatedDateTime" .= performancetypeUpdatedDateTime
    ]

instance FromJSON PerformanceType where
  parseJSON (Object o) = PerformanceType
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid PerformanceType JSON"

data PerformanceTypeSummary
  = PerformanceTypeSummary
  { performancetypesummaryId :: PerformanceTypeId
  , performancetypesummaryDescription :: String
  , performancetypesummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON PerformanceTypeSummary where
  toJSON PerformanceTypeSummary{..} = object
    [ "Id" .= performancetypesummaryId
    , "Description" .= performancetypesummaryDescription
    , "Inactive" .= performancetypesummaryInactive
    ]

instance FromJSON PerformanceTypeSummary where
  parseJSON (Object o) = PerformanceTypeSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid PerformanceTypeSummary JSON"

type ZoneMapId = Int

data ZoneMap
  = ZoneMap
  { zonemapId :: ZoneMapId
  , zonemapCreateLocation :: String
  , zonemapCreatedBy :: String
  , zonemapCreatedDateTime :: UTCTime
  , zonemapDescription :: String
  , zonemapInactive :: Bool
  , zonemapSeatMap :: SeatMapSummary
  , zonemapUpdatedBy :: String
  , zonemapUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON ZoneMap where
  toJSON ZoneMap{..} = object
    [ "Id" .= zonemapId
    , "CreateLocation" .= zonemapCreateLocation
    , "CreatedBy" .= zonemapCreatedBy
    , "CreatedDateTime" .= zonemapCreatedDateTime
    , "Description" .= zonemapDescription
    , "Inactive" .= zonemapInactive
    , "SeatMap" .= zonemapSeatMap
    , "UpdatedBy" .= zonemapUpdatedBy
    , "UpdatedDateTime" .= zonemapUpdatedDateTime
    ]

instance FromJSON ZoneMap where
  parseJSON (Object o) = ZoneMap
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "SeatMap"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid ZoneMap JSON"

data ZoneMapSummary
  = ZoneMapSummary
  { zonemapsummaryId :: ZoneMapId
  , zonemapsummaryDescription :: String
  , zonemapsummaryInactive :: Bool
  , zonemapsummarySeatMap :: SeatMapSummary
  }
  deriving (Eq)

instance ToJSON ZoneMapSummary where
  toJSON ZoneMapSummary{..} = object
    [ "Id" .= zonemapsummaryId
    , "Description" .= zonemapsummaryDescription
    , "Inactive" .= zonemapsummaryInactive
    , "SeatMap" .= zonemapsummarySeatMap
    ]

instance FromJSON ZoneMapSummary where
  parseJSON (Object o) = ZoneMapSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "SeatMap"
  parseJSON _ = error "Invalid ZoneMapSummary JSON"

type SeasonTypeId = Int

data SeasonType
  = SeasonType
  { seasontypeId :: SeasonTypeId
  , seasontypeBusinessUnit :: BusinessUnitSummary
  , seasontypeCreateLocation :: String
  , seasontypeCreatedBy :: String
  , seasontypeCreatedDateTime :: UTCTime
  , seasontypeDescription :: String
  , seasontypeInactive :: Bool
  , seasontypeUpdatedBy :: String
  , seasontypeUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON SeasonType where
  toJSON SeasonType{..} = object
    [ "Id" .= seasontypeId
    , "BusinessUnit" .= seasontypeBusinessUnit
    , "CreateLocation" .= seasontypeCreateLocation
    , "CreatedBy" .= seasontypeCreatedBy
    , "CreatedDateTime" .= seasontypeCreatedDateTime
    , "Description" .= seasontypeDescription
    , "Inactive" .= seasontypeInactive
    , "UpdatedBy" .= seasontypeUpdatedBy
    , "UpdatedDateTime" .= seasontypeUpdatedDateTime
    ]

instance FromJSON SeasonType where
  parseJSON (Object o) = SeasonType
    <$> o .: "Id"
    <*> o .: "BusinessUnit"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid SeasonType JSON"

data SeasonTypeSummary
  = SeasonTypeSummary
  { seasontypesummaryId :: SeasonTypeId
  , seasontypesummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON SeasonTypeSummary where
  toJSON SeasonTypeSummary{..} = object
    [ "Id" .= seasontypesummaryId
    , "Description" .= seasontypesummaryDescription
    ]

instance FromJSON SeasonTypeSummary where
  parseJSON (Object o) = SeasonTypeSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid SeasonTypeSummary JSON"

type EntityId = Int

data Entity
  = Entity
  { entityId :: EntityId }
  deriving (Eq)

instance ToJSON Entity where
  toJSON Entity{..} = object
    [ "Id" .= entityId ]

instance FromJSON Entity where
  parseJSON (Object o) = Entity
    <$> o .: "Id"
  parseJSON _ = error "Invalid Entity JSON"

type ContactPermissionCategoryId = Int

data ContactPermissionCategory
  = ContactPermissionCategory
  { contactpermissioncategoryId :: ContactPermissionCategoryId
  , contactpermissioncategoryAskFrequencyMonths :: Int
  , contactpermissioncategoryControlGroup :: ControlGroup
  , contactpermissioncategoryCreateLocation :: String
  , contactpermissioncategoryCreatedBy :: String
  , contactpermissioncategoryCreatedDateTime :: UTCTime
  , contactpermissioncategoryDescription :: String
  , contactpermissioncategoryInactive :: Bool
  , contactpermissioncategoryUpdatedBy :: String
  , contactpermissioncategoryUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON ContactPermissionCategory where
  toJSON ContactPermissionCategory{..} = object
    [ "Id" .= contactpermissioncategoryId
    , "AskFrequencyMonths" .= contactpermissioncategoryAskFrequencyMonths
    , "ControlGroup" .= contactpermissioncategoryControlGroup
    , "CreateLocation" .= contactpermissioncategoryCreateLocation
    , "CreatedBy" .= contactpermissioncategoryCreatedBy
    , "CreatedDateTime" .= contactpermissioncategoryCreatedDateTime
    , "Description" .= contactpermissioncategoryDescription
    , "Inactive" .= contactpermissioncategoryInactive
    , "UpdatedBy" .= contactpermissioncategoryUpdatedBy
    , "UpdatedDateTime" .= contactpermissioncategoryUpdatedDateTime
    ]

instance FromJSON ContactPermissionCategory where
  parseJSON (Object o) = ContactPermissionCategory
    <$> o .: "Id"
    <*> o .: "AskFrequencyMonths"
    <*> o .: "ControlGroup"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid ContactPermissionCategory JSON"

data ContactPermissionCategorySummary
  = ContactPermissionCategorySummary
  { contactpermissioncategorysummaryId :: ContactPermissionCategoryId
  , contactpermissioncategorysummaryAskFrequencyMonths :: Int
  , contactpermissioncategorysummaryDescription :: String
  , contactpermissioncategorysummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON ContactPermissionCategorySummary where
  toJSON ContactPermissionCategorySummary{..} = object
    [ "Id" .= contactpermissioncategorysummaryId
    , "AskFrequencyMonths" .= contactpermissioncategorysummaryAskFrequencyMonths
    , "Description" .= contactpermissioncategorysummaryDescription
    , "Inactive" .= contactpermissioncategorysummaryInactive
    ]

instance FromJSON ContactPermissionCategorySummary where
  parseJSON (Object o) = ContactPermissionCategorySummary
    <$> o .: "Id"
    <*> o .: "AskFrequencyMonths"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid ContactPermissionCategorySummary JSON"

type TheaterId = Int

data Theater
  = Theater
  { theaterId :: TheaterId
  , theaterCity :: String
  , theaterCreateLocation :: String
  , theaterCreatedBy :: String
  , theaterCreatedDateTime :: UTCTime
  , theaterDataWindowDefinition :: String
  , theaterDescription :: String
  , theaterDrivingDirections :: String
  , theaterInactive :: Bool
  , theaterMaximumNumberOfSeats :: Int
  , theaterPhone :: String
  , theaterPostalCode :: String
  , theaterState :: String
  , theaterStreet :: String
  , theaterUpdatedBy :: String
  , theaterUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON Theater where
  toJSON Theater{..} = object
    [ "Id" .= theaterId
    , "City" .= theaterCity
    , "CreateLocation" .= theaterCreateLocation
    , "CreatedBy" .= theaterCreatedBy
    , "CreatedDateTime" .= theaterCreatedDateTime
    , "DataWindowDefinition" .= theaterDataWindowDefinition
    , "Description" .= theaterDescription
    , "DrivingDirections" .= theaterDrivingDirections
    , "Inactive" .= theaterInactive
    , "MaximumNumberOfSeats" .= theaterMaximumNumberOfSeats
    , "Phone" .= theaterPhone
    , "PostalCode" .= theaterPostalCode
    , "State" .= theaterState
    , "Street" .= theaterStreet
    , "UpdatedBy" .= theaterUpdatedBy
    , "UpdatedDateTime" .= theaterUpdatedDateTime
    ]

instance FromJSON Theater where
  parseJSON (Object o) = Theater
    <$> o .: "Id"
    <*> o .: "City"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "DataWindowDefinition"
    <*> o .: "Description"
    <*> o .: "DrivingDirections"
    <*> o .: "Inactive"
    <*> o .: "MaximumNumberOfSeats"
    <*> o .: "Phone"
    <*> o .: "PostalCode"
    <*> o .: "State"
    <*> o .: "Street"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid Theater JSON"

data TheaterSummary
  = TheaterSummary
  { theatersummaryId :: TheaterId
  , theatersummaryDescription :: String
  }
  deriving (Eq)

instance ToJSON TheaterSummary where
  toJSON TheaterSummary{..} = object
    [ "Id" .= theatersummaryId
    , "Description" .= theatersummaryDescription
    ]

instance FromJSON TheaterSummary where
  parseJSON (Object o) = TheaterSummary
    <$> o .: "Id"
    <*> o .: "Description"
  parseJSON _ = error "Invalid TheaterSummary JSON"

type ConstituentGroupId = Int

data ConstituentGroup
  = ConstituentGroup
  { constituentgroupId :: ConstituentGroupId
  , constituentgroupCreateLocation :: String
  , constituentgroupCreatedBy :: String
  , constituentgroupCreatedDateTime :: UTCTime
  , constituentgroupDescription :: String
  , constituentgroupInactive :: Bool
  , constituentgroupUpdatedBy :: String
  , constituentgroupUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON ConstituentGroup where
  toJSON ConstituentGroup{..} = object
    [ "Id" .= constituentgroupId
    , "CreateLocation" .= constituentgroupCreateLocation
    , "CreatedBy" .= constituentgroupCreatedBy
    , "CreatedDateTime" .= constituentgroupCreatedDateTime
    , "Description" .= constituentgroupDescription
    , "Inactive" .= constituentgroupInactive
    , "UpdatedBy" .= constituentgroupUpdatedBy
    , "UpdatedDateTime" .= constituentgroupUpdatedDateTime
    ]

instance FromJSON ConstituentGroup where
  parseJSON (Object o) = ConstituentGroup
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid ConstituentGroup JSON"

data ConstituentGroupSummary
  = ConstituentGroupSummary
  { constituentgroupsummaryId :: ConstituentGroupId
  , constituentgroupsummaryDescription :: String
  , constituentgroupsummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON ConstituentGroupSummary where
  toJSON ConstituentGroupSummary{..} = object
    [ "Id" .= constituentgroupsummaryId
    , "Description" .= constituentgroupsummaryDescription
    , "Inactive" .= constituentgroupsummaryInactive
    ]

instance FromJSON ConstituentGroupSummary where
  parseJSON (Object o) = ConstituentGroupSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid ConstituentGroupSummary JSON"

type BusinessUnitId = Int

data BusinessUnit
  = BusinessUnit
  { businessunitId :: BusinessUnitId
  , businessunitCreateLocation :: String
  , businessunitCreatedBy :: String
  , businessunitCreatedDateTime :: UTCTime
  , businessunitDescription :: String
  , businessunitInactive :: Bool
  , businessunitUpdatedBy :: String
  , businessunitUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON BusinessUnit where
  toJSON BusinessUnit{..} = object
    [ "Id" .= businessunitId
    , "CreateLocation" .= businessunitCreateLocation
    , "CreatedBy" .= businessunitCreatedBy
    , "CreatedDateTime" .= businessunitCreatedDateTime
    , "Description" .= businessunitDescription
    , "Inactive" .= businessunitInactive
    , "UpdatedBy" .= businessunitUpdatedBy
    , "UpdatedDateTime" .= businessunitUpdatedDateTime
    ]

instance FromJSON BusinessUnit where
  parseJSON (Object o) = BusinessUnit
    <$> o .: "Id"
    <*> o .: "CreateLocation"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid BusinessUnit JSON"

data BusinessUnitSummary
  = BusinessUnitSummary
  { businessunitsummaryId :: BusinessUnitId
  , businessunitsummaryDescription :: String
  , businessunitsummaryInactive :: Bool
  }
  deriving (Eq)

instance ToJSON BusinessUnitSummary where
  toJSON BusinessUnitSummary{..} = object
    [ "Id" .= businessunitsummaryId
    , "Description" .= businessunitsummaryDescription
    , "Inactive" .= businessunitsummaryInactive
    ]

instance FromJSON BusinessUnitSummary where
  parseJSON (Object o) = BusinessUnitSummary
    <$> o .: "Id"
    <*> o .: "Description"
    <*> o .: "Inactive"
  parseJSON _ = error "Invalid BusinessUnitSummary JSON"

type ControlGroupId = Int

data ControlGroup
  = ControlGroup
  { controlgroupId :: ControlGroupId
  , controlgroupCreatedBy :: String
  , controlgroupCreatedDateTime :: UTCTime
  , controlgroupDescription :: String
  , controlgroupInactive :: Bool
  , controlgroupPermission :: Int
  , controlgroupUpdatedBy :: String
  , controlgroupUpdatedDateTime :: UTCTime
  }
  deriving (Eq)

instance ToJSON ControlGroup where
  toJSON ControlGroup{..} = object
    [ "Id" .= controlgroupId
    , "CreatedBy" .= controlgroupCreatedBy
    , "CreatedDateTime" .= controlgroupCreatedDateTime
    , "Description" .= controlgroupDescription
    , "Inactive" .= controlgroupInactive
    , "Permission" .= controlgroupPermission
    , "UpdatedBy" .= controlgroupUpdatedBy
    , "UpdatedDateTime" .= controlgroupUpdatedDateTime
    ]

instance FromJSON ControlGroup where
  parseJSON (Object o) = ControlGroup
    <$> o .: "Id"
    <*> o .: "CreatedBy"
    <*> o .: "CreatedDateTime"
    <*> o .: "Description"
    <*> o .: "Inactive"
    <*> o .: "Permission"
    <*> o .: "UpdatedBy"
    <*> o .: "UpdatedDateTime"
  parseJSON _ = error "Invalid ControlGroup JSON"

data ProductionSeasonSearchRequest
data ProductionSeasonSearchResponse
