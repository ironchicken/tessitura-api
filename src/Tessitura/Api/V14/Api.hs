{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tessitura.Api.V14.Api where

import Servant.API
import Tessitura.Api.V14.Models

type PostWebSession =
  "Web" :> "Session"
    :> ReqBody '[JSON] SessionRequest
    :> Post '[JSON] SessionResponse

type GetWebSession =
  "Web" :> "Session"
    :> Capture "sessionKey" SessionKey
    :> Get '[JSON] (Maybe Session)

type Login =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] Session

type ConstituentOnAccount =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Constituent" :> "OnAccount"
    :> QueryParam "paymentMethodIds" String
    :> Get '[JSON] [OnAccountBalance]

type LoginEmail =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Login" :> "Email"
    :> ReqBody '[JSON] LoginEmailRequest
    :> Post '[JSON] Session

type LoadOrder =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "LoadOrder" :> Capture "orderId" String
    :> Post '[JSON] Cart

type Reprint =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Reprint"
    :> ReqBody '[JSON] ReprintRequest
    :> Post '[JSON] NoBody

type OrdersSearch =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Orders" :> "Search"
    :> QueryParam "seasonId" String
    :> QueryParam "renewalsOnly" String
    :> QueryParam "modeOfsaleId" String
    :> QueryParam "perfStartDate" String
    :> QueryParam "perfEndDate" String
    :> QueryParam "deliveryMethodId" String
    :> QueryParam "unprintedOnly" String
    :> Get '[JSON] WebOrderSearchResponse

type SessionVariables =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Variables"
    :> Get '[JSON] [SessionVariable]

type PromoCode =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "PromoCode"
    :> ReqBody '[JSON] WebPromoCodeRequest
    :> Post '[JSON] [WebPromoCode]

type SessionDefault =
  "Web" :> "Session" :> Capture "sessionKey" SessionKey :> "Default" :> Capture "fieldName" String
    :> Get '[JSON] SystemDefaultSummary

type WebServiceApi =
  PostWebSession
  :<|> GetWebSession
  :<|> Login
  :<|> ConstituentOnAccount
  :<|> LoginEmail
  :<|> LoadOrder
  :<|> Reprint
  :<|> OrdersSearch
  :<|> SessionVariables
  :<|> PromoCode
  :<|> SessionDefault

type TessituraApi =
  WebServiceApi
