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

type GetAllTitles =
  "TXN" :> "Titles"
    :> Get '[JSON] [Title]

type GetAllTitlesSummaries =
  "TXN" :> "Titles" :> "Summary"
    :> Get '[JSON] [TitleSummary]

type GetTitle =
  "TXN" :> "Title" :> Capture "id" TitleId
    :> Get '[JSON] (Maybe Title)

type GetAllProductions =
  "TXN" :> "Productions"
    :> QueryParam "titleIds" [TitleId]
    :> Get '[JSON] [Production]

type GetAllProductionsSummaries =
  "TXN" :> "Productions" :> "Summaries"
    :> QueryParam "titleIds" [TitleId]
    :> Get '[JSON] [ProductionSummary]

type GetProduction =
  "TXN" :> "Productions" :> Capture "id" ProductionId
    :> Get '[JSON] (Maybe Production)

type GetAllProductionSeasons =
  "TXN" :> "ProductionSeasons"
    :> QueryParam "seasonIds" [SeasonId]
    :> QueryParam "productionIds" [ProductionId]
    :> QueryParam "ids" [ProductionSeasonId]
    :> Get '[JSON] [ProductionSeason]

type GetProductionSeasonsSummaries =
  "TXN" :> "ProductionSeaons" :> "Summary"
    :> QueryParam "seasonIds" [SeasonId]
    :> QueryParam "productionIds" [ProductionId]
    :> QueryParam "ids" [ProductionSeasonId]
    :> Get '[JSON] [ProductionSeasonSummary]

type GetProductionSeason =
  "TXN" :> "ProductionSeasons" :> Capture "id" ProductionSeasonId
    :> Get '[JSON] (Maybe ProductionSeason)

type PostProductionSeasonsSearch =
  "TXN" :> "ProductionSeaons" :> "Search"
    :> ReqBody '[JSON] ProductionSeasonSearchRequest
    :> Post '[JSON] [ProductionSeasonSearchResponse]

type WebSessionApi =
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

type TXNApi =
  GetAllTitles
  :<|> GetAllTitlesSummaries
  :<|> GetTitle
  :<|> GetAllProductions
  :<|> GetAllProductionsSummaries
  :<|> GetProduction
  :<|> GetAllProductionSeasons
  :<|> GetProductionSeasonsSummaries
  :<|> GetProductionSeason
  :<|> PostProductionSeasonsSearch

type TessituraApi =
  WebSessionApi
  :<|> TXNApi
