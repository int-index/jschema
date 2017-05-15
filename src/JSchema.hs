{-# LANGUAGE TypeInType, GADTs, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}

module JSchema where

import Control.Applicative
import Control.Monad
import Data.Foldable as F
import Data.Kind
import Data.Proxy
import Data.Scientific
import Data.Singletons
import Data.Singletons.Prelude.List hiding (Union)
import Data.Singletons.Prelude.Tuple
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
import Data.Union
import Data.Vector
import Data.Vinyl
import GHC.Exts (Constraint)
import GHC.TypeLits

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM

data Schema =
  SchNull |
  SchNumber |
  SchString |
  SchArray Schema |
  SchObject [(Symbol, Schema)] |
  SchOneOf [Schema]

data instance Sing (s :: Schema) where
  SSchNull :: Sing SchNull
  SSchNumber :: Sing SchNumber
  SSchString :: Sing SchString
  SSchArray :: Sing s -> Sing (SchArray s)
  SSchObject :: Sing rs -> Sing (SchObject rs)
  SSchOneOf :: Sing us -> Sing (SchOneOf us)

data JsonG :: Schema -> Type where
  JsonNull :: JsonG SchNull
  JsonNumber :: Scientific -> JsonG SchNumber
  JsonString :: T.Text -> JsonG SchString
  JsonArray :: Vector (JsonG a) -> JsonG (SchArray a)
  JsonObject :: Rec JsonF rs -> JsonG (SchObject rs)
  JsonOneOf :: Union JsonG us -> JsonG (SchOneOf us)

data JsonF :: (Symbol, Schema) -> Type where
  JsonField :: JsonG (sch :: Schema) -> JsonF '(name, sch)

jsonValidate :: SingI (sch :: Schema) => J.Value -> Maybe (JsonG sch)
jsonValidate = jsonValidate' sing

jsonValidate'
  :: Sing (sch :: Schema)
  -> J.Value
  -> Maybe (JsonG sch)
jsonValidate' sch v =
  case sch of
    SSchNull -> case v of
      J.Null -> Just JsonNull
      _      -> Nothing
    SSchNumber -> case v of
      J.Number s -> Just (JsonNumber s)
      _          -> Nothing
    SSchString -> case v of
      J.String s -> Just (JsonString s)
      _          -> Nothing
    SSchArray schElem -> case v of
      J.Array xs -> JsonArray <$> traverse (jsonValidate' schElem) xs
      _          -> Nothing
    SSchObject schFields -> case v of
      J.Object hm -> JsonObject <$> jsonObjectValidate schFields hm
      _           -> Nothing
    SSchOneOf schElems ->
      JsonOneOf <$> jsonUnionValidate schElems v

jsonObjectValidate
  :: Sing (schFields :: [(Symbol, Schema)])
  -> J.Object
  -> Maybe (Rec JsonF schFields)
jsonObjectValidate schFields hm =
  case schFields of
    SNil -> do
      guard $ HM.null hm
      Just RNil
    SCons (STuple2 (sName :: Sing name) sch) schFields' ->
      withKnownSymbol sName $ do
        let name = T.pack (symbolVal (Proxy :: Proxy name))
        v <- HM.lookup name hm
        field <- JsonField <$> jsonValidate' sch v
        fields <- jsonObjectValidate schFields' hm
        return $ field :& fields

jsonUnionValidate
  :: Sing (schElems :: [Schema])
  -> J.Value
  -> Maybe (Union JsonG schElems)
jsonUnionValidate schElems v =
  case schElems of
    SNil -> Nothing
    SCons sch schElems' ->
      This <$> jsonValidate' sch v <|>
      That <$> jsonUnionValidate schElems' v
