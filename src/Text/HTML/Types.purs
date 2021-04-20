module Text.HTML.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)


newtype Name = Name String

derive newtype instance eqName :: Eq Name
derive instance genericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow


newtype Value = Value String

derive newtype instance eqValue :: Eq Value
derive instance genericValue :: Generic Value _
instance showValue :: Show Value where
  show = genericShow


data Attribute = Attribute Name Value

derive instance eqAttribute :: Eq Attribute
derive instance genericAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where
  show = genericShow

type Attributes = List Attribute


data Tag
  = TagOpen Name Attributes
  | TagClose Name
  | TagSingle Name Attributes
  | TagText String
  | TagComment String
  | TagDoctype

derive instance eqTag :: Eq Tag
derive instance genericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow

type Tags = List Tag
