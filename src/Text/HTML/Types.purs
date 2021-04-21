-- | Miscellaneous newtype wrappers and types.
module Text.HTML.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)


-- | Newtype wrapper for names.
newtype Name = Name String

derive newtype instance eqName :: Eq Name
derive instance genericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow


-- | Newtype wrapper for values.
newtype Value = Value String

derive newtype instance eqValue :: Eq Value
derive instance genericValue :: Generic Value _
instance showValue :: Show Value where
  show = genericShow


-- | Product of a `Name` and `Value`.
data Attribute = Attribute Name Value

derive instance eqAttribute :: Eq Attribute
derive instance genericAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where
  show = genericShow

-- | Type alias for convenience.
type Attributes = List Attribute


-- | Represents HTML tags in the document.
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

-- | Type alias for convenience.
type Tags = List Tag
