{- HLINT ignore "Use camelCase" -}
module EE2_Join where

import Control.Arrow
import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

{-
What are all our customers' favorite flavors?

If they don't have one, give back a `Nothing`.
-}
a_favoriteFlavors :: DB [(Entity Customer, Maybe (Entity Flavor))]
a_favoriteFlavors = do
  custflavors <- select $ do
    (customer :& mflavor) <-
      from $ table @Customer
        `leftJoin` table @Flavor
         `on` \(customer :& mflavor) ->
          (customer ^. CustomerFavoriteFlavor) ==. (mflavor ?. FlavorId)
    pure (customer, mflavor)
  pure custflavors
{-
We'd like to determine the popularity of each flavor.

Return a list of each flavor name along with how many customers have it as
their favorite flavor. Sort it by popularity in descending order,
and then alphabetically by name.

Sample results:
[ ("Chunky Chocolate", 27)
, ("Smooth Strawberry", 12)
, ("Coconut Cream", 3)
, ("Variegated Vanilla", 3)
]
-}
b_flavorPopularity :: DB [(Text, Int)]
b_flavorPopularity = undefined

{-
We have a concept of "groups" provided by CustomerLink and CustomerGroupParent.

Who are all the customers in the largest group?
-}
c_largestGroup :: DB [Entity Customer]
c_largestGroup = undefined

{-
For each CustomerGroupParent, list its ID as well as the IDs of all the customers in that group.
-}
d_customerGroups :: DB [(CustomerGroupParentId, [CustomerId])]
d_customerGroups  = undefined
