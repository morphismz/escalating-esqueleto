{- HLINT ignore "Use camelCase" -}
module EE2_Join where

import Control.Arrow
import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Database.Esqueleto.PostgreSQL
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
b_flavorPopularity = do
  t <- select $ do
    (flavor :& customer) <- from $ table @Flavor
      `innerJoin` table @Customer
      `on` \(flavor :& customer) ->
      (just $ flavor ^. FlavorId) ==. (customer ^. CustomerFavoriteFlavor)
    groupBy (flavor ^. FlavorName)
    orderBy [desc (countDistinct @Int (customer ^. CustomerId)), asc (flavor ^. FlavorName)]
    pure (flavor ^. FlavorName, countDistinct (customer ^. CustomerId))
  pure $ fmap (unValue *** unValue) t

{-
We have a concept of "groups" provided by CustomerLink and CustomerGroupParent.

Who are all the customers in the largest group?
-}
c_largestGroup :: DB [Entity Customer]
c_largestGroup = do
  select $ do
    ((clinkParentId, cnt) :& customerLink :& customer) <- from $
      (do
        custLink <- from $ table @CustomerLink
        groupBy (custLink ^. CustomerLinkParentId)
        orderBy [desc $ countDistinct @Int (custLink ^. CustomerLinkCustomerId)]
        limit 1                                                                         
        pure $ (custLink ^. CustomerLinkParentId, countDistinct @Int (custLink ^. CustomerLinkCustomerId)))
      `innerJoin` table @CustomerLink
      `on` (\((clinkparentId, _) :& customerLink) ->
        clinkparentId ==. (customerLink ^. CustomerLinkParentId))
      `innerJoin` table @Customer                                                                
      `on` (\((_ :& customerLink) :& customer) ->
        (customerLink ^. CustomerLinkCustomerId) ==. (customer ^. CustomerId))
    pure $ customer
{-  select $ do
    custlink <- from $ table @CustomerLink
    undefined-}
  
{-
For each CustomerGroupParent with at least one customer in it, list its name,
as well as the names of all the customers in that group.
-}
d_customerGroups :: DB [(Text, [Text])]
d_customerGroups = do
  fmap (fmap (unValue *** unValue)) $ select $ do
    (customerGroupParent :& customerLink :& customer) <- from $ table @CustomerGroupParent
      `innerJoin` table @CustomerLink
      `on` (\(customerGroupParent :& customerLink) ->
        (customerGroupParent ^. CustomerGroupParentId) ==. (customerLink ^. CustomerLinkParentId))
      `innerJoin` table @Customer
      `on` (\((_ :& customerLink) :& customer) ->
        (customerLink.customerId) ==. (customer.id))
    groupBy (customerGroupParent.id, customerGroupParent.name)
    pure (customerGroupParent.name , maybeArray (arrayAgg (customer.name)))

