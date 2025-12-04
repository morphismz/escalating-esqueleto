{- HLINT ignore "Use camelCase" -}
module EE2_Join where

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
a_favoriteFlavors = _

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
b_flavorPopularity = _

{-
We have a concept of "groups" provided by CustomerLink and CustomerGroupParent.

Who are all the customers in the largest group?
-}
c_largestGroup :: DB [Entity Customer]
c_largestGroup = _

{-
For each CustomerGroupParent with at least one customer in it, list its name,
as well as the names of all the customers in that group.
-}
d_customerGroups :: DB [(Text, [Text])]
d_customerGroups  = _
