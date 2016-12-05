module Test.Text.Allstar.Stacks where
import qualified Text.Allstar.GSS as GSS
import Data.Set (fromList)
import Text.Allstar.Stacks

stacks = fromList
  [ [0, 1, 3, 7]
  , [0, 1, 4, 7]
  , [0, 2, 5, 7]
  , [0, 2, 6, 8]
  ]

gss = GSS.fromStacks stacks

gssExp =
  ( fromList  [ (7,3), (3,1), (1,0)
              , (7,4), (4,1), (1,0)
              , (7,5), (5,2), (2,0)
              , (8,6), (6,2), (2,0)
              ]
  , fromList  [0])

gssPopExp =
  ( fromList  [ (7,3), (3,1), (1,0)
              , (7,4), (4,1), (1,0)
              , (7,5), (5,2), (2,0)
              , (8,6), (6,2), (2,0)
              ]
  , fromList  [1,2])

