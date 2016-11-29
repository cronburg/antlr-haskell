module Test.Text.Allstar.GSS where
import Text.Allstar.GSS
import Data.Set (fromList)

gss = fromList
  [ [0, 1, 3, 7]
  , [0, 1, 4, 7]
  , [0, 2, 5, 7]
  , [0, 2, 6, 8]
  ]

-- Should it pop all common
gssPopExp = fromList
  [ (0, fromList [[1,3,7],[1,4,7],[2,5,7]])
  ]

