module Text.Allstar.GSS where
-- Graph Structured Stack

-- Graph Structured Stack
--type Gamma = GSS ATNState

data GSS a = GSSEmpty
           | GSSWildcard
           | GSSStack [a]
           | GSSMerge (GSS a) (GSS a)

(\+/) = GSSMerge
(#) = GSSWildcard

-- configuration
--type Configuration = (ATNState, Int, Gamma)

