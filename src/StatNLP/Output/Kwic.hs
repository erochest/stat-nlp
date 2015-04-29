module StatNLP.Output.Kwic where


import StatNLP.Types


kwic :: PlainToken -> Index PlainToken LinePos -> Int -> Document -> [KwicNode]
kwic = undefined

posKwic :: PlainToken -> LinePos -> Int -> Document -> [KwicNode]
posKwic = undefined

concordance :: Index PlainToken LinePos -> Int -> Document -> [(PlainToken, [KwicNode])]
concordance = undefined
