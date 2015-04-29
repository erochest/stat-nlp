module StatNLP.Output.Kwic where


import StatNLP.Types


kwic :: PlainToken -> Index PlainToken SpanPos -> Int -> Document -> [KwicNode]
kwic = undefined

posKwic :: PlainToken -> SpanPos -> Int -> Document -> [KwicNode]
posKwic = undefined

concordance :: Index PlainToken SpanPos -> Int -> Document -> [(PlainToken, [KwicNode])]
concordance = undefined
