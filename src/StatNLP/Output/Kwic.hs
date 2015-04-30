module StatNLP.Output.Kwic where


import StatNLP.Types


kwic :: PlainToken -> Index PlainToken SpanPos -> Int -> Document -> ()
kwic = undefined

posKwic :: PlainToken -> SpanPos -> Int -> Document -> ()
posKwic = undefined

concordance :: Index PlainToken SpanPos -> Int -> Document -> [(PlainToken, ())]
concordance = undefined
