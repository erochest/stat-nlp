module StatNLP.Output.Kwic where


import StatNLP.Types


kwic :: PlainToken -> Index PlainToken p -> Int -> Document -> ()
kwic = undefined

posKwic :: PlainToken -> p -> Int -> Document -> ()
posKwic = undefined

concordance :: Index PlainToken p -> Int -> Document -> [(PlainToken, ())]
concordance = undefined
