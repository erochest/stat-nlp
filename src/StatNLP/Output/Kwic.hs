module StatNLP.Output.Kwic where


import StatNLP.Types


kwic :: PlainToken -> Index PlainToken p -> Int -> Document p -> ()
kwic = undefined

posKwic :: PlainToken -> p -> Int -> Document p -> ()
posKwic = undefined

concordance :: Index PlainToken p -> Int -> Document p -> [(PlainToken, ())]
concordance = undefined
