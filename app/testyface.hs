main :: IO()
main = do 
    print "who dis for"
    recipient <- getLine
    print "What dis called"
    title <- getLine
    print "who did dis"
    author <- getLine 
    print (createEmail recipient title author)


toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart title = "Title: " ++ title ++ ",\n"
fromPart author = "Thanks,\n"++author

createEmail recipient title author = toPart recipient ++
                                     bodyPart title ++ 
                                     fromPart author
    