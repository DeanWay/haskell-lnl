isBig x = x > 9000

boolToYesNo x = if x then "Yes" else "No"

myProgram = boolToYesNo . isBig

main = do
  line <- getLine
  print (myProgram (read line))
