x = concatMap show [1..]

y = [x!!0, x!!9, x!!99, x!!999, x!!9999, x!!99999, x!!999999]

result = product $ map (\z->read [z]) y :: Int

main :: IO ()
main = print result
