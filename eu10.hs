import Primes (primes)

main = do
    let primes' = takeWhile (<2000000) primes
    let result = sum primes'
    print result

