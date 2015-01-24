-- No while until if then else for case swith class extends implements
-- l <= 80 L <= 100
-- No comments
module Main (
    main
) where
import Data.Functor
import Data.List
import System.Environment
import Text.Read

primes::Max->[Prime]
type Max   = Int
type Prime = Int
primes n =[x|x<-[1..n], x `notElem`[a*b|a<-[2..x],b<-[2..x],a<=b && a*b <= n]]

distances::[Int]->[(Int,D)]
type D = Int
distances = foldl (\acc x -> acc++[(x,d x acc)]) [(0,0)]
  where d::Int->[(Int,D)]->D
        d x acc = x-fst(last acc)

pdistances::Max->[D]
pdistances n = sort(tail(distances (primes n)) >>= \(_,y) -> [y])

groupByD::Max->[[D]]
groupByD n = group(pdistances n)

frequency::[D]->[(D,C)]
type C = Int
frequency x = [(head x,length x)]

histogram::(D,C)->[Bar]
type Bar = String
histogram (d,c) = [('#' <$ [1..c])++(show d)]

hPrimes::Max->[Bar]
hPrimes n = groupByD n >>= frequency >>= histogram

hprimes::Max->String
hprimes = lines.hPrimes
 where lines::[String]->String
       lines = foldl (\acc x -> acc++x++"\n\r") ""

main = getArgs >>= printOutput.apply(hprimes).readInput

printOutput               = putStrLn.prompt
readInput                 = readInt.readArgs

readArgs::Args->ErrorOrArg
type Args                 = [String]
type ErrorOrArg           = Either Error Arg
type Error                = String
type Arg                  = Either Help N
type Help                 = String
type N                    = String
readArgs ["-h"]           = Right (Left ("wprimes <number>"::Help))
readArgs ["-v"]           = Right (Left ("0.0.1"::Help))
readArgs [n]              = Right (Right (n::N))
readArgs []               = Left  ("No arguments"::Error)
readArgs _                = Left  ("Too many arguments"::Error)

readInt::ErrorOrArg -> MessageOrValue
type MessageOrValue       = Either Message Value
type Message              = (Either Error Help)
type Value                = Maybe Int
readInt (Right (Right n)) = Right (readMaybe (n::N))
readInt (Right (Left h))  = Left (Right (h::Help))
readInt (Left  e)         = Left (Left  (e::Error))

apply::F->MessageOrValue->Output
type F                    = Int -> String
type Output               = Either (Either Error Help) Result
type Result               = String
apply f (Right (Just n))  = Right (f (n::Int))
apply f (Right Nothing)   = Left (Left ("Please provide a number"::Error))
apply f (Left (Right h))  = Left (Right (h::Help))
apply f (Left (Left  e))  = Left (Left  (e::Error))

prompt::Output->String
prompt (Right r)          = (r::Result)
prompt (Left (Right h))   = (h::Help)
prompt (Left (Left  e))   = "Error: " ++ (e::Error)

