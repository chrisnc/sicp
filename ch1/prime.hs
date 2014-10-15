-- SICP Section 1.2.6 Example, Testing for Primality

square x = x * x

smallestDivisor n = findDivisor n 2

findDivisor n testDivisor
  | square testDivisor > n = n
  | divides testDivisor n  = testDivisor
  | otherwise              = findDivisor n (testDivisor + 1)

divides a b = rem b a == 0

prime n = n == smallestDivisor n
