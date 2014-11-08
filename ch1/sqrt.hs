-- SICP Section 1.1.7 example, Square Roots by Newton's Method, page 28

mySqrt x = rootIter 1 where
  rootIter guess =
    if goodEnough guess
      then guess
      else rootIter $ improve guess
  goodEnough guess = abs (guess * guess - x) < 0.001
  improve guess = average guess $ x / guess
  average x y = (x + y) / 2

myCubeRoot x = rootIter 1 where
  rootIter guess =
    if goodEnough guess
      then guess
      else rootIter $ improve guess
  goodEnough guess = abs (guess * guess * guess - x) < 0.001
  improve guess = (x / (guess * guess) + 2 * guess) / 3
  average x y = (x + y) / 2
