-- SICP Exercise 1.15, sine of an angle

cube x = x * x * x

p x = 3 * x - 4 * (cube x)

sine angle =
  if not $ abs angle > 0.1
    then angle
    else p $ sine $ angle / 3.0
