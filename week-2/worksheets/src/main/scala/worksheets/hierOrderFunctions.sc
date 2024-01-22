def product(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 1 else f(a) * product(f)(a + 1, b)

// Let's do above function more general
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, defaultVal: Int)(a: Int, b: Int): Int =
  if a > b then defaultVal else combine(f(a), mapReduce(f, combine, defaultVal)(a + 1, b))

def factorial(n: Int) = mapReduce(f = x => x, combine = (x, y) => x * y, defaultVal = 1)(1, n)
factorial(5)

def sum(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)
def sumOfFactorial = sum(factorial)
sumOfFactorial(4, 5)


// Finding fixing point of a function