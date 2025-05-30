def fibonacci(x)
  x < 2 ? x : fibonacci(x - 1) + fibonacci(x - 2)
end

p(fibonacci(32))
