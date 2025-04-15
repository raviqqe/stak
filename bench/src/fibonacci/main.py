def fibonacci(x):
    return x if x < 2 else fibonacci(x - 1) + fibonacci(x - 2)


print(fibonacci(32))
