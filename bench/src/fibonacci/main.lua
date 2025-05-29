function fibonacci(x)
	if x < 2 then
		return x
	end

	return fibonacci(x - 1) + fibonacci(x - 2)
end

print(fibonacci(32))
