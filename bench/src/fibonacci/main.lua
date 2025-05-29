function fibonacci(x)
	if x < 2 then
		return x
	else
		return fibonacci(x - 1) + fibonacci(x - 2)
	end
end

print(fibonacci(32))
