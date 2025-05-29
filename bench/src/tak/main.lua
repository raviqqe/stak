function tak(x, y, z)
	if y < x then
		return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
	end

	return z
end

print(tak(16, 8, 0))
