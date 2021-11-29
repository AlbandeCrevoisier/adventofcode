input = permutedims(hcat(split.(readlines("2"))...))
parsed = hcat(parse.(Int, permutedims(hcat(split.(input[:, 1], '-')...))), map(s -> s[1], input[:, 2]), input[:, 3])
counts = hcat(parsed[:, 1:2], map((a, b) -> count(c -> c == a, b), parsed[:, 3], parsed[:, 4]))
println(sum(mapslices(r -> r[1] <= r[3] && r[3] <= r[2], counts, dims = [2])))
