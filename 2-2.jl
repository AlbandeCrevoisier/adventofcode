input = permutedims(hcat(split.(readlines("2"))...))
parsed = hcat(parse.(Int, permutedims(hcat(split.(input[:, 1], '-')...))), map(s -> s[1], input[:, 2]), input[:, 3])
println(sum(map((i, j, c, s) -> (s[i] == c) ⊻ (s[j] == c), parsed[:, 1], parsed[:, 2], parsed[:, 3], parsed[:, 4])))
