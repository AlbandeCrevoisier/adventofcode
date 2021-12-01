### A Pluto.jl notebook ###
# v0.16.0

using Markdown
using InteractiveUtils

# ╔═╡ f02f643a-3407-11eb-1ace-a1544760ccab
md"
# Advent of Code

### Day 1

##### Part 1
"

# ╔═╡ 5ac23a64-3408-11eb-001c-b9a5801b4c43
input_1 = reshape(parse.(Int, readlines("1")), (200, 1))

# ╔═╡ 4aca5f8e-3409-11eb-36be-b1a2aa7c013b
square = input_1 .+ permutedims(input_1)

# ╔═╡ 10bf8dde-340c-11eb-2124-f1d4618c877d
square_indices = findall(square .== 2020)[1]

# ╔═╡ 69319426-340c-11eb-125d-2dd42145f030
input_1[square_indices[1]] * input_1[square_indices[2]]

# ╔═╡ 97f579ee-340c-11eb-1af0-65b91d2a4288
md"##### Part 2"

# ╔═╡ c2cda012-340c-11eb-3cc0-bb0fe3de2162
input_1_2 = reshape(input_1, (200, 1, 1))

# ╔═╡ f96216a6-340c-11eb-26a5-17d1e4e82148
cube = input_1 .+ permutedims(input_1_2, [2, 1, 3]) .+ permutedims(input_1_2, [3, 2, 1])

# ╔═╡ 6b192910-340d-11eb-302a-99dde6e8f5ba
cube_indices = findall(cube .== 2020)[1]

# ╔═╡ 94d9f3a6-340d-11eb-0c2f-db1aaa80f3c5
input_1_2[cube_indices[1]] * input_1_2[cube_indices[2]] * input_1_2[cube_indices[3]]

# ╔═╡ a78e63ee-340e-11eb-0a26-976fb6fcf5cc
md"These solutions are far from optimal: sums are done two or three too many times, respectively, & all sums are done instead of stopping when the solution is found. But it was fun goofing around multi-dimensional arrays."

# ╔═╡ 5b05a3cc-3486-11eb-14d2-1112f31469ce
md"
### Day 2

##### Part 1
"

# ╔═╡ 6a41a444-3486-11eb-122a-c350d9dfb724
input_2 = permutedims(hcat(split.(readlines("2"))...))

# ╔═╡ 357a4bac-348c-11eb-3cee-ad92f7f3d310
parsed_2 = hcat(parse.(Int, permutedims(hcat(split.(input_2[:, 1], '-')...))), map(s -> s[1], input_2[:, 2]), input_2[:, 3])

# ╔═╡ 67ecd520-348f-11eb-321f-1944fb7d38b3
counts = hcat(parsed_2[:, 1:2], map((a, b) -> count(c -> c == a, b), parsed_2[:, 3], parsed_2[:, 4]))

# ╔═╡ f64e84b6-349a-11eb-3af2-bd63735f8c5b
sum(mapslices(r -> r[1] <= r[3] && r[3] <= r[2], counts, dims = [2]))

# ╔═╡ 2b6ba4a0-349e-11eb-1b2c-c5e1a96df446
md"##### Part 2"

# ╔═╡ 510eb936-349e-11eb-2883-57d736229717
sum(map((i, j, c, s) -> (s[i] == c) ⊻ (s[j] == c), parsed_2[:, 1], parsed_2[:, 2], parsed_2[:, 3], parsed_2[:, 4]))

# ╔═╡ ffd0a9b4-3658-11eb-1c7d-853c2d4ed648
md"
### Day 3

##### Part 1
"

# ╔═╡ 0b61f256-3659-11eb-3e17-a3f48318e457
input_3 = readlines("3")

# ╔═╡ b2e49c80-37e0-11eb-30e4-df0b17ca44e8
let
	acc = 0
	for i in 1:size(input_3)[1]
		acc += '#' == input_3[i][1 + 3 * (i - 1) % length(input_3[1])]
	end
	acc
end

# ╔═╡ 8bba15e8-37e2-11eb-2741-2148f43dd0ad
md"##### Part 2"

# ╔═╡ 2be4f6b4-37e3-11eb-01f6-a9a253f5a921
let
	res = []
	for (inc_i, inc_j) in [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
		acc = 0
		for i in 1:ceil(Int, size(input_3)[1] / inc_i)
			acc += '#' == input_3[1 + inc_i * (i - 1)][1 + inc_j * (i - 1) % length(input_3[1])]
		end
		push!(res, acc)
	end
	*(res...)
end

# ╔═╡ 58cba33e-37e4-11eb-1fa4-1faeca75032d
md"
### Day 4

##### Part 1
"

# ╔═╡ 61ba7964-37e4-11eb-38e6-d1920b603cee
input_4 = readlines("4")

# ╔═╡ Cell order:
# ╟─f02f643a-3407-11eb-1ace-a1544760ccab
# ╟─5ac23a64-3408-11eb-001c-b9a5801b4c43
# ╟─4aca5f8e-3409-11eb-36be-b1a2aa7c013b
# ╟─10bf8dde-340c-11eb-2124-f1d4618c877d
# ╟─69319426-340c-11eb-125d-2dd42145f030
# ╟─97f579ee-340c-11eb-1af0-65b91d2a4288
# ╟─c2cda012-340c-11eb-3cc0-bb0fe3de2162
# ╟─f96216a6-340c-11eb-26a5-17d1e4e82148
# ╟─6b192910-340d-11eb-302a-99dde6e8f5ba
# ╟─94d9f3a6-340d-11eb-0c2f-db1aaa80f3c5
# ╟─a78e63ee-340e-11eb-0a26-976fb6fcf5cc
# ╟─5b05a3cc-3486-11eb-14d2-1112f31469ce
# ╟─6a41a444-3486-11eb-122a-c350d9dfb724
# ╟─357a4bac-348c-11eb-3cee-ad92f7f3d310
# ╟─67ecd520-348f-11eb-321f-1944fb7d38b3
# ╟─f64e84b6-349a-11eb-3af2-bd63735f8c5b
# ╟─2b6ba4a0-349e-11eb-1b2c-c5e1a96df446
# ╟─510eb936-349e-11eb-2883-57d736229717
# ╟─ffd0a9b4-3658-11eb-1c7d-853c2d4ed648
# ╟─0b61f256-3659-11eb-3e17-a3f48318e457
# ╟─b2e49c80-37e0-11eb-30e4-df0b17ca44e8
# ╟─8bba15e8-37e2-11eb-2741-2148f43dd0ad
# ╠═2be4f6b4-37e3-11eb-01f6-a9a253f5a921
# ╟─58cba33e-37e4-11eb-1fa4-1faeca75032d
# ╠═61ba7964-37e4-11eb-38e6-d1920b603cee
