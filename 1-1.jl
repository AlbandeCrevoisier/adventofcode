input = parse.(Int, readlines("1"))
for i in 1:length(input)-1
    for j in i:length(input)
        if input[i] + input[j] == 2020
            println(input[i] * input[j])
            return
        end
    end
end
