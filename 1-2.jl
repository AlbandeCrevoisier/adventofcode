input = parse.(Int, readlines("1"))
for i in 1:length(input)-2
    for j in i:length(input)-1
        for k in j:length(input)
            if input[i] + input[j] + input[k] == 2020
                println(input[i] * input[j] * input[k])
                return
            end
        end
    end
end
