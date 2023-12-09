f = open("Day2/input", "r")
# config = [12, 13, 14] # red green blue
ids = []
rolling_sum = 0
for x in f:
    x = x.strip()
    x = x.replace(", ", ",").replace(": ", ":").replace("; ", ";")
    game_samples = x.split(":")
    samples = game_samples[1].split(";")
    impossible = False
    # maxRGB = [0, 0, 0] # rgb
    maxRGB = {"red": 0, "green" : 0, "blue" : 0}
    for sample in samples:
        rgb = sample.split(",")
        for s in rgb:
            number = s.split(" ")
            maxRGB[number[1]] = max(maxRGB[number[1]], int(number[0]))
    product = 1
    for key in maxRGB:
        product = product * maxRGB[key]
    rolling_sum += product
            # if number[1] == "red" and int(number[0]) > 12:
            #     impossible = True
            # elif number[1] == "green" and int(number[0]) > 13:
            #     impossible = True
            # elif number[1] == "blue" and int(number[0]) > 14:
            #     impossible = True

    # if impossible:
    #     print(game_samples[0].split(" ")[1])
    #     ids.append(int(game_samples[0].split(" ")[1]))
    
print(rolling_sum)