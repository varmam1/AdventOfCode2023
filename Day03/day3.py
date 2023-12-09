import more_itertools as mit

lines = list(open("input"))

def part_1():
    # Pad the array with . for ease of index search
    padded_array = ['.' * (len(lines[0].strip()) + 2)]
    for line in lines:
        line = line.strip()
        padded_array.append("." + line + ".")
    padded_array.append('.' * (len(lines[0].strip()) + 2))

    # Get all the indices which have numbers
    numeric_indices = [[j for j in range(0, len(padded_array[0])) if padded_array[i][j].isnumeric()] for i in range(0, len(padded_array))]
    # This should have it so row is the index -> groups of consecutive indices
    grouped_numeric_indices = [[list(group) for group in mit.consecutive_groups(numeric_indices[i])] for i in range(0, len(padded_array))]
    symboled_numbers = []

    # Search each of the surroundings of the numbers for a symbol; if there add to list
    for row in range(1, len(padded_array) - 1):
        for number_indices in grouped_numeric_indices[row]:
            length_of_number = len(number_indices)
            start_of_number = number_indices[0]
            for i in range(row - 1, row + 2):
                for j in range(start_of_number - 1, start_of_number + length_of_number + 1):
                    symbol = padded_array[i][j]
                    if symbol != "." and not symbol.isnumeric():
                        symboled_numbers.append(int(padded_array[row][start_of_number: start_of_number + length_of_number]))

    print(sum(symboled_numbers))


def part_2():
    # Pad the array with . for ease of index search
    padded_array = ['.' * (len(lines[0].strip()) + 2)]
    for line in lines:
        line = line.strip()
        padded_array.append("." + line + ".")
    padded_array.append('.' * (len(lines[0].strip()) + 2))

    # Get all the indices which have numbers
    numeric_indices = [[j for j in range(0, len(padded_array[0])) if padded_array[i][j].isnumeric()] for i in range(0, len(padded_array))]
    # This should have it so row is the index -> groups of consecutive indices
    grouped_numeric_indices = [[list(group) for group in mit.consecutive_groups(numeric_indices[i])] for i in range(0, len(padded_array))]
    
    # Get the indices of the stars
    star_indices = [[j for j in range(0, len(padded_array[0])) if padded_array[i][j] == '*'] for i in range(0, len(padded_array))]
    gear_ratios = []
    for row in range(1, len(padded_array) - 1):
        stars = star_indices[row]
        for col in stars:
            # If the two numbers are in the same row, there would only be two entries with the same row number
            # Otherwise, they will have differing row numbers
            found_number_indices = []
            for i in range(row - 1, row + 2):
                for j in range(col - 1, col + 2):
                    if padded_array[i][j].isnumeric():
                        found_number_indices.append((i, j))
            if len(found_number_indices) == 1:
                continue
            if len(found_number_indices) == 2 and found_number_indices[0][0] == found_number_indices[1][0] and found_number_indices[0][1] + 1 == found_number_indices[1][1]:
                continue
            if len(found_number_indices) == 3 and found_number_indices[0][0] == found_number_indices[1][0] and found_number_indices[0][0] == found_number_indices[2][0]:
                continue

            # By this point, if we haven't continued, we have a gear
            numbers = []
            # Find the corresponding number to each index (will have duplicates)
            for indices in found_number_indices:
                for group in grouped_numeric_indices[indices[0]]:
                    if indices[1] in group:
                        numbers.append(int(padded_array[indices[0]][group[0] : group[0] + len(group)]))

            # Remove duplicates so we only have 2 numbers
            numbers = list(set(numbers))
            gear_ratios.append(numbers[0] * numbers[1])

    print(sum(gear_ratios))

part_2()