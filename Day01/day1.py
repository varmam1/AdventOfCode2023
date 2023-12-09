f = open("Day01/input", "r")
numbers = []
for x in f:
  s = x.strip()
  s = s.replace("one", "o1ne") \
            .replace("two", "t2wo") \
            .replace("three", "t3hree") \
            .replace("four", "f4our") \
            .replace("five", "f5ive") \
            .replace("six", "s6ix") \
            .replace("seven", "s7even") \
            .replace("eight", "eigh8t") \
            .replace("nine", "nin9e") 
  nums = []
  for c in s:
    if c.isnumeric():
      nums.append(c)
  if len(nums) == 1:
    numbers.append(int(nums[0] + nums[0]))
  else:
    numbers.append(int(nums[0] + nums[-1]))

print(sum(numbers))
