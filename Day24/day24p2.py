from z3 import Solver, sat, Reals, Real

f = open("Day24/input", "r")
parsedLines = []

for line in f:
    newLine = line.replace(" ", "")
    posVelo = newLine.split("@")
    positions = list(map(int, posVelo[0].split(",")))
    velocities = list(map(int, posVelo[1].split(",")))
    parsedLines.append((tuple(positions), tuple(velocities)))

solver = Solver()
x, y, z, vx, vy, vz = Reals('x y z vx vy vz')
for i, ((a, b, c), (va, vb, vc)) in enumerate(parsedLines):
    t = Real(f"t{i}")
    solver.add(t > 0)
    solver.add(x + vx * t == a + va * t)
    solver.add(y + vy * t == b + vb * t)
    solver.add(z + vz * t == c + vc * t)
if solver.check() == sat:
    m = solver.model()
    print (m.eval(x).as_long() + m.eval(y).as_long() + m.eval(z).as_long())