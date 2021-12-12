from itertools import chain

def parsePoint(pointStr: str):
    return tuple([int(n) for n in pointStr.split(",")])


def parseLine(line: str):
    return tuple(parsePoint(p) for p in line.split("->"))


def countPoints(g: [[int]]):
    return sum(1 for p in chain(*g) if p > 1)

def prettyPrint(x : [[int]]):
    for row in x:
        print(row)

if __name__ == "__main__":
    with open("input") as f:
        fullInput = f.readlines()

    lines = [parseLine(l) for l in fullInput]

    # this will need to be bigger
    grid = [[0 for _ in range(1000)] for _ in range(1000)]

    for line in lines:
        fst, snd = line
        x0, y0 = fst
        x1, y1 = snd
        if x0 == x1:
            y0, y1 = sorted([y0, y1])
            for y in range(y0, y1+1):
                grid[x0][y] += 1
        elif y0 == y1:
            x0, x1 = sorted([x0, x1])
            for x in range(x0, x1+1):
                grid[x][y0] += 1
        else:
            p1, p2 = sorted([fst, snd])
            x0, y0 = p1
            x1, y1 = p2

            y = y0
            if y <= y1:
                yDir = 1
            else:
                yDir = -1


            for x in range(x0, x1+1):
                grid[x][y] += 1
                y += yDir

        # print(fst, snd)
        # prettyPrint(grid)
        # print()
    print(countPoints(grid))
