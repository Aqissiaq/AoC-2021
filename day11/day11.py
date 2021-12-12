from itertools import product

def parseLine(l):
    return [int(c) for c in l.strip()]


def pPrint(arr):
    for row in arr:
        print(*row, sep="")
    print()


def getNeighbors(arr, x, y):
    return [(x+i, y+j) for i in range(-1, 2) for j in range(-1, 2)
            if (x+i) >= 0 and (y+j) >= 0 and (x+i) < len(arr)
            and (y+j) < len(arr[0])]


def oneStep(arr):

    shape = list(product(range(len(arr)), range(len(arr[0]))))

    for x, y in shape:
        arr[x][y] += 1

    flashes = 0
    flashed = True
    while flashed:
        flashed = False
        for x, y in shape:
            if arr[x][y] > 9:
                flashed = True
                arr[x][y] = 0
                flashes += 1
                for nx, ny in getNeighbors(arr, x, y):
                    if arr[nx][ny] != 0:
                        arr[nx][ny] += 1
    return flashes



if __name__ == "__main__":
    with open("input") as f:
        data = list(map(parseLine, f.readlines()))

    #part1
    # print(sum(oneStep(data) for _ in range(100)))

    #part2
    print([step for step in range(1000) if oneStep(data) == 100])
    # this is incredibly ugly, lol
