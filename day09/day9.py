from functools import reduce


def parseLine(l):
    return [int(c) for c in l.strip()]


def pPrint(arr):
    for row in arr:
        print(row)


def getNeighbors(arr, x, y):
    return [arr[x+i][y+j] for i in range(-1, 2) for j in range(-1, 2)
            if (x+i) >= 0 and (y+j) >= 0 and (x+i) < len(arr)
            and (y+j) < len(arr[0]) and i+j != 0]


def minima(arr):
    return [arr[x][y]
            for x in range(len(arr))
            for y in range(len(arr[0]))
            if arr[x][y] < min(getNeighbors(arr, x, y))]


def minimaCoords(arr):
    return [(x, y)
            for x in range(len(arr))
            for y in range(len(arr[0]))
            if arr[x][y] < min(getNeighbors(arr, x, y))]


# bottom is the minima, returns size of basin
def basinSize(arr, bottom):
    basin = []
    visited = []

    def flood(x, y, basin=basin, visited=visited):
        if (x, y) in visited:
            return
        if arr[x][y] == 9:
            return

        basin += [arr[x][y]]
        visited += [(x, y)]

        if x > 0:
            flood(x-1, y)
        if x < len(arr)-1:
            flood(x+1, y)
        if y > 0:
            flood(x, y-1)
        if y < len(arr[0])-1:
            flood(x, y+1)

    flood(*bottom)
    return len(basin)


if __name__ == "__main__":
    with open("input") as f:
        data = list(map(parseLine, f.readlines()))

    #part1
    print(sum(map(lambda x: x+1, minima(data))))

    #part2
    basins = list(map(lambda b: basinSize(data, b), minimaCoords(data)))
    topThree = sorted(basins, reverse=True)[:3]
    print(reduce(lambda x, y: x*y, topThree))
