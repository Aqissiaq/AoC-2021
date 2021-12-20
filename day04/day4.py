
def rotate(m):
    return list(zip(*m[::-1]))


def makeTuple(c: str):
    return (int(c), False)


def parseBoard(boardStr: str):
    return [list(map(makeTuple, row.split())) for row in boardStr.split('\n')]


def sumBoard(board: [[(int, bool)]]):
    return sum(n for row in board for (n, marked) in row if not marked)


def winner(board: [[(int, bool)]]):
    sumsOfRows = [sum([1 for (_, marked) in r if marked]) for r in board]
    sumsOfCols = [sum([1 for (_, marked) in r if marked]) for r in rotate(board)]

    return (5 in sumsOfRows) or (5 in sumsOfCols)


if __name__ == "__main__":
    with open("input") as f:
        fullInput = f.read().split("\n\n")

        inputNums = map(int, fullInput.pop(0).split(','))
        boards = list(map(parseBoard, fullInput))

        winners = []
        for called in inputNums:
            for board in boards:
                if board in winners:
                    continue
                for row in board:
                    for i in range(len(row)):
                        if row[i] == (called, False):
                            row[i] = (called, True)
                if winner(board):
                    winners += [board]
                    if len(winners) == len(boards):
                        print(sumBoard(board) * called)
