import sys
import unittest
from heapq import *

def to_1d(n, i, j):
    return i * (n-1) + j

def to_2d(n, k):
    row = k / n
    col = k % n
    return row, col

class Board(object):
    """ Immutable representation of puzzle board
        with dimension n and manhattan heurisitc """

    def __init__(self, tiles):
        self.board = tuple([tuple(row) for row in tiles])
        self.n = len(tiles[0])
        self.manhattan = 0
        for i, row in enumerate(tiles):
            for j, tile in enumerate(row):
                goalrow = (tile - 1) / self.n
                goalcol = (tile - 1) % self.n
                if tile != 0:
                    self.manhattan += abs(i - goalrow) + abs(j - goalcol)

    def __eq__(self, other):
        if self.n != other.n: return False
        same = True
        for i in range(self.n):
            same = same and self.board[i] == other.board[i]
        return same

    def __ne__(self, other):
        return not self.__eq__(other)

    def __repr__(self):
        s = "\n"
        for row in self.board:
            s += str(row) + '\n'
        s += '\nManhattan: ' + str(self.manhattan) + '\n'
        return s

    def neighbors(self):
        n = self.n
        flat = [self.board[i][j] for i in range(n) for j in range(n)]
        idx = flat.index(0)
        z_row, z_col = to_2d(n, idx)
        boards = []
        north = (-1, 1, 0, 0)
        east = (0, 0, 1, -1)
        for i in range(4):
            r = z_row + north[i]
            c = z_col + east[i]
            tiles = []
            if not any([r < 0, r >= n, c <0, c >= n]):
                for m in range(n):
                    tiles_row = []
                    for k in range(n):
                        tiles_row.append(self.board[m][k])
                    tiles.append(tiles_row)
                tiles[z_row][z_col] = self.board[r][c]
                tiles[r][c] = 0
                nb = Board(tiles)
                if nb:
                    boards.append(nb)
        return boards

class Node(object):

    def __init__(self, board, m, previous):
        self.board = board
        self.m = m
        self.previous = previous
        self.priority = self.board.manhattan + m  

    def __lt__(self, other):
        return self.priority < other.priority 

def solve(initial_board):
    initial_node = Node(initial_board, 0, None)
    frontier = []
    heappush(frontier, initial_node)
    while True:
        node = heappop(frontier)
        if node.board.manhattan == 0:
            return node
        for b in node.board.neighbors():
            if node.previous == None or b != node.previous.board:
                heappush(frontier, Node(b, node.m+1, node))
    return node

def read_board(f):
    file = open(f)
    tiles = []
    for i, line in enumerate(file):
        if i == 0: continue
        row = [int(x) for x in line.split()]
        tiles.append(row)
    return tiles

def main():
    f = sys.argv[1]
    tiles = read_board(f)
    initial_board = Board(tiles)
    node = solve(initial_board)
    print node.board
    node = node.previous
    while node:
        print node.board
        node = node.previous


class TestSequenceFunctions(unittest.TestCase):

    def setUp(self):
        self.a = Board([[1,2],[3,0]])
        self.b = Board([[2,1,3],[4,5,6],[7,8,0]])
        self.c = Board([[2,1,3],[4,5,6],[7,8,0]])
        self.d = Board([[0,1,3],[4,2,5],[7,8,6]])
        self.e = Board([[1,2,3],[4,5,6],[7,8,0]])


    def test_equals(self):
        self.assertTrue(self.b == self.c)
        self.assertTrue(self.b != self.a)
        self.assertTrue(self.d != self.c)

    def test_neighbors(self):
        self.assertTrue(len(self.d.neighbors()) == 2)
        self.assertTrue(len(self.b.neighbors()) == 2)

    def test_to_1d(self):
        self.assertTrue(to_1d(5,0,0) == 0)
        self.assertTrue(to_1d(3,1,2) == 4)

    def test_to_2d(self):
        self.assertTrue(to_2d(5,0) == (0,0))
        self.assertTrue(to_2d(3,4) == (1,1))

    def test_solve(self):
        self.assertTrue(solve(self.d).board == self.e)    


if __name__ == '__main__':
    test = False
    if test:
        suite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)
        unittest.TextTestRunner(verbosity=2).run(suite)
    else:
        main()
