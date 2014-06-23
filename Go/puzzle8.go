// Solve the 8 puzzle using the A* algorithm with heuristic manhattan distance + 
// the number of moves to reach the board as its negative priority.
// Uses the priority queue from http://golang.org/pkg/container/heap/ with modifications
// to resize the underlying array as needed. Assumes the puzzle has a solution

package main

import (
	"bufio"
	"bytes"
	"container/heap"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

//************************************************************************
// representation of the puzzle board
//************************************************************************

type Board struct {
	n         int     // dimension of board
	board     [][]int // the board in an nxn slice with 0 representing the blank tile
	manhattan int     // manhattan distance of board to solution board
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func newBoard(tiles [][]int) (b *Board) {
	// create a board form an nxn slice, and set its manhattan distance	
	b = new(Board)
	b.n = len(tiles[0][:])
	b.board = make([][]int, b.n)
	for i := 0; i < b.n; i++ {
		b.board[i] = make([]int, b.n)
		for j := 0; j < b.n; j++ {
			tileNum := tiles[i][j]
			b.board[i][j] = tileNum

			// calculate the row and col of the tile in the solution board 
			// and passing the board its manhattan distance
			goalrow := (tileNum - 1) / b.n
			goalcol := (tileNum - 1) % b.n
			if b.board[i][j] != 0 {
				b.manhattan += abs(i-goalrow) + abs(j-goalcol)
			}
		}
	}
	return b
}

func equals(x, y *Board) bool {
	// two boards are equal of all of the tiles are in the same place	
	if x.n != y.n {
		return false
	}
	for i := 0; i < x.n; i++ {
		for j := 0; j < x.n; j++ {
			if x.board[i][j] == y.board[i][j] {
				continue
			} else {
				return false
			}
		}
	}
	return true
}

func (b Board) String() string {
	// Board implements Stringer interface
	n := len(b.board[0][:])
	var buffer bytes.Buffer
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			s := fmt.Sprintf("%2d ", b.board[i][j])
			buffer.WriteString(s)
		}
		buffer.WriteString("\n")
	}
	return buffer.String()
}

func (b Board) neighbors() (boards []*Board) {
	// Calculate the possible resulting board after a legal move
	// return a slice with the 2 to 4 resulting boards
	N := b.n
	var row, col int
	tiles := make([][]int, N)
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			if b.board[i][j] == 0 {
				row, col = i, j
			}
		}
	}
	north := []int{-1, 1, 0, 0} // move 0 (blanck) tile up or down
	east := []int{0, 0, 1, -1}  // move 0 tile left or right
	for i := 0; i < 4; i++ {
		r := row + north[i]
		c := col + east[i]
		// if (r,c) is on the board (i.e. a legal move) create a board and add it
		// to the slice boards
		if !(r < 0 || r >= N || c < 0 || c >= N) {
			for m := 0; m < N; m++ {
				tiles[m] = make([]int, N)
				for n := 0; n < N; n++ {
					tiles[m][n] = b.board[m][n]
				}
			}
			tiles[row][col] = b.board[r][c]
			tiles[r][c] = 0
			nb := newBoard(tiles)
			if nb != nil {
				boards = append(boards, nb)
			}
		}
	}
	return boards
}

//************************************************************************
// Priority Queue contains elements of type *Node
//************************************************************************

type Node struct {
	board    *Board
	m        int   // moves made to arrive and this board
	priority int   // manhattan distance of board + m, smaller is better
	previous *Node // a link to previous node, used to output all boards in solution
	index    int   // the index of the item in the heap
}

type PriorityQueue []*Node // implements heap.Interface and holds Nodes

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	// We want Pop to give us the lowest, not highest, priority so we use less than here.
	// In other words we want a min-queue
	return pq[i].priority < pq[j].priority
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x interface{}) {
	// Push and Pop use pointer receivers because they modify the slice's length,
	// not just its contents. If the queue reaches capacity we copy the items into
	// a new slice with double the capcity.
	qLen := len(*pq)
	if qLen == cap(*pq) {
		c := make([]*Node, qLen, qLen*2)
		copy(c, *pq)
		*pq = c
	}
	*pq = (*pq)[0 : qLen+1]
	node := x.(*Node)
	node.index = qLen
	(*pq)[qLen] = node
}

func (pq *PriorityQueue) Pop() interface{} {
	// 	When queue size reaches 1/4 capacity, copy it to a new queue with 1/2 the capacity
	qLen := len(*pq)
	node := (*pq)[qLen-1]
	node.index = -1 // for safety
	*pq = (*pq)[0 : qLen-1]
	if len(*pq) < cap(*pq)/4 {
		c := make([]*Node, len(*pq)/2)
		copy(c, *pq)
		*pq = c
	}
	return node
}

//************************************************************************
// Solve the puzzle
//************************************************************************

func NewNode(b *Board, m int, previous *Node) (node *Node) {
	node = new(Node)
	node.board = b
	node.m = m
	node.previous = previous
	node.priority = b.manhattan + m
	return node
}

func Solve(initial *Board) *Node {
	initialNode := NewNode(initial, 0, nil)
	frontier := make(PriorityQueue, 0, 1)
	heap.Push(&frontier, initialNode)
	var node *Node
	for {
		node = heap.Pop(&frontier).(*Node)
		if node.board.manhattan == 0 {
			return node
		}
		for _, b := range node.board.neighbors() {
			if node.previous == nil || !equals(b, node.previous.board) {
				heap.Push(&frontier, NewNode(b, node.m+1, node))
			}
		}
	}
	return node
}

func ReadBoard() ([][]int) {
	f, err := os.Open(os.Args[1]) // command line argument is path to input file
	if err != nil {
		fmt.Println(err)
		return nil
	}
	defer f.Close()
	r := bufio.NewReader(f)
	line, err := r.ReadString('\n')
	s := strings.Trim(line, "\n")
	n, _ := strconv.Atoi(s)
	var tiles [][]int
	for i := 0; i < n; i++ {
		var row []int
		line, err = r.ReadString('\n')
		u := strings.Replace(line, "  ", " ", -1)
		u = strings.TrimSpace(u)
		t := strings.Split(u, " ")
		for _, w := range t {
			num, _ := strconv.Atoi(w)
			row = append(row, num)
		}
		tiles = append(tiles, row)
	}
	return tiles
}

func main() {
	// input file format is first row is the dimension, remaining rows are the values
	// of the board squares separated by one or two spaces.
	var bd = newBoard(ReadBoard())
	t0 := time.Now()
	node := Solve(bd)
	t1 := time.Now()
	fmt.Println("Solved in ", node.m, "moves, ", t1.Sub(t0), "seconds.\n")
	// use defer to pring boards of solution in order -- initial board first.
	defer fmt.Println(node.board)
	for node.previous != nil {
		node = node.previous
		defer fmt.Println(node.board)
	}
}
