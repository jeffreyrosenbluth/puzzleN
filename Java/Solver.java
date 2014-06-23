/*************************************************************************
 * Name: Jeffrey Rosenbluth
 * Email: jeffrey.rosenbluth@gmail.com
 *
 * Compilation:  javac Solver.java
 * Execution:
 * Dependencies: .java
 *
 * Description: Solve an N x N puzzle board
 *
 *************************************************************************/
public class Solver {
	private Node winning;
	private MinPQ<Node> frontier;
	private MinPQ<Node>  backcountry;
	private boolean solveable = false;
	
	private class Node implements Comparable<Node> {
		private Board board;
		private int M; // moves to reach board;
		private Node previous;
		
		public Node(Board board, int M, Node previous) {
			this.board = board;
			this.M = M;
			this.previous = previous;
		}
		
        @Override
		public int compareTo(Node that) {
			if (this.board.manhattan() + this.M  < 
					that.board.manhattan() + that.M) {
                        return -1;
                    }
			else if (this.board.manhattan() + this.M >
					that.board.manhattan() + that.M) {
                        return 1;
                    }
			else {
                        return 0;
                    }
	    }
	}

	public Solver(Board initial) {
		frontier = new MinPQ<>();
		backcountry = new MinPQ<>();
		Node aNode = new Node(initial, 0, null);
		frontier.insert(aNode);
		Node bNode = new Node(initial.twin(), 0, null);
		backcountry.insert(bNode);
		Node node, twin;
	    while (true) {
	    	node = frontier.delMin();
	    	twin = backcountry.delMin();
	    	if (node.board.isGoal()) {
	    		solveable = true;
	    		break;
	    	} else if (twin.board.isGoal()) {
	    		break;
	    	}
	    	for (Board b : node.board.neighbors()) {
	    		if (node.previous == null || !b.equals(node.previous.board)) {
                    frontier.insert(new Node(b, node.M+1, node));
                }
	    	}
	    	for (Board b : twin.board.neighbors()) {
	    		if (twin.previous == null || !b.equals(twin.previous.board)) {
                    backcountry.insert(new Node(b, twin.M+1, node));
                }
	    	}
	    }
    	winning = node;
	}
	
	public boolean isSolvable() {
		return solveable;
	}
	
	public int moves() {
		if (!isSolvable()) return -1;
		int m = 0;
		Node n = winning;
		while (n != null) {
			m++;
			n = n.previous;
		}
		return m-1; // minimum number of moves
	}
	
	public Iterable<Board> solution() {
		if (!isSolvable()) {
                return null;
            }
		Stack<Board> stack = new Stack<>();
		stack.push(winning.board);
		Node node = winning.previous;
		while (node != null) {
			stack.push(node.board);
			node = node.previous;
		}
		return stack;
	}
	
	public static void main(String[] args) {
		// create initial board from file
	    In in = new In(args[0]);
	    int N = in.readInt();
	    int[][] blocks = new int[N][N];
	    for (int i = 0; i < N; i++) {
                for (int j = 0; j < N; j++) {
                    blocks[i][j] = in.readInt();
                }
            }
	    Board initial = new Board(blocks);

	    // solve the puzzle
	    Stopwatch timer = new Stopwatch();
	    Solver solver = new Solver(initial);
	    StdOut.println("Elapsed time: " + timer.elapsedTime());
	    // print solution to standard output
	    if (!solver.isSolvable()) {
            StdOut.println("No solution possible");
        }
	    else {
	        StdOut.println("Minimum number of moves = " + solver.moves());
	        for (Board board : solver.solution()) {
                StdOut.println(board);
            }
	    }
	}

}
