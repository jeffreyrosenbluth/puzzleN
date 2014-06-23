/*************************************************************************
 * Name: Jeffrey Rosenbluth
 * Email: jeffrey.rosenbluth@gmail.com
 *
 * Compilation:  javac Board.java
 * Execution:
 * Dependencies: 
 *
 * Description: An N x N puzzle board
 *
 *************************************************************************/

public class Board {
	private final int N; // dimension of board
	private final int[][] board;
	private final int newyork;
	
	public Board(int[][] blocks) {
		N = blocks.length;
		board =  new int[N][N];
		for (int i = 0; i < N; i++) 
			for (int j = 0; j < N; j++)
				board[i][j] = blocks[i][j];
		// calculate manhattan distance
		int dist = 0;
		int blockNum = 0;
		int goalrow = 0;
		int goalcol = 0;
		for (int i = 0; i < N; i++)
			for (int j = 0; j < N; j++) {
				blockNum = board[i][j];
				goalrow = (blockNum-1) / N;
				goalcol = (blockNum-1) % N;
				if (board[i][j] != 0) {
					dist += Math.abs(i - goalrow) + Math.abs(j - goalcol);
				}
			}
		newyork = dist;
	}

	public int dimension() {
		return N;
	}
	
	public int hamming() {
		int count = 0;
		for (int i = 0; i < N; i++)
			for (int j = 0; j < N; j++) {
				if (board[i][j] != 0 && board[i][j] != i * N + j + 1)
					count += 1;
			}
		return count;
	}
	
	public int manhattan() {
//		int dist = 0;
//		for (int i = 0; i < N; i++)
//			for (int j = 0; j < N; j++) {
//				int blockNum = board[i][j];
//				int goalrow = blockNum / N;
//				int goalcol = (blockNum-1) % N;
//				if (board[i][j] != 0)
//					dist += Math.abs(i - goalrow) + Math.abs(j - goalcol);
//			}
		return newyork;
	}
	
	public boolean isGoal() {
		if (hamming() == 0)
				return true;
		return false;
	}
	
	public Board twin() {
		int[][] blocks = new int[N][N];
		for (int i = 0; i < N; i++) 
			for (int j = 0; j < N; j++)
				blocks[i][j] = board[i][j];
		if (blocks[0][0] != 0 && blocks[0][1] != 0) {
			int temp = blocks[0][0];
			blocks[0][0] = blocks[0][1];
			blocks[0][1] = temp;
		} else {
			int temp = blocks[1][0];
			blocks[1][0] = blocks[1][1];
			blocks[1][1] = temp;
		}
		return new Board(blocks);
	}
	
	@Override
	public boolean equals(Object y) {
		 if (y == this) return true;
	     if (y == null) return false;
	     if (y.getClass() != this.getClass()) return false;
	     if (((Board) y).N != this.N) return false;
	     Board that = (Board) y;
	     int count = 0;
	     for (int i = 0; i < N; i++) 
			for (int j = 0; j < N; j++)
				if (this.board[i][j] == that.board[i][j])
					count += 1;
	     return (count == N*N);
	}
	
	public Iterable<Board> neighbors() {
		Stack<Board> stack = new Stack<Board>();
		int[][] blocks = new int[N][N];
		int row, col, r, c;
		row = col = r = c = 0;
		for (int i = 0; i < N; i++)
			for (int j = 0; j < N; j++) 
				if (board[i][j] == 0) {
					row = i;
					col = j;
				}
		int[] north = new int[]{-1, 1, 0, 0};
		int[] east  = new int[]{0, 0, 1, -1};
		for (int i = 0; i < 4; i++) {
			r = row + north[i];
			c = col + east[i];
			if (!(r < 0 || r >= N || c < 0 || c >= N)) {
				for (int m = 0; m < N; m++)
					for (int n = 0; n < N; n++)
						blocks[m][n] = board[m][n];
				blocks[row][col] = board[r][c];
				blocks[r][c] = 0;
				stack.push(new Board(blocks));
			}
		}
		return stack;
	}
	
	@Override
	public String toString() {
		 StringBuilder s = new StringBuilder();
		    s.append(N + "\n");
		    for (int i = 0; i < N; i++) {
		        for (int j = 0; j < N; j++) {
		            s.append(String.format("%2d ", board[i][j]));
		        }
		        s.append("\n");
		    }
		    return s.toString();
	}
	
	public static void main(String[] args) {
		In in = new In(args[0]);
	    int N = in.readInt();
	    int[][] blocks = new int[N][N];
	    for (int i = 0; i < N; i++)
	        for (int j = 0; j < N; j++)
	            blocks[i][j] = in.readInt();
	    Board initial = new Board(blocks);
	    StdOut.print(initial);
	    StdOut.println("hamming: "+ initial.hamming());
	    StdOut.println("manhattan: "+ initial.manhattan());
	    StdOut.println("isGoal: "+ initial.isGoal());
	    StdOut.print(initial.twin());
	    StdOut.println("equals this: "+ initial.equals(initial));
	    for (Board b : initial.neighbors())
	    	StdOut.print(b);
	}

}
