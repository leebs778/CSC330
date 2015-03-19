# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  Cheat_Piece = [[[0, 0]]]
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
	               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
	               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
	               [[0, 0], [0, -1], [0, 1], [0, 2]]],
	               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
	               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
	               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
	               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
	               [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
	               	[[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]], #5 long (only needs 2 rotations)	
	               rotations([[0, 0], [0, 1], [1, 1] ]), # 3 block square
	               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2,1]]) #5 piece rectangle
					]
	def self.next_piece (board)
    	MyPiece.new(All_My_Pieces.sample, board)
  	end 

  	def self.gen_cheat_piece (board)
    	MyPiece.new(Cheat_Piece, board)
  	end
end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_flag = false
  end

  def next_piece
	if @cheat_flag
		@current_block = MyPiece.gen_cheat_piece(self)
		@cheat_flag = false
	else
		@current_block = MyPiece.next_piece(self)
	end
	@current_pos = nil
   end


  def cheat 
  	if @score >= 100 and not @cheat_flag
  		@score -= 100
  		@cheat_flag = true
  	end 
  end

  def rotate_180
  	rotate_clockwise
  	rotate_clockwise
  end


  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end



end

class MyTetris < Tetris
  # your enhancements here
  def set_board
	    @canvas = TetrisCanvas.new
	    @board = MyBoard.new(self)
	    @canvas.place(@board.block_size * @board.num_rows + 3,
	                  @board.block_size * @board.num_columns + 6, 24, 80)
	    @board.draw
  end

  def key_bindings  
    super
	@root.bind('u', lambda {@board.rotate_180})
	@root.bind('c', lambda {@board.cheat})
  end

end


