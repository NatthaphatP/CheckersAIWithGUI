object CheckersRules {
  type Board = Vector[Vector[Piece]]
  val BoardSize = 8
  val SquareSize = 80

  def initialBoard(): Board = {
    Vector.tabulate(BoardSize, BoardSize) { (r, c) =>
      if ((r + c) % 2 == 1 && r < 3) Man(isBlack = false)
      else if ((r + c) % 2 == 1 && r > 4) Man(isBlack = true)
      else Empty
    }
  }

  def pieceMoves(board: Board, r: Int, c: Int, piece: Piece): Seq[Move] = {
    val dirs = if (piece.isKing) Seq((-1, -1), (-1, 1), (1, -1), (1, 1))
    else if (piece.isBlack) Seq((-1, -1), (-1, 1)) // Black moves forward (up)
    else Seq((1, -1), (1, 1)) // White moves forward (down)

    dirs.flatMap { case (dr, dc) =>
      val nr = r + dr
      val nc = c + dc
      val jumpR = r + 2 * dr
      val jumpC = c + 2 * dc

      if (isValidPosition(nr, nc) && board(nr)(nc) == Empty) {
        Seq(Move(r, c, nr, nc, None)) // Regular move
      } else if (isValidPosition(jumpR, jumpC)) {
        val jumpedPiece = board(nr)(nc)
        if (isValidJump(jumpedPiece, piece) && board(jumpR)(jumpC) == Empty) {
          Seq(Move(r, c, jumpR, jumpC, Some((nr, nc)))) // Valid jump
        } else Seq()
      } else Seq()
    }
  }

  def isValidPosition(row: Int, col: Int): Boolean = {
    row >= 0 && row < BoardSize && col >= 0 && col < BoardSize
  }

  def isValidJump(jumpedPiece: Piece, currentPiece: Piece): Boolean = {
    jumpedPiece != Empty && jumpedPiece.isBlack != currentPiece.isBlack
  }

  def applyMove(board: Board, move: Move): Board = {
    val piece = board(move.fromRow)(move.fromCol)
    var newBoard = board.updated(move.fromRow, board(move.fromRow).updated(move.fromCol, Empty))
    move.jumped.foreach { case (jr, jc) =>
      newBoard = newBoard.updated(jr, newBoard(jr).updated(jc, Empty)) // Remove the jumped piece
    }
    
    // Handle promotion to king
    val finalPiece = if (shouldBeKing(piece, move.toRow)) King(piece.isBlack) else piece
    newBoard.updated(move.toRow, newBoard(move.toRow).updated(move.toCol, finalPiece))
  }

  def shouldBeKing(piece: Piece, toRow: Int): Boolean = {
    (piece.isBlack && toRow == 0) || (!piece.isBlack && toRow == BoardSize - 1)
  }

  def checkWinner(board: Board, isBlackTurn: Boolean): Option[Boolean] = {
    if (!isGameOver(board)) {
      None // The game is not over yet
    } else {
      val opponentMoves = generateMoves(board, !isBlackTurn)
      if (opponentMoves.isEmpty) {
        Some(isBlackTurn) // Current player wins
      } else {
        Some(!isBlackTurn) // Opponent wins
      }
    }
  }

  def isGameOver(board: Board): Boolean = {
    val blackMoves = generateMoves(board, isBlackTurn = true)
    val whiteMoves = generateMoves(board, isBlackTurn = false)
    blackMoves.isEmpty || whiteMoves.isEmpty
  }

  def generateMoves(board: Board, isBlackTurn: Boolean): Seq[Move] = {
    // Find single jumps
    val singleJumps = for {
      r <- 0 until BoardSize
      c <- 0 until BoardSize
      piece = board(r)(c)
      if piece != Empty && piece.isBlack == isBlackTurn
      move <- pieceMoves(board, r, c, piece)
      if move.jumped.isDefined
    } yield move
    
    // If there are single jumps, return only those
    if (singleJumps.nonEmpty) {
      return singleJumps
    }
    
    // If no jumps are available, find regular moves
    for {
      r <- 0 until BoardSize
      c <- 0 until BoardSize
      piece = board(r)(c)
      if piece != Empty && piece.isBlack == isBlackTurn
      move <- pieceMoves(board, r, c, piece)
      if move.jumped.isEmpty
    } yield move
  }
  
  sealed trait Piece {
    val isBlack: Boolean
    val isKing: Boolean
  }

  case class Man(isBlack: Boolean) extends Piece {
    val isKing = false
  }

  case class King(isBlack: Boolean) extends Piece {
    val isKing = true
  }

  case class Move(
    fromRow: Int, 
    fromCol: Int, 
    toRow: Int, 
    toCol: Int, 
    jumped: Option[(Int, Int)]
  )

  case object Empty extends Piece {
    val isBlack = false
    val isKing = false
  }
}