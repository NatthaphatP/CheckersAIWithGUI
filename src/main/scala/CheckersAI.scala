import CheckersRules.*

import scala.util.boundary
import scala.util.Random
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicReference

object CheckersAI {
  val AI_DEPTH = 100

  // Simplified minimax - no parallelization
  def minimax(
      board: Board,
      depth: Int,
      isMaximizing: Boolean,
      alpha: Double,
      beta: Double,
      maximizingPlayerIsBlack: Boolean
  ): Double = boundary {
    if (depth == 0 || isGameOver(board)) {
      return evaluateBoard(board, maximizingPlayerIsBlack)
    }

    // Current player's turn in this board state
    val currentPlayerIsBlack =
      if (isMaximizing) maximizingPlayerIsBlack else !maximizingPlayerIsBlack

    val possibleMoves = generateMoves(board, currentPlayerIsBlack)
    if (possibleMoves.isEmpty) {
      return if (isMaximizing) Double.NegativeInfinity
      else Double.PositiveInfinity
    }

    var (a, b) = (alpha, beta)

    if (isMaximizing) {
      var maxEval = Double.NegativeInfinity
      for (move <- possibleMoves) {
        val newBoard = applyMove(board, move)
        val eval = minimax(
          newBoard,
          depth - 1,
          false,
          a,
          b,
          maximizingPlayerIsBlack
        )
        maxEval = math.max(maxEval, eval)
        a = math.max(a, maxEval)
        if (b <= a) boundary.break(maxEval)
      }
      maxEval
    } else {
      var minEval = Double.PositiveInfinity
      for (move <- possibleMoves) {
        val newBoard = applyMove(board, move)
        val eval = minimax(
          newBoard,
          depth - 1,
          true,
          a,
          b,
          maximizingPlayerIsBlack
        )
        minEval = math.min(minEval, eval)
        b = math.min(b, minEval)
        if (b <= a) boundary.break(minEval)
      }
      minEval
    }
  }

  def evaluateBoard(board: Board, isBlackTurn: Boolean): Double = {
    // Material values
    val pieceValue = 1.0
    val kingValue = 4.0

    // Game stage detection
    val totalPieces = board.flatten.count(_ != Empty)
    val isEndgame = totalPieces <= 8

    def isCenter(row: Int, col: Int): Boolean = {
      val centerRows = row >= 2 && row <= 5
      val centerCols = col >= 2 && col <= 5
      centerRows && centerCols
    }

    def isInnerCenter(row: Int, col: Int): Boolean = {
      val innerRows = row >= 3 && row <= 4
      val innerCols = col >= 3 && col <= 4
      innerRows && innerCols
    }

    def isEdge(row: Int, col: Int): Boolean =
      row == 0 || row == 7 || col == 0 || col == 7

    def hasFriendlyNeighbor(piece: Piece, row: Int, col: Int): Boolean = {
      val directions = List((-1, -1), (-1, 1), (1, -1), (1, 1))
      directions.exists { case (dr, dc) =>
        val newRow = row + dr
        val newCol = col + dc
        if (newRow >= 0 && newRow < 8 && newCol >= 0 && newCol < 8) {
          val neighbor = board(newRow)(newCol)
          neighbor != Empty && neighbor.isBlack == piece.isBlack
        } else false
      }
    }

    def isSafe(piece: Piece, row: Int, col: Int): Boolean = {
      // A piece is safe if it cannot be captured
      val opponentMoves = generateMoves(board, !piece.isBlack)
      !opponentMoves.exists(move =>
        move.jumped.isEmpty &&
          row == (move.fromRow + move.toRow) / 2 &&
          col == (move.fromCol + move.toCol) / 2
      )
    }

    def positionalValue(piece: Piece, row: Int, col: Int): Double = {
      val isBlack = piece.isBlack

      // Basic positional bonuses
      val centerBonus =
        if (isInnerCenter(row, col)) 0.3
        else if (isCenter(row, col)) 0.2
        else 0.0

      val edgePenalty = if (isEdge(row, col)) -0.15 else 0.0

      // Forward progression (more valuable in early/midgame)
      val forwardBonus = if (!isEndgame) {
        if (isBlack) (7 - row) * 0.06 else row * 0.06
      } else 0.0

      // King mobility (more valuable in endgame)
      val mobilityFactor = if (isEndgame) 0.12 else 0.05
      val kingMobility =
        if (piece.isKing) countPieceMoves(board, row, col) * mobilityFactor
        else 0.0

      // Formation bonuses
      val formationBonus =
        if (hasFriendlyNeighbor(piece, row, col)) 0.1 else -0.1

      // Safety bonus
      val safetyBonus = if (isSafe(piece, row, col)) 0.2 else 0.0

      // Back row bonus (defensive position)
      val backRowBonus =
        if ((isBlack && row == 0) || (!isBlack && row == 7)) 0.12 else 0.0

      centerBonus + edgePenalty + forwardBonus + kingMobility +
        formationBonus + safetyBonus + backRowBonus
    }

    // Calculate threat potential
    def calculateThreatValue(): Double = {
      val currentPlayerMoves = generateMoves(board, isBlackTurn)
      val captureMoves = currentPlayerMoves.filter(_.jumped.isDefined)
      captureMoves.size * 0.1 // Each potential capture is worth 0.1
    }

    val scores = for {
      (row, rIdx) <- board.zipWithIndex
      (piece, cIdx) <- row.zipWithIndex
      if piece != Empty
    } yield {
      val base = if (piece.isKing) kingValue else pieceValue
      val positionBonus = positionalValue(piece, rIdx, cIdx)
      val sign = if (piece.isBlack == isBlackTurn) 1 else -1
      sign * (base + positionBonus)
    }

    // Add threat potential to the evaluation
    val threatValue = calculateThreatValue()
    val staticEval = scores.sum

    // Slight randomness to break ties
    staticEval + threatValue + (Random.nextDouble() * 0.01)
  }

  def countPieceMoves(board: Board, row: Int, col: Int): Int = {
    val piece = board(row)(col)
    if (piece == Empty) return 0
    generateMoves(board, piece.isBlack).count(move =>
      move.fromRow == row && move.fromCol == col
    )
  }

  def bestMove(
      board: Board,
      isBlackTurn: Boolean,
      maxDepth: Int,
      timeLimitMillis: Long
  ): Option[Move] = {
    val moves = generateMoves(board, isBlackTurn)
    if (moves.isEmpty) {
      println("No valid moves available.")
      return None
    }
    if (moves.size == 1) {
      println("Only one possible move. Returning it immediately.")
      return Some(moves.head)
    }

    println(s"${moves.size} possible moves. Starting search...")
    iterativeDeepening(board, isBlackTurn, maxDepth, timeLimitMillis)
  }

  def iterativeDeepening(
      board: Board,
      isBlackTurn: Boolean,
      maxDepth: Int,
      timeLimitMillis: Long
  ): Option[Move] = {
    var bestMove: Option[Move] = None
    val aiStartTime = System.nanoTime()
    var depth = 1
    var timeLimitExceeded = false

    while (depth <= maxDepth && !timeLimitExceeded) {
      val elapsedMillis = (System.nanoTime() - aiStartTime) / 1_000_000
      val remainingTime = timeLimitMillis - elapsedMillis

      println(
        s"Searching at depth: $depth with remaining time: $remainingTime ms"
      )
      val tempBestMove =
        bestMoveAtDepth(board, isBlackTurn, depth, remainingTime)
      if (tempBestMove.isEmpty) {
        println(
          s"Time limit exceeded before starting depth $depth. Returning best move from depth ${depth - 1}."
        )
        timeLimitExceeded = true
      } else {
        bestMove = tempBestMove
        println(s"Finished searching at depth: $depth")
        depth += 1
      }
    }

    bestMove
  }

  def bestMoveAtDepth(
      board: Board,
      isBlackTurn: Boolean,
      depth: Int,
      timeLimitMillis: Long
  ): Option[Move] = {
    val moves = generateMoves(board, isBlackTurn)
    if (moves.isEmpty) return None

    val startTime = System.nanoTime()

    // Parallel evaluation using Futures
    val futures = moves.map { move =>
      Future {
        val newBoard = applyMove(board, move)
        // When we make a move, opponent will be minimizing (isMaximizing=false)
        // and the maximizingPlayer is still the current player (isBlackTurn)
        val eval = minimax( // Use the non-parallel minimax here
          newBoard,
          depth - 1,
          false,
          Double.NegativeInfinity,
          Double.PositiveInfinity,
          isBlackTurn
        )
        (move, eval)
      }
    }

    try {
      val results = Await.result(
        Future.sequence(futures),
        Duration(timeLimitMillis, MILLISECONDS)
      )
      // We need to maximize our score (the current player's score)
      val best = results.maxBy(_._2)
      println(
        f"Best move at depth $depth: ${best._1} with eval: ${best._2}%.2f"
      )
      Some(best._1)
    } catch {
      case _: TimeoutException =>
        println("Parallel evaluation exceeded time limit.")
        None
    }
  }
}
