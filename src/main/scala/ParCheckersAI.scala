import CheckersRules.*

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, boundary}

object ParCheckersAI {
  val AI_DEPTH = 10  // Set the depth limit here

  def minimax(board: Board,
              depth: Int,
              isMaximizing: Boolean,
              alpha: Double,
              beta: Double,
              isBlackTurn: Boolean
             ): Double = boundary {
    // Stop recursion if depth limit is reached or the game is over
    if (depth == 0 || isGameOver(board)) {
      return evaluateBoard(board, isBlackTurn)
    } else {
      val possibleMoves = generateMoves(board, isMaximizing)
      if (possibleMoves.isEmpty) {
        return if (isMaximizing) Double.NegativeInfinity else Double.PositiveInfinity
      } else {
        var (a, b) = (alpha, beta)

        if (isMaximizing) {
          var maxEval = Double.NegativeInfinity
          for (move <- possibleMoves) {
            val newBoard = applyMove(board, move)
            val eval = minimax(newBoard, depth - 1, false, a, b, isBlackTurn)
            maxEval = math.max(maxEval, eval)
            a = math.max(a, maxEval)
            if (b <= a) boundary.break(maxEval)
          }
          maxEval
        } else {
          var minEval = Double.PositiveInfinity
          for (move <- possibleMoves) {
            val newBoard = applyMove(board, move)
            val eval = minimax(newBoard, depth - 1, true, a, b, isBlackTurn)
            minEval = math.min(minEval, eval)
            b = math.min(b, minEval)
            if (b <= a) boundary.break(minEval)
          }
          minEval
        }
      }
    }
  }

  def evaluateBoard(board: Board, isBlackTurn: Boolean): Double = {
    val pieceValue = 1.0
    val kingValue = 4.0

    def pieceScore(piece: Piece, row: Int, col: Int): Double = {
      if (piece.isKing) kingValue else pieceValue
    }

    val scores = for {
      (row, rIdx) <- board.zipWithIndex
      (piece, cIdx) <- row.zipWithIndex
      if piece != Empty
    } yield {
      val sign = if (piece.isBlack == isBlackTurn) 1 else -1
      sign * pieceScore(piece, rIdx, cIdx)
    }

    scores.sum + (Random.nextDouble() * 0.1)
  }

  def bestMove(board: Board, isBlackTurn: Boolean, timeLimitMillis: Long): Option[Move] = {
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
    benchmarkWithoutLimits(board, isBlackTurn, timeLimitMillis)
  }

  def benchmarkWithoutLimits(board: Board, isBlackTurn: Boolean, timeLimitMillis: Long): Option[Move] = {
    var bestMove: Option[Move] = None
    val aiStartTime = System.nanoTime()  // Start time for benchmarking

    // Run the AI without a depth or time limit. It will search until the game is over or another stopping condition is met.
    val moves = generateMoves(board, isBlackTurn)
    if (moves.isEmpty) return None

    var maxEval = Double.NegativeInfinity
    var bestMoveFound: Option[Move] = None

    moves.foreach { move =>
      val newBoard = applyMove(board, move)
      val eval = minimax(newBoard, AI_DEPTH, false, Double.NegativeInfinity, Double.PositiveInfinity, isBlackTurn)
      if (eval > maxEval) {
        maxEval = eval
        bestMoveFound = Some(move)
      }
    }

    bestMove = bestMoveFound

    val elapsedMillis = (System.nanoTime() - aiStartTime) / 1_000_000
    println(f"Total time for move calculation: $elapsedMillis ms")  // Print time taken in milliseconds

    bestMove
  }
}
