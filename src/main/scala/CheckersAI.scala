import CheckersRules.*

import scala.util.boundary
import scala.util.Random
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

object CheckersAI {
  val AI_DEPTH = 100

  def minimax(board: Board,
              depth: Int,
              isMaximizing: Boolean,
              alpha: Double,
              beta: Double): Double = boundary {
    if (depth == 0 || isGameOver(board)) {
      return quiescence(board, isMaximizing, alpha, beta, 4) // depth-limited
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
            val eval = minimax(newBoard, depth - 1, isMaximizing = false, a, b)
            maxEval = math.max(maxEval, eval)
            a = math.max(a, maxEval)
            if (b <= a) boundary.break(maxEval)
          }
          maxEval
        } else {
          var minEval = Double.PositiveInfinity
          for (move <- possibleMoves) {
            val newBoard = applyMove(board, move)
            val eval = minimax(newBoard, depth - 1, isMaximizing = true, a, b)
            minEval = math.min(minEval, eval)
            b = math.min(b, minEval)
            if (b <= a) boundary.break(minEval)
          }
          minEval
        }
      }
    }
  }

  def quiescence(board: Board, isBlackTurn: Boolean, alpha: Double, beta: Double, depth: Int): Double = {
    val standPat = evaluateBoard(board, isBlackTurn)
    var a = math.max(alpha, standPat)
    if (a >= beta || depth == 0) return standPat

    val captureMoves = generateMoves(board, isBlackTurn).filter(_.jumped.isDefined)
    if (captureMoves.isEmpty) return standPat

    var value = standPat
    for (move <- captureMoves) {
      val newBoard = applyMove(board, move)
      val score = -quiescence(newBoard, !isBlackTurn, -beta, -a, depth - 1)
      value = math.max(value, score)
      a = math.max(a, score)
      if (a >= beta) return beta
    }
    value
  }

  def evaluateBoard(board: Board, isBlackTurn: Boolean): Double = {
    val pieceValue = 1.0
    val kingValue = 4.0
    val captureBonus = 0.6         // Reduced from 1.5
    val trappedPenalty = 0.4       // Reduced from 0.8

    def isCenter(col: Int): Boolean = col >= 2 && col <= 5

    def positionalValue(piece: Piece, row: Int, col: Int): Double = {
      val centerBonus = if (isCenter(col)) 0.2 else 0.0
      val forwardBonus = if (piece.isBlack) (7 - row) * 0.05 else row * 0.05
      val mobility = countPieceMoves(board, row, col)
      val kingMobility = if (piece.isKing) mobility * 0.05 else 0.0
      val isTrapped = piece.isKing && mobility == 0
      val trapPenalty = if (isTrapped) -trappedPenalty else 0.0
      centerBonus + forwardBonus + kingMobility + trapPenalty
    }

    val baseScores = for {
      (row, rIdx) <- board.zipWithIndex
      (piece, cIdx) <- row.zipWithIndex
      if piece != Empty
    } yield {
      val base = if (piece.isKing) kingValue else pieceValue
      val positionBonus = positionalValue(piece, rIdx, cIdx)
      val sign = if (piece.isBlack == isBlackTurn) 1 else -1
      sign * (base + positionBonus)
    }

    val captureCount = generateMoves(board, isBlackTurn).count(_.jumped.isDefined)

    baseScores.sum + (captureCount * captureBonus) + (Random.nextDouble() * 0.001) // less randomness
  }

  def countPieceMoves(board: Board, row: Int, col: Int): Int = {
    val piece = board(row)(col)
    if (piece == Empty) return 0
    generateMoves(board, piece.isBlack).count(m =>
      m.fromRow == row && m.fromCol == col
    )
  }

  def bestMove(board: Board, isBlackTurn: Boolean, maxDepth: Int, timeLimitMillis: Long): Option[Move] = {
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

  def iterativeDeepening(board: Board, isBlackTurn: Boolean, maxDepth: Int, timeLimitMillis: Long): Option[Move] = {
    var bestMove: Option[Move] = None
    val aiStartTime = System.nanoTime()
    var depth = 1
    var timeLimitExceeded = false

    while (depth <= maxDepth && !timeLimitExceeded) {
      val elapsedMillis = (System.nanoTime() - aiStartTime) / 1_000_000
      val remainingTime = timeLimitMillis - elapsedMillis

      println(s"Searching at depth: $depth with remaining time: $remainingTime ms")
      val tempBestMove = bestMoveAtDepth(board, isBlackTurn, depth, remainingTime)
      if (tempBestMove.isEmpty) {
        println(s"Time limit exceeded before starting depth $depth. Returning best move from depth ${depth - 1}.")
        timeLimitExceeded = true
      } else {
        bestMove = tempBestMove
        println(s"Finished searching at depth: $depth")
        depth += 1
      }
    }

    bestMove
  }

  def bestMoveAtDepth(board: Board, isBlackTurn: Boolean, depth: Int, timeLimitMillis: Long): Option[Move] = {
    val moves = generateMoves(board, isBlackTurn)
    if (moves.isEmpty) return None

    val startTime = System.nanoTime()

    val futures = moves.map { move =>
      Future {
        val newBoard = applyMove(board, move)
        val eval = minimax(newBoard, depth - 1, !isBlackTurn, Double.NegativeInfinity, Double.PositiveInfinity)
        (move, eval)
      }
    }

    try {
      val results = Await.result(Future.sequence(futures), Duration(timeLimitMillis, MILLISECONDS))
      val best = results.maxBy(_._2)
      println(f"Best move at depth $depth: ${best._1} with eval: ${best._2}%.2f")
      Some(best._1)
    } catch {
      case _: TimeoutException =>
        println("Parallel evaluation exceeded time limit.")
        None
    }
  }
}
