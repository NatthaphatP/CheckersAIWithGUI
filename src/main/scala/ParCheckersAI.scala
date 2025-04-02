import CheckersRules._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object ParCheckersAI {
  val AI_DEPTH = 100

  def minimax(board: Board,
              depth: Int,
              isMaximizing: Boolean,
              alpha: Double,
              beta: Double,
              isBlackTurn: Boolean
             ): Double = {
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
            if (b <= a) return maxEval
          }
          maxEval
        } else {
          var minEval = Double.PositiveInfinity
          for (move <- possibleMoves) {
            val newBoard = applyMove(board, move)
            val eval = minimax(newBoard, depth - 1, true, a, b, isBlackTurn)
            minEval = math.min(minEval, eval)
            b = math.min(b, minEval)
            if (b <= a) return minEval
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

    var bestMove: Option[Move] = None
    var maxEval = Double.NegativeInfinity

    val futures = moves.map { move =>
      Future {
        val newBoard = applyMove(board, move)
        val eval = minimax(newBoard, depth - 1, false, Double.NegativeInfinity, Double.PositiveInfinity, isBlackTurn)
        (move, eval)
      }
    }

    val results = Await.result(Future.sequence(futures), Duration.Inf)

    // Pick the best move based on evaluation
    val (bestMoveResult, _) = results.maxBy(_._2)
    bestMove = Some(bestMoveResult)

    val elapsedMillis = (System.nanoTime() - startTime) / 1_000_000
    if (elapsedMillis >= timeLimitMillis) return None

    println(f"Best move at depth $depth: $bestMove with eval: $maxEval%.2f")
    bestMove
  }
}
