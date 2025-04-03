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

    // Use parallelism only for nodes near the root of the search tree
    // and only when we have multiple branches to explore
    if (
      !forceSequential && depth >= PARALLEL_THRESHOLD && possibleMoves.size > 1
    ) {
      if (isMaximizing) {
        // Process first move sequentially to potentially improve alpha (Young Brothers Wait)
        val firstMove = possibleMoves.head
        val firstBoard = applyMove(board, firstMove)
        val currentAlpha = new AtomicReference[Double](alpha)
        var maxEval = parallelMinimax(
          firstBoard,
          depth - 1,
          false,
          currentAlpha.get(),
          beta,
          maximizingPlayerIsBlack,
          true
        )
        currentAlpha.set(math.max(currentAlpha.get(), maxEval))

        // If we can already prune, return early
        if (beta <= currentAlpha.get()) {
          return maxEval
        }

        // Process remaining moves in parallel with updated alpha
        val remainingMoves = possibleMoves.tail
        val futureEvals = remainingMoves.map { move =>
          Future {
            val newBoard = applyMove(board, move)
            parallelMinimax(
              newBoard,
              depth - 1,
              false,
              currentAlpha.get(),
              beta,
              maximizingPlayerIsBlack,
              true
            )
          }
        }

        try {
          val results = Await.result(Future.sequence(futureEvals), Duration.Inf)
          (results :+ maxEval).max // Include the result from the first move
        } catch {
          case e: Exception =>
            println(s"Error in parallel minimax: ${e.getMessage}")
            maxEval // Return at least the first evaluation if parallel fails
        }
      } else {
        // Similar approach for minimizing player
        val firstMove = possibleMoves.head
        val firstBoard = applyMove(board, firstMove)
        val currentBeta = new AtomicReference[Double](beta)
        var minEval = parallelMinimax(
          firstBoard,
          depth - 1,
          true,
          alpha,
          currentBeta.get(),
          maximizingPlayerIsBlack,
          true
        )
        currentBeta.set(math.min(currentBeta.get(), minEval))

        if (currentBeta.get() <= alpha) {
          return minEval
        }

        val remainingMoves = possibleMoves.tail
        val futureEvals = remainingMoves.map { move =>
          Future {
            val newBoard = applyMove(board, move)
            parallelMinimax(
              newBoard,
              depth - 1,
              true,
              alpha,
              currentBeta.get(),
              maximizingPlayerIsBlack,
              true
            )
          }
        }

        try {
          val results = Await.result(Future.sequence(futureEvals), Duration.Inf)
          (results :+ minEval).min
        } catch {
          case e: Exception =>
            println(s"Error in parallel minimax: ${e.getMessage}")
            minEval
        }
      }
    } else {
      // Use sequential alpha-beta pruning for deeper nodes
      var (a, b) = (alpha, beta)

      if (isMaximizing) {
        var maxEval = Double.NegativeInfinity
        for (move <- possibleMoves) {
          val newBoard = applyMove(board, move)
          val eval = parallelMinimax(
            newBoard,
            depth - 1,
            false,
            a,
            b,
            maximizingPlayerIsBlack,
            true
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
          val eval = parallelMinimax(
            newBoard,
            depth - 1,
            true,
            a,
            b,
            maximizingPlayerIsBlack,
            true
          )
          minEval = math.min(minEval, eval)
          b = math.min(b, minEval)
          if (b <= a) boundary.break(minEval)
        }
        minEval
      }
    }
  }

  def evaluateBoard(board: Board, isBlackTurn: Boolean): Double = {
    // Material values
    val pieceValue = 1.0
    val kingValue = 4.0

    def isCenter(col: Int): Boolean = col >= 2 && col <= 5

    def positionalValue(piece: Piece, row: Int, col: Int): Double = {
      val centerBonus = if (isCenter(col)) 0.2 else 0.0
      val forwardBonus = if (piece.isBlack) (7 - row) * 0.05 else row * 0.05
      val kingMobility = if (piece.isKing) countPieceMoves(board, row, col) * 0.05 else 0.0
      centerBonus + forwardBonus + kingMobility
    }

    val scores = for {
      (row, rIdx) <- board.zipWithIndex
      (piece, cIdx) <- row.zipWithIndex
      if piece != Empty
    } yield {
      val sign = if (piece.isBlack == isBlackTurn) 1 else -1
      sign * pieceScore(piece, rIdx, cIdx)
    }

    scores.sum + (Random.nextDouble() * 0.01) // tiny randomness to break ties
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

    val futures = moves.map { move =>
      Future {
        val newBoard = applyMove(board, move)
        // When we make a move, opponent will be minimizing (isMaximizing=false)
        // and the maximizingPlayer is still the current player (isBlackTurn)
        val eval = parallelMinimax(
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
