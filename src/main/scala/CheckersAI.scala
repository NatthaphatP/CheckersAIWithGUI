import CheckersRules.*

import scala.util.boundary
import scala.util.Random
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicReference

object CheckersAI {
  val AI_DEPTH = 100
  // Threshold for when to use parallelism - adjust based on performance testing
  val PARALLEL_THRESHOLD = 4

  def minimax(
      board: Board,
      depth: Int,
      isMaximizing: Boolean,
      alpha: Double,
      beta: Double,
      maximizingPlayerIsBlack: Boolean
  ): Double =
    parallelMinimax(
      board,
      depth,
      isMaximizing,
      alpha,
      beta,
      maximizingPlayerIsBlack,
      false
    )

  def parallelMinimax(
      board: Board,
      depth: Int,
      isMaximizing: Boolean,
      alpha: Double,
      beta: Double,
      maximizingPlayerIsBlack: Boolean,
      forceSequential: Boolean = false
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
