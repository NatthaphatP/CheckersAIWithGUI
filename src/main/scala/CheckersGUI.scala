import CheckersRules.*
import SeqCheckersAI.*
import ParCheckersAI.*
import javafx.scene.input.MouseEvent
import scalafx.animation.PauseTransition
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.TextArea
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.util.Duration

import java.io.PrintStream

object CheckersGUI extends JFXApp3 {
  var gameState: GameState = GameState(initialBoard(), Nil, 0)
  def board: Board = gameState.board
  var selected: Option[(Int, Int)] = None
  var isBlackTurn = true
  var hoveredPiece: Option[(Int, Int)] = None
  var previousMove: Option[Move] = None
  val AI_TIME_LIMIT_MS = 100000 // Set a large time limit for the AI
  val AI_DEPTH = 9

  // Add counter for consecutive non-capture moves
  var nonCaptureMovesCount: Int = 0
  val DRAW_AFTER_NON_CAPTURE_MOVES = 40

  var blackWins = 0
  var whiteWins = 0
  var draws = 0
  var gameCount = 0
  val totalGames = 1

  // Variables to track the total AI calculation time for sequential and parallel AIs
  var totalSequentialTime: Long = 0
  var totalParallelTime: Long = 0
  // Add counters for the number of moves each AI makes
  var sequentialMoveCount: Int = 0
  var parallelMoveCount: Int = 0

  override def start(): Unit = {
    val canvas = new Canvas(BoardSize * SquareSize, BoardSize * SquareSize)
    val gc = canvas.graphicsContext2D
    val textArea = new TextArea {
      editable = false
      prefHeight = 100
      style = "-fx-control-inner-background: black; -fx-text-fill: white;"
    }

    val ps = new PrintStream((b: Int) => {
      textArea.appendText(b.toChar.toString)
    })
    System.setOut(ps)
    System.setErr(ps)

    def draw(): Unit = {
      def drawPiece(
          gc: GraphicsContext,
          col: Int,
          row: Int,
          color: Color
      ): Unit = {
        gc.fill = color
        gc.fillOval(col * SquareSize + 10, row * SquareSize + 10, 60, 60)
      }

      def drawKing(
          gc: GraphicsContext,
          col: Int,
          row: Int,
          color: Color
      ): Unit = {
        drawPiece(gc, col, row, color)
        gc.fill = Color.Gold
        gc.setFont(new Font(20))
        gc.fillText("K", col * SquareSize + 30, row * SquareSize + 50)
      }

      gc.clearRect(0, 0, canvas.width.value, canvas.height.value)
      for (r <- 0 until BoardSize; c <- 0 until BoardSize) {
        gc.fill = if ((r + c) % 2 == 0) Color.Burlywood else Color.SaddleBrown
        gc.fillRect(c * SquareSize, r * SquareSize, SquareSize, SquareSize)

        if (hoveredPiece.contains((r, c))) {
          gc.fill = Color.Yellow
          gc.fillRect(c * SquareSize, r * SquareSize, SquareSize, SquareSize)
        }

        board(r)(c) match {
          case Man(true)   => drawPiece(gc, c, r, Color.Black)
          case Man(false)  => drawPiece(gc, c, r, Color.White)
          case King(true)  => drawKing(gc, c, r, Color.Black)
          case King(false) => drawKing(gc, c, r, Color.White)
          case _           =>
        }

        previousMove.foreach { move =>
          val fromX = move.fromCol * SquareSize + SquareSize / 2
          val fromY = move.fromRow * SquareSize + SquareSize / 2
          val toX = move.toCol * SquareSize + SquareSize / 2
          val toY = move.toRow * SquareSize + SquareSize / 2

          gc.stroke = Color.Blue
          gc.lineWidth = 3
          gc.strokeLine(fromX, fromY, toX, toY)

          val arrowSize = 10
          val angle = Math.atan2(toY - fromY, toX - fromX)
          val arrowX1 = toX - arrowSize * Math.cos(angle - Math.PI / 6)
          val arrowY1 = toY - arrowSize * Math.sin(angle - Math.PI / 6)
          val arrowX2 = toX - arrowSize * Math.cos(angle + Math.PI / 6)
          val arrowY2 = toY - arrowSize * Math.sin(angle + Math.PI / 6)

          gc.strokeLine(toX, toY, arrowX1, arrowY1)
          gc.strokeLine(toX, toY, arrowX2, arrowY2)

          if (move.jumped.nonEmpty) {
            gc.fill = Color.Red
            move.jumped.foreach { case (jr, jc) =>
              gc.fillOval(
                jc * SquareSize + SquareSize / 2 - 5,
                jr * SquareSize + SquareSize / 2 - 5,
                10,
                10
              )
            }
          }
        }
      }
    }

    def resetGame(): Unit = {
      gameState = GameState(initialBoard(), Nil, 0)
      selected = None
      hoveredPiece = None
      previousMove = None
      isBlackTurn = true
      nonCaptureMovesCount = 0
      draw()
    }

    def scheduleNextAIJump(currentMove: Move): Unit = {
      val additionalJumps = generateMoves(board, isBlackTurn).filter(m =>
        m.fromRow == currentMove.toRow && m.fromCol == currentMove.toCol && m.jumped.isDefined
      )

      if (additionalJumps.nonEmpty) {
        new PauseTransition(Duration(500)) {
          onFinished = _ => {
            val bestJump =
              if (isBlackTurn)
                ParCheckersAI
                  .bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS)
                  .getOrElse(additionalJumps.head)
              else
                SeqCheckersAI
                  .bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS)
                  .getOrElse(additionalJumps.head)

            println(s"AI multi-jump: $bestJump")
            previousMove = Some(bestJump)
            gameState = gameState.applyMove(bestJump)
            draw()
            scheduleNextAIJump(bestJump)
          }
        }.play()
      } else {
        isBlackTurn = !isBlackTurn
        selected = None
        draw()
        endPlayerTurn()
      }
    }

    def endPlayerTurn(): Unit = {
      // Check draw condition first
      if (nonCaptureMovesCount >= DRAW_AFTER_NON_CAPTURE_MOVES) {
        draws += 1
        println("Game drawn after 40 consecutive non-capture moves.")

        println(s"Score after game ${gameCount + 1}:")
        println(
          s"Black (ParAI): $blackWins | White (SeqAI): $whiteWins | Draws: $draws\n"
        )

        // Print the total AI calculation time for both AIs
        println(
          f"Total Sequential AI calculation time: ${totalSequentialTime / 1_000_000_000.0}%.6f seconds"
        )
        println(
          f"Total Parallel AI calculation time: ${totalParallelTime / 1_000_000_000.0}%.6f seconds"
        )

        // Print the average AI calculation time for both AIs
        val avgSequentialTime =
          if (sequentialMoveCount > 0)
            totalSequentialTime.toDouble / sequentialMoveCount / 1_000_000_000.0
          else 0.0

        val avgParallelTime =
          if (parallelMoveCount > 0)
            totalParallelTime.toDouble / parallelMoveCount / 1_000_000_000.0
          else 0.0

        println(
          f"Sequential AI (White): Average time per move: ${avgSequentialTime}%.6f seconds (${sequentialMoveCount} moves)"
        )
        println(
          f"Parallel AI (Black): Average time per move: ${avgParallelTime}%.6f seconds (${parallelMoveCount} moves)"
        )

        // Reset counters
        totalSequentialTime = 0
        totalParallelTime = 0
        sequentialMoveCount = 0
        parallelMoveCount = 0

        gameCount += 1
        nonCaptureMovesCount = 0

        if (gameCount >= totalGames) {
          println("All games completed.")
        } else {
          new PauseTransition(Duration(2000)) {
            onFinished = _ => {
              resetGame()
              endPlayerTurn()
            }
          }.play()
        }
      } else if (isGameOver(board)) {
        val blackHasMoves = generateMoves(board, isBlackTurn = true).nonEmpty
        val whiteHasMoves = generateMoves(board, isBlackTurn = false).nonEmpty

        val winner = (blackHasMoves, whiteHasMoves) match {
          case (true, false) => "Black"
          case (false, true) => "White"
          case _             => "None"
        }

        if (winner == "None" || gameState.isDraw) {
          draws += 1
          println("Game drawn.")
        } else {
          if (winner == "Black") blackWins += 1 else whiteWins += 1
          println(s"Game over! $winner wins.")
        }

        println(s"Score after game ${gameCount + 1}:")
        println(
          s"Black (ParAI): $blackWins | White (SeqAI): $whiteWins | Draws: $draws\n"
        )

        // Print the total AI calculation time for both AIs
        println(
          f"Total Sequential AI calculation time: ${totalSequentialTime / 1_000_000_000.0}%.6f seconds"
        )
        println(
          f"Total Parallel AI calculation time: ${totalParallelTime / 1_000_000_000.0}%.6f seconds"
        )

        // Print the average AI calculation time for both AIs
        val avgSequentialTime =
          if (sequentialMoveCount > 0)
            totalSequentialTime.toDouble / sequentialMoveCount / 1_000_000_000.0
          else 0.0

        val avgParallelTime =
          if (parallelMoveCount > 0)
            totalParallelTime.toDouble / parallelMoveCount / 1_000_000_000.0
          else 0.0

        println(
          f"Sequential AI (White): Average time per move: ${avgSequentialTime}%.6f seconds (${sequentialMoveCount} moves)"
        )
        println(
          f"Parallel AI (Black): Average time per move: ${avgParallelTime}%.6f seconds (${parallelMoveCount} moves)"
        )

        // Reset the counters for the next game
        totalSequentialTime = 0
        totalParallelTime = 0
        sequentialMoveCount = 0
        parallelMoveCount = 0

        gameCount += 1

        if (gameCount >= totalGames) {
          println("All games completed.")
        } else {
          new PauseTransition(Duration(2000)) {
            onFinished = _ => {
              resetGame()
              endPlayerTurn()
            }
          }.play()
        }
      } else {
        new PauseTransition(Duration(500)) {
          onFinished = _ => {
            println(
              s"${if (isBlackTurn) "Black" else "White"} AI is thinking..."
            )

            // Start tracking time before the AI move
            val startTime = System.nanoTime()

            val aiMove = SeqCheckersAI.bestMove(
              board,
              isBlackTurn,
              AI_DEPTH,
              AI_TIME_LIMIT_MS
            )

            // End time tracking after the AI move
            val endTime = System.nanoTime()
            val moveTime = endTime - startTime

            // Accumulate time for either sequential or parallel AI
            if (isBlackTurn) {
              totalParallelTime += moveTime
              parallelMoveCount += 1
            } else {
              totalSequentialTime += moveTime
              sequentialMoveCount += 1
            }

            aiMove match {
              case Some(am) =>
                println(s"AI move: $am")
                previousMove = Some(am)

                // Update non-capture moves counter
                if (am.jumped.isEmpty) {
                  nonCaptureMovesCount += 1
                  println(
                    s"Non-capture moves: $nonCaptureMovesCount/$DRAW_AFTER_NON_CAPTURE_MOVES"
                  )
                } else {
                  nonCaptureMovesCount = 0
                  println("Capture move! Non-capture counter reset to 0.")
                }

                gameState = gameState.applyMove(am)
                draw()
                if (am.jumped.isDefined) {
                  scheduleNextAIJump(am)
                } else {
                  // Check for draw condition based on non-capture moves
                  if (nonCaptureMovesCount >= DRAW_AFTER_NON_CAPTURE_MOVES) {
                    println("Game drawn due to consecutive non-capture moves.")
                    draws += 1
                    resetGame()
                  } else {
                    isBlackTurn = !isBlackTurn
                    selected = None
                    draw()
                    endPlayerTurn()
                  }
                }

              case None =>
                isBlackTurn = !isBlackTurn
                selected = None
                draw()
                endPlayerTurn()
            }
          }
        }.play()
      }
    }

    canvas.onMouseMoved = (e: MouseEvent) => {
      val c = (e.getX / SquareSize).toInt
      val r = (e.getY / SquareSize).toInt
      if (isValidPosition(r, c)) {
        hoveredPiece = Some((r, c))
        draw()
      }
    }

    canvas.onMousePressed = (e: MouseEvent) => {}
    canvas.onMouseReleased = (e: MouseEvent) => {}

    stage = new PrimaryStage {
      title = "Checkers AI vs AI"
      scene = new Scene(BoardSize * SquareSize, BoardSize * SquareSize + 100) {
        content = new VBox {
          children = Seq(canvas, textArea)
        }
      }
    }

    draw()
    endPlayerTurn()
  }
}
