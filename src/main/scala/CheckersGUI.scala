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
  val AI_TIME_LIMIT_MS = 5000
  val AI_DEPTH = 100

  var blackWins = 0
  var whiteWins = 0
  var draws = 0
  var gameCount = 0
  val totalGames = 10

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
      def drawPiece(gc: GraphicsContext, col: Int, row: Int, color: Color): Unit = {
        gc.fill = color
        gc.fillOval(col * SquareSize + 10, row * SquareSize + 10, 60, 60)
      }

      def drawKing(gc: GraphicsContext, col: Int, row: Int, color: Color): Unit = {
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
          case Man(true) => drawPiece(gc, c, r, Color.Black)
          case Man(false) => drawPiece(gc, c, r, Color.White)
          case King(true) => drawKing(gc, c, r, Color.Black)
          case King(false) => drawKing(gc, c, r, Color.White)
          case _ =>
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
              gc.fillOval(jc * SquareSize + SquareSize / 2 - 5, jr * SquareSize + SquareSize / 2 - 5, 10, 10)
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
                SeqCheckersAI.bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS).getOrElse(additionalJumps.head)
              else
                ParCheckersAI.bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS).getOrElse(additionalJumps.head)

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
      if (isGameOver(board)) {
        val blackHasMoves = generateMoves(board, isBlackTurn = true).nonEmpty
        val whiteHasMoves = generateMoves(board, isBlackTurn = false).nonEmpty

        val winner = (blackHasMoves, whiteHasMoves) match {
          case (true, false) => "Black"
          case (false, true) => "White"
          case _ => "None"
        }

        if (winner == "None" || gameState.isDraw) {
          draws += 1
          println("Game drawn.")
        } else {
          if (winner == "Black") blackWins += 1 else whiteWins += 1
          println(s"Game over! $winner wins.")
        }

        println(s"Score after game ${gameCount + 1}:")
        println(s"Black (SeqAI): $blackWins | White (ParAI): $whiteWins | Draws: $draws\n")

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
            println(s"${if (isBlackTurn) "Black" else "White"} AI is thinking...")
            val aiMove =
              if (isBlackTurn)
                SeqCheckersAI.bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS)
              else
                ParCheckersAI.bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS)

            aiMove match {
              case Some(am) =>
                println(s"AI move: $am")
                previousMove = Some(am)
                gameState = gameState.applyMove(am)
                draw()
                if (am.jumped.isDefined) {
                  scheduleNextAIJump(am)
                } else {
                  isBlackTurn = !isBlackTurn
                  selected = None
                  draw()
                  endPlayerTurn()
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

