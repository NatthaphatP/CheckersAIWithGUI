import CheckersAI.*
import CheckersRules.*
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

import java.io.{OutputStream, PrintStream}
import scala.annotation.tailrec

object CheckersGUI extends JFXApp3 {
  // GUI State
  var gameState: GameState = GameState(initialBoard(), Nil, 0)
  def board: Board = gameState.board
  var selected: Option[(Int, Int)] = None
  var isBlackTurn = true
  var hoveredPiece: Option[(Int, Int)] = None
  var previousMove: Option[Move] = None
  var aiStartTime: Long = 0L
  val AI_TIME_LIMIT_MS = 1000

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
              gc.fillOval(jc * SquareSize + SquareSize/2 - 5, jr * SquareSize + SquareSize/2 - 5, 10, 10)
            }
          }
        }
      }
    }

    draw()

    def handleMouseMoveAndDrag(e: MouseEvent): Unit = {
      val c = (e.getX / SquareSize).toInt
      val r = (e.getY / SquareSize).toInt
      if (isValidPosition(r, c)) {
        hoveredPiece = Some((r, c))
        draw()
      }
    }

    canvas.onMouseMoved = handleMouseMoveAndDrag
    canvas.onMouseDragged = handleMouseMoveAndDrag

    canvas.onMousePressed = (e: MouseEvent) => {
      val c = (e.getX / SquareSize).toInt
      val r = (e.getY / SquareSize).toInt
      if (isValidPosition(r, c) && board(r)(c) != Empty && board(r)(c).isBlack == isBlackTurn) {
        selected = Some((r, c))
        println(s"Piece selected at: ($r, $c)")
      }
    }

    canvas.onMouseReleased = (e: MouseEvent) => {
      val c = (e.getX / SquareSize).toInt
      val r = (e.getY / SquareSize).toInt
      selected match {
        case Some((sr, sc)) =>
          val validMoves = generateMoves(board, isBlackTurn)
          val move = validMoves.find(m => m.fromRow == sr && m.fromCol == sc && m.toRow == r && m.toCol == c)
          move match {
            case Some(m) =>
              println(s"Move applied: $m")
              gameState = gameState.applyMove(m)
              previousMove = Some(m)
              draw()
              checkForDraw()
              endPlayerTurn()
            case None =>
              println(s"Invalid move from ($sr, $sc) to ($r, $c)")
              selected = None
              draw()
          }
        case None => println("No piece selected")
      }
    }

    def checkForDraw(): Unit = {
      if (gameState.isDraw) {
        println("Game drawn by 40-move rule or threefold repetition.")
        System.exit(0)
      }
    }

    def scheduleNextAIJump(currentMove: Move): Unit = {
      val additionalJumps = generateMoves(board, isBlackTurn).filter(m =>
        m.fromRow == currentMove.toRow && m.fromCol == currentMove.toCol && m.jumped.isDefined
      )

      if (additionalJumps.nonEmpty) {
        new PauseTransition(Duration(500)) {
          onFinished = _ => {
            val bestJump = if (additionalJumps.size == 1) {
              additionalJumps.head
            } else {
              bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS).getOrElse(additionalJumps.head)
            }
            println(s"AI multi-jump: $bestJump")
            previousMove = Some(bestJump)
            gameState = gameState.applyMove(bestJump)
            draw()
            checkForDraw()
            scheduleNextAIJump(bestJump)
          }
        }.play()
      } else {
        isBlackTurn = true
        selected = None
        draw()
      }
    }

    @tailrec
    def endPlayerTurn(): Unit = {
      if (isGameOver(board)) {
        checkForDraw()
        println("Game over!")
        new PauseTransition(Duration(2500)) {
          onFinished = _ => System.exit(0)
        }.play()
      } else {
        if (!isBlackTurn) {
          new PauseTransition(Duration(500)) {
            onFinished = _ => {
              println("AI is thinking...")
              val availableMoves = generateMoves(board, isBlackTurn)
              val aiMove = if (availableMoves.size == 1) Some(availableMoves.head)
              else bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS)

              aiMove.foreach { am =>
                println(s"AI move: $am")
                previousMove = Some(am)
                gameState = gameState.applyMove(am)
                draw()
                checkForDraw()
                if (am.jumped.isDefined) scheduleNextAIJump(am)
                else {
                  isBlackTurn = true
                  selected = None
                  draw()
                }
              }

              if (aiMove.isEmpty || aiMove.exists(_.jumped.isEmpty)) {
                isBlackTurn = true
                selected = None
                draw()
              }
            }
          }.play()
        } else {
          var hasAdditionalJumps = false
          previousMove.foreach { pm =>
            if (pm.jumped.isDefined) {
              val additionalJumps = generateMoves(board, isBlackTurn).filter(move =>
                move.fromRow == pm.toRow && move.fromCol == pm.toCol && move.jumped.isDefined)
              if (additionalJumps.nonEmpty) {
                println("You have additional jumps available. Continue jumping!")
                selected = Some((pm.toRow, pm.toCol))
                draw()
                hasAdditionalJumps = true
              }
            }
          }

          if (!hasAdditionalJumps) {
            isBlackTurn = false
            selected = None
            draw()
            endPlayerTurn()
          }
        }
      }
    }

    stage = new PrimaryStage {
      title = "Checkers AI"
      scene = new Scene(BoardSize * SquareSize, BoardSize * SquareSize + 100) {
        content = new VBox {
          children = Seq(canvas, textArea)
        }
      }
    }
  }
}
