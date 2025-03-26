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
  var board: Board = initialBoard()
  var selected: Option[(Int, Int)] = None
  var isBlackTurn = true
  var hoveredPiece: Option[(Int, Int)] = None
  var previousMove: Option[Move] = None
  var aiStartTime: Long = 0L
  val AI_TIME_LIMIT_MS = 5000

  override def start(): Unit = {
    val canvas = new Canvas(BoardSize * SquareSize, BoardSize * SquareSize)
    val gc = canvas.graphicsContext2D
    val textArea = new TextArea {
      editable = false
      prefHeight = 100
      style = "-fx-control-inner-background: black; -fx-text-fill: white;"
    }

    // Redirect standard output to the TextArea
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

        // Highlight hovered square
        if (hoveredPiece.contains((r, c))) {
          gc.fill = Color.Yellow
          gc.fillRect(c * SquareSize, r * SquareSize, SquareSize, SquareSize)
        }

        // Draw pieces
        board(r)(c) match {
          case Man(true) => drawPiece(gc, c, r, Color.Black)
          case Man(false) => drawPiece(gc, c, r, Color.White)
          case King(true) => drawKing(gc, c, r, Color.Black)
          case King(false) => drawKing(gc, c, r, Color.White)
          case _ =>
        }

        // Draw arrow for the previous move
        previousMove.foreach { move =>
          val fromX = move.fromCol * SquareSize + SquareSize / 2
          val fromY = move.fromRow * SquareSize + SquareSize / 2
          val toX = move.toCol * SquareSize + SquareSize / 2
          val toY = move.toRow * SquareSize + SquareSize / 2

          gc.stroke = Color.Blue
          gc.lineWidth = 3
          gc.strokeLine(fromX, fromY, toX, toY)

          // Draw arrowhead
          val arrowSize = 10
          val angle = Math.atan2(toY - fromY, toX - fromX)
          val arrowX1 = toX - arrowSize * Math.cos(angle - Math.PI / 6)
          val arrowY1 = toY - arrowSize * Math.sin(angle - Math.PI / 6)
          val arrowX2 = toX - arrowSize * Math.cos(angle + Math.PI / 6)
          val arrowY2 = toY - arrowSize * Math.sin(angle + Math.PI / 6)

          gc.strokeLine(toX, toY, arrowX1, arrowY1)
          gc.strokeLine(toX, toY, arrowX2, arrowY2)
          
          // Draw intermediate points for multi-jumps
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

    // Mouse hover highlighting
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

    canvas.onMouseReleased = (e: javafx.scene.input.MouseEvent) => {
      val c = (e.getX / SquareSize).toInt
      val r = (e.getY / SquareSize).toInt
      selected match {
        case Some((sr, sc)) =>
          val validMoves = generateMoves(board, isBlackTurn)
          val move = validMoves.find(m => m.fromRow == sr && m.fromCol == sc && m.toRow == r && m.toCol == c)
          move match {
            case Some(m) =>
              println(s"Move applied: $m")
              board = applyMove(board, m)
              previousMove = Some(m) // Track the player's move
              draw()
              endPlayerTurn() // Handle turn switching
            case None =>
              println(s"Invalid move from ($sr, $sc) to ($r, $c)")
              selected = None
              draw()
          }
        case None =>
          println("No piece selected")
      }
    }

    def displayWinner(): Unit = {
      checkWinner(board, isBlackTurn) match {
        case Some(true) => println("Game over! The winner is Black.")
        case Some(false) => println("Game over! The winner is White.")
        case None => println("The game is not over yet.")
      }
    }

    def findAdditionalJumps(currentMove: Move): Seq[Move] = {
      generateMoves(board, isBlackTurn).filter(move =>
        move.fromRow == currentMove.toRow && move.fromCol == currentMove.toCol && move.jumped.isDefined
      )
    }

    @tailrec
    def endPlayerTurn(): Unit = {
      if (isGameOver(board)) {
        displayWinner()
        new PauseTransition(Duration(2500)) {
          onFinished = _ => {
            System.exit(0) // End the game if no moves are available
          }
        }.play()
      } else {
        if (!isBlackTurn) { // If it's the AI's turn
          // Delay the AI's move to allow the player's move to render
          new PauseTransition(Duration(500)) {
            onFinished = _ => {
              println("AI is thinking...")
              // Check if there's only one move available first
              val availableMoves = generateMoves(board, isBlackTurn)
              val aiMove = if (availableMoves.size == 1) {
                Some(availableMoves.head)
              } else {
                bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS)
              }
              
              aiMove.foreach { am =>
                println(s"AI move: $am")
                previousMove = Some(am) // Track the AI's move
                board = applyMove(board, am)
                draw()

                // Handle multi-jumps for the AI
                if (am.jumped.isDefined) {
                  // Use scheduled version with delay instead of recursion
                  scheduleNextAIJump(am)
                } else {
                  // Not a jump move, end AI turn
                  isBlackTurn = true
                  selected = None
                }
              }

              if (aiMove.isEmpty || aiMove.exists(_.jumped.isEmpty)) {
                // If no move was found or if the move wasn't a jump, end AI turn
                isBlackTurn = true
                selected = None
                draw()
              }
            }
          }.play()
        } else {
          // Check if the player's move was a jump and if additional jumps are available
          var hasAdditionalJumps = false
          previousMove.foreach { pm =>
            if (pm.jumped.isDefined) {
              val additionalJumps = findAdditionalJumps(pm)
              if (additionalJumps.nonEmpty) {
                println("You have additional jumps available. Continue jumping!")
                selected = Some((pm.toRow, pm.toCol)) // Keep the piece selected for the next jump
                draw()
                hasAdditionalJumps = true // Set the flag to indicate additional jumps
              }
            }
          }

          // If there are no additional jumps, switch to AI's turn
          if (!hasAdditionalJumps) {
            // Switch turn to the AI
            isBlackTurn = false
            selected = None // Reset selected piece
            draw()
            endPlayerTurn()
          }
        }
      }
    }

    def scheduleNextAIJump(currentMove: Move): Unit = {
      val additionalJumps = findAdditionalJumps(currentMove)
      
      if (additionalJumps.nonEmpty) {
        // Add a delay before the next jump for better visualization
        new PauseTransition(Duration(500)) {
          onFinished = _ => {
            val bestJump = if (additionalJumps.size == 1) {
              additionalJumps.head
            } else {
              bestMove(board, isBlackTurn, AI_DEPTH, AI_TIME_LIMIT_MS).getOrElse(additionalJumps.head)
            }
            
            println(s"AI multi-jump: $bestJump")
            previousMove = Some(bestJump) // Update previousMove to show the current jump
            board = applyMove(board, bestJump)
            draw()
            
            // Check for more jumps
            val furtherJumps = findAdditionalJumps(bestJump)
            if (furtherJumps.nonEmpty) {
              // If there are more jumps, schedule the next one
              scheduleNextAIJump(bestJump)
            } else {
              // No more jumps, end AI turn
              println("AI multi-jump sequence completed.")
              isBlackTurn = true
              selected = None
              draw()
            }
          }
        }.play()
      } else {
        // No additional jumps, end AI turn
        isBlackTurn = true
        selected = None
        draw()
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