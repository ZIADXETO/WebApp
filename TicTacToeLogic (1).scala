package tictactoe

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

import cs214.webapp.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer
import cs214.webapp.messages.Action
import cats.conversions.all

object TicTacToeStateMachine extends cs214.webapp.StateMachine[TicTacToeEvent, TicTacToeState, TicTacToeView]:

  val name: String = "tictactoe"
  val wire = TicTacToeWire

  override def init(clients: Seq[UserId]): TicTacToeState =
    val initialBoard = Board(List.fill(3, 3)(None))
    TicTacToeState.InProgress(initialBoard, clients.head, clients)
  // Failures in the Try must hold instances of AppException
  // (from Exceptions.scala under lib/shared/)
  override def transition(state: TicTacToeState)(uid: UserId, event: TicTacToeEvent): Try[Seq[Action[TicTacToeState]]] = { 
    state match
      case TicTacToeState.InProgress(board, currentPlayer, allPlayers) => 
        event match
          case TicTacToeEvent.Move(x, y) => 
            if uid == currentPlayer then
              if board.isValid(x ,y) then
                if board(x,y).isEmpty then
                  val newBoard = update(board, x, y, Some(uid))
                  val nextPlayer = allPlayers.find(_ != currentPlayer).getOrElse(currentPlayer)
                  val newState = changeToNextState(newBoard,uid,nextPlayer,allPlayers)
                  Success(Seq(Action.Render(newState)))
                else
                  Failure(new IllegalMoveException(" Already Occupied"))
              else
                Failure(new IllegalMoveException(" Illegal Move"))
            else
              Failure(new NotYourTurnException)
          
      case TicTacToeState.IsFinished(board, winner) => 
        event match
        case TicTacToeEvent.Move(x, y) => Failure(new  IllegalMoveException(" Already over"))
        case _ => Success(Seq(Action.Render(state)))
  }

  override def project(state: TicTacToeState)(uid: UserId): TicTacToeView =
    state match {
      case TicTacToeState.InProgress(board, currentPlayer, clients) =>
        val yourTurn = uid == currentPlayer
        TicTacToeView.Playing(board, yourTurn)

      case TicTacToeState.IsFinished(board, winner) =>
        TicTacToeView.Finished(winner)
    }
  private def update(board: Board, x: Int, y: Int, user: Option[UserId]): Board =
    Board(
      board.cells.zipWithIndex.map {
        case (row, i) =>
          row.zipWithIndex.map {
            case (_, j) =>
              if (i == x && j == y) user
              else board(i, j)
          }
      }
    )
  private def changeToNextState(board: Board, currentPlayer: UserId, nextPlayer: UserId, allPlayers: Seq[UserId]): TicTacToeState = 
  {
    if checkIfPlayerWon(board, currentPlayer) then
      TicTacToeState.IsFinished(board, Some(currentPlayer))
    else if board.isFull then
      TicTacToeState.IsFinished(board, None)
    else
      TicTacToeState.InProgress(board, nextPlayer, allPlayers)
}
  private def checkIfPlayerWon(board:Board, player : UserId): Boolean = 
      // CHECK COLUMNS, ROWN, DIAGS
         (board(0, 0) == Some(player) && board(1,0) == Some(player)  && board(2,0) == Some(player)) ||
         (board(0, 1) == Some(player) && board(1,1) == Some(player)  && board(2,1) == Some(player)) ||
         (board(0, 2) == Some(player) && board(1,2) == Some(player)  && board(2,2) == Some(player)) ||
        // ROWS
         (board(0, 0) == Some(player) && board(0,1) == Some(player)  && board(0,2) == Some(player)) ||
         (board(1, 0) == Some(player) && board(1,1) == Some(player)  && board(1,2) == Some(player)) ||
         (board(2, 0) == Some(player) && board(2,1) == Some(player)  && board(2,2) == Some(player)) ||
        // DIAGS
         (board(0, 0) == Some(player) && board(1,1) == Some(player)  && board(2,2) == Some(player)) ||
         (board(0, 2) == Some(player) && board(1,1) == Some(player)  && board(2,0) == Some(player))
// Server registration magic
class register:
  WebServer.register(TicTacToeStateMachine)
