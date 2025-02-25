
package tictactoe

import cs214.webapp.UserId

import scala.util.{Failure, Success, Try}

/** Stores all information about the current game. */
enum TicTacToeState : 
  case InProgress(board: Board, currentPlayer: UserId, allPlayers: Seq[UserId])
  case IsFinished(board: Board, winner: Option[UserId])

  // Change this type to hold actual information (use an enum, class, …)

/** There is only one event in tic-tac-toe: clicking a cell. */
enum TicTacToeEvent:
  /** User clicked cell (x, y) */
  case Move(x: Int, y: Int)

/** Client views reflect the state of the game: playing or finished. */
enum TicTacToeView:
  /** Game in progress. */
  case Playing(board: Board, yourTurn: Boolean)

  /** Game over. [[winner]] is [[None]] if the game ended in a tie. */
  case Finished(winner: Option[UserId])

// Change this class definition to store board states.
case class Board(cells: List[List[Option[UserId]]]):
  /** Get the value in the cell at (r, c). */
  def apply(r: Int, c: Int): Option[UserId] =
     // Add an appropriate precondition
    require(r >= 0 && r < cells.length && c >= 0 && c < cells(r).length)
    cells(r)(c)
  def isFull: Boolean = cells.flatten.forall(_.isDefined)
  def isValid(x : Int, y: Int): Boolean =
    (x >= 0 &&  x < cells.length && y >= 0 && y < cells(x).length)
