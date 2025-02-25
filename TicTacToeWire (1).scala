package tictactoe

import ujson.*
import scala.util.{Failure, Success, Try}

import cs214.webapp.wires.*
import cs214.webapp.exceptions.DecodingException
import cs214.webapp.{AppWire, WireFormat, UserId}

object TicTacToeWire extends AppWire[TicTacToeEvent, TicTacToeView]:
  import TicTacToeEvent.*
  import TicTacToeView.*

  override object eventFormat extends WireFormat[TicTacToeEvent]:
    override def encode(t: TicTacToeEvent): Value =
      t match
      case Move(x, y) => Obj("type" -> "move", "x" -> x, "y" -> y)
    override def decode(json: Value): Try[TicTacToeEvent] =
        Try {
      json("type") match
        case Str("move") =>
          Move(json("x").num.toInt, json("y").num.toInt)
        case _ => throw DecodingException("Invalid event type")
      }
  override object viewFormat extends WireFormat[TicTacToeView]:
    def encode(t: TicTacToeView): Value = t match
    case Playing(board, yourTurn) =>
      Obj("type" -> "playing", "board" -> encodeBoard(board), "yourTurn" -> yourTurn)
    case Finished(winner) =>
      Obj("type" -> "finished", "winner" -> encodeWinner(winner))

    def decode(json: Value): Try[TicTacToeView] = Try {
      json("type").str match
      case "playing" =>
        val board = decodeBoard(json("board"))
        val yourTurn = json("yourTurn").bool
        Playing(board, yourTurn)
      case "finished" =>
        val winner = decodeWinner(json("winner"))
        Finished(winner)
      case _ => throw DecodingException("Invalid view type")
  }

  // Helper methods for encoding and decoding
  private def encodeBoard(board: tictactoe.Board): Value =
     ujson.Arr(board.cells.map(row => ujson.Arr(row.map(cell => encodeUserId(cell)): _*)): _*)

  private def decodeBoard(json: Value): tictactoe.Board =
    Board(json.arr.toList.map(row => row.arr.toList.map(cell => decodeUserId(cell))))
  private def encodeWinner(winner: Option[UserId]): Value =
    winner.map(Str(_)).getOrElse(Null)
  private def decodeWinner(json: Value): Option[UserId] =
    json match
      case Str(userId) => Some(userId)
      case _ => None

  private def encodeUserId(userId: Option[UserId]): Value =
    userId.map(userId => Str(userId.toString)).getOrElse(Null)

  private def decodeUserId(json: Value): Option[UserId] =
    json match
      case Str(userId) => Some(userId)
      case _ => None