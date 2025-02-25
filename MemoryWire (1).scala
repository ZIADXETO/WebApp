package memory

import scala.util.{Failure, Success, Try}

import cs214.webapp._
import cs214.webapp.wires._
import cs214.webapp.exceptions.DecodingException

object MemoryWire extends AppWire[MemoryEvent, MemoryView] {
  import MemoryEvent._
  import MemoryView._
  import StateView._
  import CardView._
  import PhaseView._
  import ujson._

  override object eventFormat extends WireFormat[MemoryEvent] {
    override def encode(event: MemoryEvent): Value = event match {
      case Toggle(cardId) => Obj("type" -> "Toggle", "cardId" -> cardId)
      case FlipSelected   => Obj("type" -> "FlipSelected")
    }

    override def decode(js: Value): Try[MemoryEvent] = Try {
      js("type").str match {
        case "Toggle"        => Toggle(js("cardId").num.toInt)
        case "FlipSelected"  => FlipSelected
        case _               => throw DecodingException(s"Unexpected MemoryEvent: $js")
      }
    }
  }

  override object viewFormat extends WireFormat[MemoryView] {
    override def encode(v: MemoryView): Value = Obj(
      "stateView" -> stateViewFormat.encode(v.stateView),
      "alreadyMatched" -> alreadyMatchedFormat.encode(v.alreadyMatched)
    )

    override def decode(js: Value): Try[MemoryView] = Try {
      MemoryView(
        stateView = stateViewFormat.decode(js("stateView")).get,
        alreadyMatched = alreadyMatchedFormat.decode(js("alreadyMatched")).get
      )
    }

    private val stateViewFormat: WireFormat[StateView] = new WireFormat[StateView] {
      override def encode(sv: StateView): Value = sv match {
        case Playing(phase, currentPlayer, board) =>
          Obj(
            "type" -> "Playing",
            "phase" -> {
              phase match
                case SelectingCards => Str("SelectingCards")
                case CardsSelected =>Str("CardsSelected")
                case Waiting => Str("Waiting")
                case GoodMatch => Str("GoodMatch")
                case BadMatch => Str("BadMatch")
            },
            "currentPlayer" -> userIdFormat.encode(currentPlayer),
            "board" -> seqWireFormat(cardViewFormat).encode(board)
          )
        case Finished(winnerIds) =>
          Obj(
            "type" -> "Finished",
            "winnerIds" -> seqWireFormat(userIdFormat).encode(winnerIds.toSeq)
          )
      }

      override def decode(js: Value): Try[StateView] = Try {
        js("type").str match {
          case "Playing" =>
            Playing(
              phase = phaseFormat.decode(js("phase")).getOrElse(throw DecodingException("Missing phase")),
              currentPlayer = userIdFormat.decode(js("currentPlayer")).getOrElse(throw DecodingException("Missing currentPlayer")),
              board = seqWireFormat(cardViewFormat).decode(js("board")).getOrElse(throw DecodingException("Missing board"))
            )
          case "Finished" =>
            Finished(seqWireFormat(userIdFormat).decode(js("winnerIds")).getOrElse(throw DecodingException("Missing winnerIds")).toSet)
          case _ =>
            throw DecodingException(s"Unexpected StateView: $js")
        }
      }
    }

    private val alreadyMatchedFormat: WireFormat[ScoresView] =
      new MapWire(userIdFormat, seqWireFormat(cardFormat)).asInstanceOf[WireFormat[ScoresView]]
  }

  private val userIdFormat: WireFormat[UserId] = StringWire.asInstanceOf[WireFormat[UserId]]
  private val cardFormat: WireFormat[Card] = StringWire.asInstanceOf[WireFormat[Card]]

  private val cardViewFormat: WireFormat[CardView] = new WireFormat[CardView] {
    override def encode(cv: CardView): Value = cv match {
      case FaceDown          => Obj("type" -> "FaceDown")
      case Selected          => Obj("type" -> "Selected")
      case FaceUp(card)      => Obj("type" -> "FaceUp", "card" -> cardFormat.encode(card))
      case AlreadyMatched(card) => Obj("type" -> "AlreadyMatched", "card" -> cardFormat.encode(card))
    }

    override def decode(js: Value): Try[CardView] = Try {
      js("type").str match {
        case "FaceDown"         => FaceDown
        case "Selected"         => Selected
        case "FaceUp"           => FaceUp(cardFormat.decode(js("card")).get)
        case "AlreadyMatched"   => AlreadyMatched(cardFormat.decode(js("card")).get)
        case _                  => throw DecodingException(s"Unexpected CardView: $js")
      }
    }
  }

  private val phaseFormat: WireFormat[PhaseView] = new WireFormat[PhaseView] {
    override def encode(phase: PhaseView): Value = phase match {
      case SelectingCards  => Str("SelectingCards")
      case CardsSelected   => Str("CardsSelected")
      case Waiting          => Str("Waiting")
      case GoodMatch        => Str("GoodMatch")
      case BadMatch         => Str("BadMatch")
    }

    override def decode(js: Value): Try[PhaseView] = Try {
      js.str match {
        case "SelectingCards" => SelectingCards
        case "CardsSelected"  => CardsSelected
        case "Waiting"        => Waiting
        case "GoodMatch"      => GoodMatch
        case "BadMatch"       => BadMatch
        case _                => throw DecodingException(s"Unexpected PhaseView: $js")
      }
    }
  }
  private def seqWireFormat[T](wf: WireFormat[T]): WireFormat[Seq[T]] =
    SeqWire(wf).asInstanceOf[WireFormat[Seq[T]]]
}