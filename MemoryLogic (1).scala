package memory

import scala.util.{Try, Random}

import ujson.Value

import cs214.webapp.*
import cs214.webapp.messages.*
import cs214.webapp.exceptions.*
import cs214.webapp.server.WebServer

import memory.*
import scala.util.Success
import scala.util.Failure
import org.http4s.Entity.Empty

// Feel free to tweak this value!
private val SHOW_CARDS_PAUSE_MS = 2500

object MemoryStateMachine extends cs214.webapp.StateMachine[MemoryEvent, MemoryState, MemoryView]:

  val name: String = "memory"
  val wire = MemoryWire

  def Deck(cards: String): Vector[String] =
    cards.trim.split(" +").to(Vector)

  val DECKS: Map[String, Vector[String]] = Map(
    "Simple" -> Deck("""
      💫 ⭐️
    """),
    "Stars" -> Deck("""
      💫 ⭐️ 🌟 ✨ ☀️
    """),
    "Animals" -> Deck("""
      🐵 🐒 🦍 🦧 🐶 🐕 🦮 🐕‍🦺
      🐩 🐺 🦊 🦝 🐱 🐈 🐈‍⬛ 🦁
      🐯 🐅 🐆 🐴 🫎 🫏 🐎 🦄
      🦓 🦌 🦬 🐮 🐂 🐃 🐄 🐷
      🐖 🐗 🐽 🐏 🐑 🐐 🐪 🐫
      🦙 🦒 🐘 🦣 🦏 🦛 🐭 🐁
      🐀 🐹 🐰 🐇 🐿️ 🦫 🦔 🦇
      🐻 🐻‍❄️ 🐨 🐼 🦥 🦦 🦨 🦘
      🦡
    """),
    "Birds" -> Deck("""
      🦃 🐔 🐓 🐣 🐤 🐥 🐦 🐧
      🕊️ 🦅 🦆 🦢 🦉 🦤 🪶 🦩
      🦚 🦜 🪽 🐦‍⬛ 🪿
    """),
    "Marine & Reptiles" -> Deck("""
      🐸 🐊 🐢 🦎 🐍 🐲 🐉 🦕
      🦖 🐳 🐋 🐬 🦭 🐟 🐠 🐡
      🦈 🐙 🐚 🪸 🪼 🦀 🦞 🦐
      🦑 🦪
    """),
    "Bugs" -> Deck("""
      🐌 🦋 🐛 🐜 🐝 🪲 🐞 🦗
      🪳 🕷️ 🕸️ 🦂 🦟 🪰 🪱 🦠
    """),
    "Plants" -> Deck("""
      💐 🌸 💮 🪷 🏵️ 🌹 🥀 🌺
      🌻 🌼 🌷 🪻 🌱 🪴 🌲 🌳
      🌴 🌵 🌾 🌿 ☘️ 🍀 🍁 🍂
      🍃 🍄 🪨 🪵
    """)
  )

  // Use any strings you want here — the tests don't check for these specific emoji
  val CARDS: Vector[String] = DECKS("Birds")

  /** Creates a new application state. */
  override def init(clients: Seq[UserId]): MemoryState =
    val shuffledCards = Random.shuffle(CARDS ++ CARDS)
    MemoryState.Playing(
      clients,
      clients.head,
      Random.shuffle(shuffledCards.map(card => CardView.FaceDown)),
      Nil,
      shuffledCards,
      clients.map( _ -> Seq.empty[memory.Card]).toMap
    )
    

  override def transition(state: MemoryState)(userId: UserId, event: MemoryEvent): Try[Seq[Action[MemoryState]]] =
   if isGameWon(state) then
    Failure(IllegalMoveException("Already over "))
    else
    event match
    case MemoryEvent.Toggle(cardId) =>
      state match
        case MemoryState.Playing(phase, currentPlayer, board, selectedCards , cards,scores)  =>
          if cardId < 0 || cardId >= board.length then
            Failure(IllegalMoveException("Invalid card index "))
          else if userId != currentPlayer then 
             Failure(new NotYourTurnException)
          else if (selectedCards.size < 2) then {
               if selectedCards.size == 0 then
                 Success(Seq(Action.Render(MemoryState.Playing(phase, currentPlayer, board.updated(cardId,CardView.Selected), List(cardId), cards,scores))))
               
               else if selectedCards.size == 1 && cardId == selectedCards.head then 
                 Success(Seq(Action.Render(MemoryState.Playing(phase, currentPlayer, board.updated(cardId,CardView.FaceDown), Nil ,cards,scores))))
               
               else  {
                 Success(Seq(Action.Render(MemoryState.Playing(phase, currentPlayer, board.updated(cardId,CardView.Selected), selectedCards ++ List(cardId), cards,scores))))
              }
             } 
          else 
              Failure(new IllegalMoveException("More than two cards selected "))

    case MemoryEvent.FlipSelected =>
     state match {
      case MemoryState.Playing(players, currentPlayer, board, selectedCards,cards,scores) =>
      if selectedCards.size != 2 then
        Failure(new IllegalArgumentException("You must select two cards before flipping"))
      else 
        if board(selectedCards.head) != CardView.Selected || board(selectedCards.tail.head) != CardView.Selected then 
          Failure(IllegalArgumentException("Invalid CardView type"))
        else
          val first = selectedCards.head
          val second = selectedCards.tail.head
          val matched = areMatching(selectedCards, cards)
          val updateBoard = board.updated(selectedCards.head , CardView.FaceUp(cards(first)))
                            .updated(selectedCards.tail.head , CardView.FaceUp(cards(second)))
          val commonState = MemoryState.Playing(players, currentPlayer,updateBoard, Nil,cards,scores)
          if matched then 
            val updatedBoard = board.updated(selectedCards.head , CardView.AlreadyMatched(cards(first)))
                            .updated(selectedCards.tail.head , CardView.AlreadyMatched(cards(second)))
            val updatedScores = scores.updated(userId, scores(userId) :+ cards(first) :+ cards(second))
            val updatedStateAction = Action.Render(MemoryState.Playing(players, currentPlayer, updatedBoard, Nil, cards,updatedScores))
            Success(Seq(Action.Render(commonState) , updatedStateAction))
          
          else 
            val updateBoard = board.updated(selectedCards.head , CardView.FaceDown)
                            .updated(selectedCards.tail.head , CardView.FaceDown)
            val nextPlayer = getNextPlayer(players, currentPlayer)
            val pauseAction = Action.Pause(SHOW_CARDS_PAUSE_MS)
            val updatedStateAction = Action.Render(MemoryState.Playing(players, nextPlayer, updateBoard, Nil, cards,scores))
            Success(Seq(Action.Render(commonState), pauseAction, updatedStateAction))
      }

  override def project(state: MemoryState)(userId: UserId): MemoryView =
  state match {
    case MemoryState.Playing(players, currentPlayer, board, selectedCards, cards,scores) =>{
      val phaseView = {
        if isGameWon(state) then
          StateView.Finished(findWinners(scores))
       else if userId == currentPlayer then 
          selectedCards.size match 
            case 0 => StateView.Playing(PhaseView.GoodMatch, userId, board)
            case 1 => StateView.Playing(PhaseView.GoodMatch, userId, board)
            case 2 => StateView.Playing(PhaseView.GoodMatch, userId, board)
            case _ => throw new IllegalArgumentException("More than 2 cards selected")
       else 
        StateView.Playing(PhaseView.GoodMatch, currentPlayer, board)
      }
      MemoryView(phaseView, scores)
    }
  }
  

  private def findWinners(scores: ScoresView): Set[UserId] =
    val maxScore = scores.values.map(_.length).max
    scores.collect { case (userId, matchedCards) if matchedCards.length == maxScore => userId }.toSet

  private def isGameWon(state: MemoryState): Boolean =
    state match
      case MemoryState.Playing(players, currentPlayer, board, selectedCards, cards,scores) =>
         board.forall( cardView => cardView match
          case CardView.AlreadyMatched(_) => true
          case _ => false )

  private def toggleCard(board: Seq[CardView], cardId: Int): Seq[CardView] =
    board.updated(cardId, board(cardId) match {
      case CardView.FaceDown => CardView.Selected
      case CardView.Selected => CardView.FaceDown
      case other => other
    })

  private def areMatching(selectedCards: List[Int], cards : Vector[Card]): Boolean =
      cards(selectedCards.head) == cards(selectedCards.tail.head)
  

  private def calculateWinner(scores: Map[UserId, Seq[String]]): Set[UserId] =
   val maxScore = scores.values.max
   scores.filter { case (_, score) => score == maxScore }.keySet
  

  private def getNextPlayer(players: Seq[UserId], currentPlayer: UserId): UserId =
   val currentPlayerIndex = players.indexOf(currentPlayer)
   val nextPlayerIndex = (currentPlayerIndex + 1) % players.length
   players(nextPlayerIndex)

  def isGameOver(board: Seq[CardView]): Boolean =
  board.forall {
    case CardView.FaceUp(_) => true
    case _ => false
  }


// Server registration magic
class register:
  WebServer.register(MemoryStateMachine)