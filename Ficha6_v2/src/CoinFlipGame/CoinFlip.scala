package CoinFlipGame

import CoinFlipGame.CoinFlipUtils._

import scala.annotation.tailrec
import scala.util.Random

case class GameState(numFlips: Int, numCorrect: Int)

object CoinFlip extends App {

    val r = Random
    val s = GameState(0, 0)
    val hist: List[GameState]=List()

    mainLoop(s, r,hist)

    @tailrec
    def mainLoop(gameState: GameState, random: Random, hist:List[GameState]) {

        showPrompt()
        val userInput = getUserInput()

        // handle the result
        userInput match {
            case "H" | "T" => {
                val coinTossResult = tossCoin(random)
                val newNumFlips = gameState.numFlips + 1
                if (userInput == coinTossResult) {
                    val newNumCorrect = gameState.numCorrect + 1
                    val newGameState = gameState.copy(numFlips = newNumFlips, numCorrect = newNumCorrect)
                    printGameState(printableFlipResult(coinTossResult), newGameState)
                    val newhist=hist
                    mainLoop(newGameState, random, newhist)
                } else {
                    val newGameState = gameState.copy(numFlips = newNumFlips)
                    printGameState(printableFlipResult(coinTossResult), newGameState)
                    val newhist=hist
                    mainLoop(newGameState, random, newhist)
                }
            }

            case "N" | "n" => {
                val h=gameState::hist
                val newgameState= GameState(0, 0)

                mainLoop(newgameState, r, h)
            }

            case _   => {
                val h=gameState::hist
                printGameOver()
                printhist(h)
                // return out of the recursion here
            }
        }
    }

}
