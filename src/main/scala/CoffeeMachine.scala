import scala.annotation.tailrec
import io.StdIn.{readInt, readLine}

@main
def main(): Unit =
  // Start the program with an initial coffee state
  run(CoffeeMachineState(400, 540, 120, 9, 550))

/**
 * Function that is recursively called to get user action
 *
 * @param currentMachineState state of the coffee machine before user action
 */
@tailrec
def run(currentMachineState: CoffeeMachineState): Unit =
  val action: Option[String] = getUserAction

  val nextMachineState: CoffeeMachineState = action match {
    case Some("buy") => buyCoffee(currentMachineState)
    case Some("fill") => fillCoffeeMachine(currentMachineState)
    case Some("take") => takeMoneyFromCoffeeMachine(currentMachineState)
    case Some("remaining") => currentMachineState.printState(); currentMachineState
    case Some("exit") => exit(); currentMachineState
    case _ => currentMachineState
  }

  run(nextMachineState)

/**
 * Case class to capture the state of the coffee machine
 *
 * @param waterMilliliters milliliters of water
 * @param milkMilliliters milliliters of milk
 * @param coffeeBeanGrams grams of coffee beans
 * @param cups number of cups
 * @param dollars number of dollars
 */
case class CoffeeMachineState(waterMilliliters: Int,
                              milkMilliliters: Int,
                              coffeeBeanGrams: Int,
                              cups: Int,
                              dollars: Int) {
  /**
   * Print the current state of this coffee machine
   */
  def printState(): Unit = {
    println {
      s"""|The coffee machine has:
          |${this.waterMilliliters} of water
          |${this.milkMilliliters} of milk
          |${this.coffeeBeanGrams} of coffee beans
          |${this.cups} disposable cups
          |$$${this.dollars} of money
          |""".stripMargin
    }
  }

  /**
   * Method to determine if a coffee purchase (negative contribution) is possible
   *
   * @param contrib contribution to apply to the coffee machine state
   * @return true if possible, false if not
   */
  def contribIsPossible(contrib: CoffeeMachineState): Boolean =
    val enoughWater = this.waterMilliliters >= -contrib.waterMilliliters
    val enoughMilk = this.milkMilliliters >= -contrib.milkMilliliters
    val enoughCoffeeBeans = this.coffeeBeanGrams >= -contrib.coffeeBeanGrams
    val enoughCups = this.cups >= -contrib.cups

    val sorry: String => String = s => s"Sorry, not enough $s!\n"

    if (!enoughWater) println(sorry("water"))
    else if (!enoughMilk) println(sorry("milk"))
    else if (!enoughCoffeeBeans) println(sorry("coffee beans"))
    else if (!enoughCups) println(sorry("cups"))
    else println("I have enough resources, making you a coffee!\n")

    enoughWater && enoughMilk && enoughCoffeeBeans && enoughCups
}

/**
 * Get user action as instruction for coffee machine
 *
 * @return action for machine
 */
def getUserAction: Option[String] =
  val actions: Seq[String] = Seq("buy", "fill", "take", "remaining", "exit") // Seq to preserver order

  print(s"Write action (${actions.mkString(", ")}):\n> ")
  val action = readLine()
  println()

  Some(action).filter(a => actions.contains(a))

/**
 * User action to buy coffee
 *
 * @param state state of the coffee machine before buying
 * @return state of the coffee machine after buying
 */
def buyCoffee(state: CoffeeMachineState): CoffeeMachineState =
  val espressoContrib = CoffeeMachineState(-250, 0, -16, -1, 4)
  val latteContrib = CoffeeMachineState(-350, -75, -20, -1, 7)
  val cappuccinoContrib = CoffeeMachineState(-200, -100, -12, -1, 6)

  print("What do you want to buy? 1 - espresso, 2 - latte, 3 - cappuccino, back - to main menu:\n> ")
  val coffeeChoice: String = readLine()

  val contrib = coffeeChoice match {
    case "1" => espressoContrib
    case "2" => latteContrib
    case "3" => cappuccinoContrib
    case "back" => CoffeeMachineState(0, 0, 0, 0, 0)
    case _ => CoffeeMachineState(0, 0, 0, 0, 0)
  }

  if (state.contribIsPossible(contrib))
    applyContrib(state, contrib)
  else
    state

/**
 * User action to fill the coffee machine
 *
 * @param state state of the coffee machine before filling
 * @return state of the coffee machine after filling
 */
def fillCoffeeMachine(state: CoffeeMachineState): CoffeeMachineState =
  val howMany: String => String = s => s"Write how many $s you to add:\n> "
  val getIngredientAmount: String => Int = s => {
    print(howMany(s))
    readInt()
  }
  val map: Map[String, Int] = Seq("ml of water", "ml of milk", "grams of coffee beans", "disposable cups")
    .map(x => x -> getIngredientAmount(x))
    .toMap
  println()

  val contrib = CoffeeMachineState(
    map("ml of water"), map("ml of milk"), map("grams of coffee beans"), map("disposable cups"), 0
  )

  applyContrib(state, contrib)


/**
 * User action to take all the money from the coffee machine
 *
 * @param state state of the coffee machine before filling
 * @return state of the coffee machine after filling
 */
def takeMoneyFromCoffeeMachine(state: CoffeeMachineState): CoffeeMachineState =
  println(s"I gave you $$${state.dollars}\n")

  state.copy(dollars = 0)

/**
 * Apply a contribution (+/-) to the coffee machine state
 *
 * @param state state of the coffee machine before contribution
 * @param contrib contribution to apply to the coffee machine state
 * @return state of the coffee machine after contribution
 */
def applyContrib(state: CoffeeMachineState,
                 contrib: CoffeeMachineState): CoffeeMachineState =
    state.copy(waterMilliliters = state.waterMilliliters + contrib.waterMilliliters,
      milkMilliliters = state.milkMilliliters + contrib.milkMilliliters,
      coffeeBeanGrams = state.coffeeBeanGrams + contrib.coffeeBeanGrams,
      cups = state.cups + contrib.cups,
      dollars = state.dollars + contrib.dollars)

/**
 * Exit the program
 */
def exit(): Unit = System.exit(0)