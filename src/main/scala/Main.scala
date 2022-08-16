object Main extends App {
  def printGame(pattern: List[Cell]) = {  
    var y = pattern.sortBy(_.y).head.y
    var x = pattern.sortBy(_.x).head.x

    pattern.groupBy(_.y).toSeq.sortBy(_._1).foreach { case (i, line) => 
      val sorted = line.sortBy(_.x)
      y to i-1 foreach { _ => println() }
      sorted.foreach(cell =>  {
        x to cell.x-1 foreach { _ => print(" ") }
        print("#")
        x = cell.x
      })
      y = i
    }
  }

  def run(pattern: GameOfLife.Pattern): Unit = {
    print("\u001b[2J")
    printGame(pattern.toList)
    
    GameOfLife.tick(pattern) match {
      case newPattern => 
        Thread.sleep(1000)
        run(newPattern)
    }
  }

  run(GameOfLife.start())
}

case class Cell(x: Int, y: Int)

object GameOfLife {
  type Pattern = Set[Cell]

  val blinker = Set(Cell(20, 10), Cell(21, 10), Cell(22, 10))
  val glider = Set(Cell(0, 3), Cell(1, 1), Cell(1, 2), Cell(2, 2), Cell(2, 3))

  def start(pattern: Pattern = glider) = pattern

  def tick(pattern: Pattern) = 
    pattern.filter(cell => neighbours(pattern, cell) match {
      case 2 => true
      case 3 => true
      case _ => false
    }) ++ neighbours(pattern).filter(cell => neighbours(pattern, cell) match {
      case 3 => true
      case _ => false
    })

  def neighbours(pattern: Pattern, cell: Cell) = pattern.-(cell).filter(neighbour(cell, _)).size

  def neighbours(pattern: Pattern) = pattern.flatMap(cell => Set(Cell(cell.x, cell.y + 1), Cell(cell.x, cell.y - 1), Cell(cell.x + 1, cell.y), Cell(cell.x - 1, cell.y), Cell(cell.x + 1, cell.y + 1), Cell(cell.x - 1, cell.y - 1), Cell(cell.x + 1, cell.y - 1), Cell(cell.x - 1, cell.y + 1)))

  def neighbour(cell: Cell, neighbour: Cell) =  {
    val xDifference = (cell.x - neighbour.x).abs
    val yDifference = (cell.y - neighbour.y).abs
    
    (xDifference == 1 || xDifference == 0) && (yDifference == 1 || yDifference == 0) 
  }
}