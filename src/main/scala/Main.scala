object Main extends App {
  println("Hello, World!")
}

case class Cell(x: Int, y: Int)

object GameOfLife {
  type Pattern = Set[Cell]

  def start(pattern: Pattern = Set(Cell(0, 0), Cell(0, 1), Cell(0, 2), Cell(0, 3))) = pattern

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