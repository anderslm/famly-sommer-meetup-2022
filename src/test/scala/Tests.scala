/**
 * The universe of the Game of Life is an infinite, two-dimensional orthogonal grid of square cells, each of which is in one of two possible states, live or dead (or populated and unpopulated, respectively). 
 * Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent. At each step in time, the following transitions occur:
 * 
 *     Any live cell with fewer than two live neighbours dies, as if by underpopulation.
 *     Any live cell with two or three live neighbours lives on to the next generation.
 *     Any live cell with more than three live neighbours dies, as if by overpopulation.
 *     Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
 * 
 * These rules, which compare the behavior of the automaton to real life, can be condensed into the following:
 * 
 *     Any live cell with two or three live neighbours survives.
 *     Any dead cell with three live neighbours becomes a live cell.
 *     All other live cells die in the next generation. Similarly, all other dead cells stay dead.
 * 
 * The initial pattern constitutes the seed of the system. The first generation is created by applying the above rules simultaneously to every cell in the seed, live or dead; births and deaths occur simultaneously, 
 * and the discrete moment at which this happens is sometimes called a tick. Each generation is a pure function of the preceding one. The rules continue to be applied repeatedly to create further generations.  
 */

import scala.util.chaining._
import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class GameOfLifeSpec extends AnyFlatSpec with should.Matchers {
  it should "start with a default pattern" in {
    val pattern = GameOfLife.start()

    pattern.size should be(5)
  }

  it should "start with a pattern" in {
    val pattern = Set(Cell(1, 2))

    GameOfLife.start(pattern) should be(pattern)
  }

  "a cell" should "have coordinates" in {
    val cell = GameOfLife.start().head

    cell.x shouldBe a[Int]
  }

  "a cell with no neighbours" should "die by underpopulation" in {
    val pattern = Set(Cell(1, 2))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) shouldBe empty
  }

  "a cell with 1 neighbour" should "die by underpopulation" in {
    val pattern = Set(Cell(1, 2), Cell(1, 3))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) shouldBe empty
  }

  "a cell with 2 neighbours" should "survive to the next generation" in {
    val pattern = Set(Cell(2, 1), Cell(2, 2), Cell(2, 3))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should contain(Cell(2, 2))
  }

  "a cell with 2 adjacent neighbours" should "survive to the next generation" in {
    val pattern = Set(Cell(1, 1), Cell(2, 2), Cell(3, 3))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should contain(Cell(2, 2))
  }

  "a cell with 2 adjacent neighbours alternative" should "survive to the next generation" in {
    val pattern = Set(Cell(1, 3), Cell(2, 2), Cell(3, 1))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should contain(Cell(2, 2))
  }

  "a cell with 3 neighbours" should "survive to the next generation" in {
    val pattern = Set(Cell(1, 1), Cell(1, 2), Cell(1, 3), Cell(2, 2))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should contain(Cell(1, 2))
  }

  "a cell with 4 neighbours" should "die by overpopulation" in {
    val pattern = Set(Cell(0, 2), Cell(1, 1), Cell(1, 2), Cell(1, 3), Cell(2, 2))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should not contain(Cell(1, 2))
  }

  "a cell with 4 adjacent neighbours" should "die by overpopulation" in {
    val pattern = Set(Cell(1, 1), Cell(1, 3), Cell(2, 2), Cell(3, 3), Cell(3, 1))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should not contain(Cell(2, 2))
  }

  "a dead cell with 3  neighbours" should "spawn as by reproduction" in {
    val pattern = Set(Cell(1, 1), Cell(1, 3), Cell(3, 3))

    pattern
      .pipe(GameOfLife.start)
      .pipe(GameOfLife.tick) should contain(Cell(2, 2))
  }
}
