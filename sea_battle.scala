import scala.annotation.tailrec
import scala.io.StdIn
object Main {
  def main(args: Array[String]) = {
    type Point = (Int, Int)
    type Field = Vector[Vector[Boolean]]
    type Ship = List[Point]
    type Fleet = Map[String, Ship]
    type Game = (Field, Fleet)
    val field : Field = Vector(Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false),
      Vector(false, false, false, false, false, false, false, false, false, false))

    def validateShip(ship: Ship): Boolean = {
      if (ship.size == 1) true
      else {
        val a = ship.size <= 4
        val (x, y) = ship(0)
        val b = ship.filter((q, w) => q != x).size == 0
        val c = ship.filter((q, w) => w != y).size == 0
        a & (b | c)
      }
    }

    def neighbours(point: Point): List[Point] = {
      val x = point._1
      val y  = point._2
      List((x - 1, y -1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)).filter
        (q => (q._1 >=0 & q._2 >= 0 & q._1 <= 9 & q._2 <= 9))
    }

    def validatePosition(ship: Ship, field: Field): Boolean = {
      val a = ship.filter((x, y) => field(x)(y)).isEmpty
      val b = ship.filter(q => !neighbours(q).filter(w => field(w._1)(w._2)).isEmpty).isEmpty
      val c = ship.filter((x, y) => (x >= 0 & x <= 9 & y >= 0 & y <= 9)).size == ship.size
      a & b & c
    }

    def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
      fleet + (name -> ship)
    }

    @tailrec
    def markUsedCells(field: Field, ship: Ship): Field = {
      if (ship.isEmpty) field
      else {
        val x = ship(0)._1
        val y = ship(0)._2
        markUsedCells(field.updated(x, field(x).updated(y, true)), ship.drop(1))
      }
    }

    def tryAddShip(game: Game, name: String, ship: Ship): Game = {
      if (validateShip(ship) & validatePosition(ship, game._1)) {
        println(name)
        (markUsedCells(game._1, ship), enrichFleet(game._2, name, ship))
      }
      else game
    }

    def input(list: List[(Int, Int)], n: Int, curr: Int = 0):List[(Int, Int)] = {
      if (curr == n) list
      else {
        val s = StdIn.readLine().split(' ')
        (s(0).toInt - 1, s(1).toInt - 1) +: input(list, n, curr + 1)
      }
    }

    def read_ships(g: Game, count:Int, curr: Int): Game = {
      if (count == curr) return g
      else {
        val name_n = StdIn.readLine().split(" ")
        val name = name_n(0)
        val n = name_n(1).toInt
        val coord = input(List(), n)
        val q = tryAddShip(g, name, coord)
        return read_ships(q, count, curr + 1)
      }
    }

    val count = StdIn.readInt()
    val l = List.range(0, count)
    val x = read_ships((field, Map()), count, 0)
  }
}
