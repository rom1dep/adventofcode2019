package aoc19

type Wire = List[String]
type Coords = List[Point]
type ICords = collection.mutable.HashSet[Point]

enum Direction
  case L, R, U, D

case class Point(x: Int, y: Int) with
  inline def distance(that: Point): Int = (that.x - x).abs + (that.y - y).abs

  inline def + (that: Point): Point = Point(x + that.x, y + that.y)

  def coordsTo(d: Direction, steps: Int): Coords = d match
    case Direction.L => (x - steps to x).map(Point(_, y)).reverse.toList
    case Direction.R => (x to x+steps).map(Point(_, y)).toList
    case Direction.U => (y - steps to y).map(Point(x, _)).reverse.toList
    case Direction.D => (y to y+steps).map(Point(x, _)).toList

object D03 extends App with
  val input = io.Source.fromFile("./c3").getLines
  val (w1, w2) = (input.next.split(',').toList, input.next.split(',').toList)
  val origin = Point(0, 0)

  def coords(w:Wire): Coords = w.foldLeft(List(Point(0,0)))(
    (acc:Coords, s: String) => s.head match {
        case 'L' => acc.head.coordsTo(Direction.L, s.tail.toInt).tail.reverse ::: acc
        case 'R' => acc.head.coordsTo(Direction.R, s.tail.toInt).tail.reverse ::: acc
        case 'U' => acc.head.coordsTo(Direction.U, s.tail.toInt).tail.reverse ::: acc
        case 'D' => acc.head.coordsTo(Direction.D, s.tail.toInt).tail.reverse ::: acc
      }
  ).reverse

  def imperativeCoords(w: Wire): collection.mutable.HashSet[Point] = 
    val points = collection.mutable.HashSet[Point](origin)
    var last = origin
    for s <- w
        d = s.head match
          case 'L' => Point(-1 , 0)
          case 'R' => Point(1 , 0)
          case 'U' => Point(0 , -1)
          case 'D' => Point(0, 1)
        o <- 0 until s.tail.toInt
    yield
      last = last + d
      points += last
    points

  def intersects(c1: Coords, c2: Coords) = c1.filter(p1 => p1 != origin && c2.exists(p2 => p1 == p2))

  def intersects(c1: ICords, c2: ICords) = c1.intersect(c2) - origin


  // Solution 1
  //    pure functional style, takes minutes to execute
  //println(intersects(coords(w1), coords(w2)).map(_.distance(Point(0,0))).min)
  //    slightly more imperative style, takes ms to execute
  println(intersects(imperativeCoords(w1), imperativeCoords(w2)).map(_.distance(origin)).min)

  // Solution 2 - same, but with a distance counter
  def imperativeCountingCoords(w: Wire): collection.mutable.HashMap[Point, Int] = 
    val points = collection.mutable.HashMap[Point, Int](origin -> 0)
    var last = origin
    var distance = 0
    for s <- w
        d = s.head match
          case 'L' => Point(-1 , 0)
          case 'R' => Point(1 , 0)
          case 'U' => Point(0 , -1)
          case 'D' => Point(0, 1)
        o <- 0 until s.tail.toInt
    yield
      last = last + d
      distance += 1
      if !points.isDefinedAt(last) then points += last -> distance
    points

  val (c1, c2) = (imperativeCountingCoords(w1), imperativeCountingCoords(w2))
  println(intersects(imperativeCoords(w1), imperativeCoords(w2)).map(p => c1(p) + c2(p)).min)
end D03
