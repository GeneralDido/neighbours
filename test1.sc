import scala.io.Source
import scala.collection.mutable.ListBuffer // https://alvinalexander.com/scala/how-add-elements-to-a-list-in-scala-listbuffer-immutable
import scala.language.postfixOps

val filename = "/Users/dimitris/Documents/Neighbours/test.txt"

def compare_and_filter(arr: Array[Int], value: Char) =
  if (value.asDigit < 0) arr else arr.filter(_ == value.asDigit)

def find_puzzles(): List[List[String]] = {
  // finds puzzles from text file according to total number of puzzles and inserts them to a List of Lists
  val lines = Source.fromFile(filename).getLines.toList
  val puzzle_num = lines(0).split(" ")(1).toInt
  var counter = 1
  var puzzle_size = 0
  var line_size = 0
  var current_line = 1
  var puzzle = new ListBuffer[List[String]]()

  while (counter <= puzzle_num) {
    puzzle_size = lines(current_line).split(" ")(1)(0).asDigit
    line_size = 2 * puzzle_size
    puzzle += lines.slice(current_line + 1, current_line + line_size)
    current_line += line_size
    counter += 1
  }
  // size of each puzzle in lines = 2n - 1, n: size, example: for 4x4 puzzle: n = 4 and size = 2n-1 = 7
  puzzle.toList
}

val p = find_puzzles()(0)
val n = (p.size + 1) / 2
val nums = 0 to 4*(n-1) by 4
val symbol = 2 to 4*(n-1)-2 by 4
val rows_with_values = 0 to p.size-1 by 2
val all_rows = 0 to p.size-1

val puzzle = rows_with_values.map { i => nums.map { j => p(i)(j) }}

var neighbour_arr = Array.fill(n, n, 4)( 0 )
// size = n, neighbour_arr[i][j][L,R,U,D], L:Left, R: Right, U: Up, D: Down, values: Boolean

all_rows.map {
  i => {
    if (i % 2 == 0) {
      // if row is even:  neighbour_arr(i/2)((j+2)/4) [Left] <-  p(i)(j) -> neighbour_arr(i/2)((j-2)/4) [Right]
      symbol.map {
        j => {
          if (p(i)(j) == ' ') {
            neighbour_arr(i/2)((j+2)/4)(0) = 0
            neighbour_arr(i/2)((j-2)/4)(1) = 0
          }
          else {
            neighbour_arr(i/2)((j+2)/4)(0) = 1
            neighbour_arr(i/2)((j-2)/4)(1) = 1
          }
        }
      }
    }
    else {
      // if row is odd: neighbour_arr((i+1)/2)(j/4) [Up] <- p(i)(j) -> neighbour_arr((i-1)/2)(j/4) [Down]
      nums.map {
        j => {
          if (p(i)(j) == ' ') {
            neighbour_arr((i+1)/2)(j/4)(2) = 0
            neighbour_arr((i-1)/2)(j/4)(3) = 0
          }
          else {
            neighbour_arr((i+1)/2)(j/4)(2) = 1
            neighbour_arr((i-1)/2)(j/4)(3) = 1
          }
        }
      }
    }
  }
}

neighbour_arr

var expected_values = Array.fill(n, n)(1 to n toArray)

// initialize array according to puzzle
for (i <- 0 to n-1)
  for (j <- 0 to n-1)
    expected_values(i)(j) = compare_and_filter(expected_values(i)(j), puzzle(i)(j))
expected_values

// expected_values(i)(j) = expected_values(0)(0).filter(_ != k), k in [1..n]
// expected_values.transpose  ==> you can access columns as rows
