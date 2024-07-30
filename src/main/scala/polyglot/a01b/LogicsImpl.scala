package polyglot.a01b

import polyglot.{OptionToOptional, Pair}
import util.Sequences.*
import util.Streams.*
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics

import java.util.Random
import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
 /*from solution on bitbucket*/
 private var minesSet: Sequence[Pair[Int, Int]] = Sequence.empty
 private val selected: Sequence[Pair[Int, Int]] = Sequence.empty
 private val gridSize: Int = size
 val random = new Random()

 while (this.minesSet.size != mines)
  this.minesSet = this.minesSet.add(new Pair(random.nextInt(size), random.nextInt(size)))

 def neighbours(x: Int, y: Int): Int =
  var count = 0
  for neighboursX <- x - 1 to x + 1 do
   for neighboursY <- y - 1 to y + 1 do
    if !(neighboursX == x && neighboursY == y) && this.minesSet.contains(new Pair(neighboursX, neighboursY))
    then count = count + 1
  count


 override def hit(x: Int, y: Int): java.util.Optional[Integer] =
  if this.minesSet.contains(new Pair(x,y))
  then OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter
  else
   this.selected.add(new Pair(x,y))
   OptionToOptional(ScalaOptional.Just(neighbours(x,y)))

 override def won:Boolean  = this.selected.size + this.minesSet.size == this.gridSize * this.gridSize
