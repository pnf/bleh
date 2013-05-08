
abstract class Reactive {
	var dirty = false
	def sully() {
	  // do something
	}
}

abstract class RD extends Reactive { //reactive double
	def eval() : Double
	def +(rhs : RD) = new RDAdder(this,rhs)
	def *(rhs : RD) = new RDMultiplier(this,rhs)
}

case class RDConst(v : Double) extends RD {
	def eval = v
}

case class RDVal() extends RD {
	var v = 0.0
	override def toString = "RDVal(" + v + ")"  // necessary because not a case class
	def set(u : Double)  = {
		v = u;
		this
	}
	def eval() : Double = v
}

case class RDMultiplier(lhs : RD, rhs : RD) extends RD {
	var v = 0.0
	def eval() = {
		v = lhs.eval() * rhs.eval()
		v
	}
}
case class RDAdder(lhs : RD, rhs : RD) extends RD {
	var v = 0.0
	dirty = true;
	def eval = if (!dirty) v else {
		v = lhs.eval + rhs.eval
		v
	}
}

object Implicits {
	implicit def double2RDConst(x : Double) = new RDConst(x)
}
import Implicits._

object R {
	def ~(x : Double) : RDConst = new RDConst(x); 
}

object Main {
  def main(args : Array[String]) : Unit = {}
  var foo : RD = _
  foo = R ~ 3.0 + 8.0;
  println(foo);  println(foo.eval);
  var x = RDVal().set(3.14);
  foo = R ~ 1.0 + x * 2.0;
  println(foo); println(foo.eval);
  x.set(2.71828);
  println(foo); println(foo.eval);
}
