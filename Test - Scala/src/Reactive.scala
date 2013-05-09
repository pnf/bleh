import scala.collection.mutable.WeakHashMap


abstract class Reactive {
	var dirty = false
	val parents = new WeakHashMap[Reactive,Integer]()
	def sully(indent : Integer = 1) {
	  if(dirty) {return}
	  println("*"*indent + "sullying " + this)
	  dirty = true;
	  parents.map { case (p,_) => if(!p.dirty) p.sully(indent+1) }
	}
	def addparent(p : Reactive) {
	  parents.put(p,1)
	  p.sully()
	}
}

abstract class RD extends Reactive { //reactive double
	var v = 0.0
	def eval() : Double
	def value() = {
		println("old value of " + this + " was " + v)
		if(dirty) {
			v = eval()
		    dirty = false;
		}
		println("new value of " + this + " is " + v)
		v
	}
	def +(rhs : RD) = new RDAdder(this,rhs)
	def *(rhs : RD) = new RDMultiplier(this,rhs)
}

case class RDConst(vv : Double) extends RD {
	v = vv
	def eval = vv
}

case class RDVal() extends RD {
	override def toString = "RDVal(" + v + ")"  // necessary because not a case class
	def set(u : Double)  = {
		v = u;
		sully()
		this
	}
	def eval() : Double = v
}

case class RDMultiplier(lhs : RD, rhs : RD) extends RD {
	lhs.addparent(this)
	rhs.addparent(this)
  def eval = {
		v = lhs.value * rhs.value
		v
	}
}
case class RDAdder(lhs : RD, rhs : RD) extends RD {
	lhs.addparent(this)
	rhs.addparent(this)
	def eval = {
		v = lhs.value + rhs.value
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
  println(foo);  println(foo.value);
  var x = RDVal().set(3.14);
  foo = R ~ 1.0 + x * 2.0;
  println(foo); println(foo.value);
  x.set(2.71828);
  println(foo); println(foo.value);
}
