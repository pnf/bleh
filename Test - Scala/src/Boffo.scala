class Foo(x : Double) {
  def bleh(n:Int) : Double = 
		  if(n==0) 1.0
		  else if(n<0) new Foo(1.0/x).bleh(-n) 
		  else if(n%2==0) {var y=bleh(n/2);y*y}
		  else x*bleh(n-1);
  		
}


object  Boffo extends Application {
	println("hello")
	var f = new Foo(2.0);
	println(f.bleh(3));
}
