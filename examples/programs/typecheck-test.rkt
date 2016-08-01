#lang mini-java

class Main {
 public static void main(String[] a) {
     {
      System.out.println((new Baz()).go1());
      System.out.println((new Baz()).go2());
     }
    }
}

class Foo {
  int f;
  public int print() {
    System.out.println(f);
    return 0;
  }
 public int foo(int num){
  int dumb;
  f = num;
  System.out.println(f);
  dumb = this.print();
  return 0;
 }   
}

class Bar {
 Foo my_foo;
 public Foo go() {
  int dumb;
  my_foo = new Foo();
  dumb = my_foo.foo(17);
  return my_foo;
  }
}

class Baz {
 public int go1(){
  Foo f;
  f = (new Bar ()).go();
  return f.print();
 }
 public int go2() {
  return (new Bar ()).go().print ();
 }
}
