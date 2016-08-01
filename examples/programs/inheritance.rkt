#lang mini-java

class Main {
 public static void main(String[] a) {
  System.out.println(new Runner().run());
 }
}

class Foo {
 int f;
 public int print() {
  System.out.println(f);
  return 0;
 }
 public int foo(int num) {
  f = num;
  return 0;
 }
}

class Bar extends Foo {
 int b;
 public int init(int n, int m) {
  int dummy;
  dummy = this.foo(n);
  b = m;
  return 0;
 }
 public int go() {                  
  int dumb;
  dumb = this.print();
  System.out.println(b);
  return 0;
 }
}

class Runner {
  Bar bar;
  public int run() {
  int temp;
  bar = new Bar();
  temp = bar.init(5,12);
  return bar.go();
  }
}