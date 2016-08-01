#lang mini-java


class Main {
 public static void main(String[] args) {
  System.out.println(new Sub().go());
 }
}

class Sup {
 int a;
 int b;
 public int init(int n, int m) {
  a = n;
  b = m;
  return 0;
 }
 public int print() {
  System.out.println(a);
  System.out.println(b);
  return 0;
 }
}

class Sub extends Sup {
 int c;
 public int go() {
  System.out.println(this.run(3));
  return 42;
 }
 public int run(int m) {
  int temp;
  c = m;
  temp = this.init(c - 2, c - 1);
  temp = this.print();
  System.out.println(c);
  return 0;
 }
}