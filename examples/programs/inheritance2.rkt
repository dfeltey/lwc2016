#lang mini-java

class Main {
 public static void main(String[] args) {
  System.out.println(new Sub().sub());
 }
}

class Sup {
 public int sup_1(int n) {
  System.out.println(1);
  return n;
 }
 public int sup_2(int m) {
  System.out.println(2);
  return m;
 }
}

class Sub extends Sup {
 public int sup_2(int m) {
  System.out.println(3);
  return m;
 }
 public int sub() {
  System.out.println(this.sup_1(4));
  System.out.println(this.sup_2(5));
  return 0;
 }
}