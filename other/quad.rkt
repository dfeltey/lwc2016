#lang mini-java

class Main{
 public static void main(String[] args){
  System.out.println(new Math().quad(10));
 }
}

class Math {
 public int quad(int num) {
  return this.square(this.square(num));
 }
 public int square(int num){
  return num * num;
 }
}