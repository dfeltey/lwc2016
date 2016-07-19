#lang mini-java

class Main {
 public static void main(String [] args) {
  System.out.println((new Runner()).run(10));
 }
}

class Runner {
 public int run(int n) {
  Parity check;                        
  int current;
  check = new Parity();
  current = 0;
  while (current < n) {
   if (check.is_even(current)) {
    System.out.println(current);
   }
   else {}
   current = current + 1;
  }
  return 0;
 }
}

class Parity {
 public boolean is_even(int n){
  return (n == 0) || this.is_odd(n - 1);
 }
 public boolean is_odd(int n) {
  return (! (n == 0)) && this.is_even(n - 1);
 }
}