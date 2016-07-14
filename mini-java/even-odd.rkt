#lang mini-java

class Main {
  public static void main(String [] args) {
    if ((new Even()).is_even(12)) {
      System.out.println(1);
    }
    else {
      System.out.println(0);
    }
  }
}

class Even {          
 public boolean is_even(int n){
  return (n == 0) || this.is_odd(n - 1);
 }
 public boolean is_odd(int n) {
  return (! (n == 0)) && this.is_even(n - 1); 
 }
}