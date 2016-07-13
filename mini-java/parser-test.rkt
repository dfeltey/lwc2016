#lang racket

(require rackunit
         racket
         "lexer-sig.rkt"
         "parser-sig.rkt"
         "lexer-unit.rkt"
         "parser-unit.rkt")

(define-compound-unit/infer lexer+parser@
  (import)
  (export lexer^ parser^)
  (link lexer@ parser@))
  
(define-values/invoke-unit/infer lexer+parser@)

(define (output input-string)
  (map syntax->datum (parse-program (open-input-string input-string) 'program)))


(check-equal? (output "class Test{
                         public static void main(String[] a){
                           System.out.println(1);
                          }
                       }")
              '((class Test (public static void main (String () a) ((System.out.println (1))))))
              "Single Main Class")

(check-equal? (output "class Test{
                         public static void main(String[] a){
                           System.out.println(new Helper().M(5));
                         }
                       }
                       class Helper{
                         boolean t;
                         public int M(int n) {
                           int num;
                           if(false)
                             num = 0;
                           else
                           num = 1;
                           return 1;
                         }
                       }")
              '((class Test
                  (public static void main
                          (String () a)
                          ((System.out.println (((new Helper ()) M (5)))))))
                (class Helper
                  ((boolean t)
                   (public int M (int n)
                           ((int num)
                            (if (false)
                                (num = 0)
                                else
                                (num = 1))
                            return 1)))))
              "if else")

(check-equal? (output "class Test{
                         public static void main(String[] a){
                           System.out.println(new Helper().M(5));
                         }
                       }
                       class Helper{
                       boolean t;
                       public int M(int n) {
                         int num;
                         int ret;
                         t = true;
                         if (t) num = 0; else num = 1;
                         if (num < n) ret = n; else ret = num;
                         return ret;
                       }
                       }")
              '((class Test
                  (public static void main
                          (String () a)
                          ((System.out.println (((new Helper ()) M (5)))))))
                (class Helper
                  ((boolean t)
                   (public int M (int n)
                           ((int num)
                            (int ret)
                            (t = true)
                            (if (t)
                                (num = 0)
                                else
                                (num = 1))
                            (if ((num < n))
                                (ret = n)
                                else
                                (ret = num))
                            return ret)))))
              "complex if else")

(check-equal? (output "class Test{
                         public static void main(String[] a){
                           System.out.println(new Helper().M(5));
                         }
                       }
                       class Helper{
                       boolean t;
                       public int M(int n) {
                         int num;
                         t = true;
                         if (!(!t) && true) num = 0; else num = 1;
                         return num;
                       }
                       }")
              '((class Test
                  (public static void main
                          (String () a)
                          ((System.out.println (((new Helper ()) M (5)))))))
                (class Helper
                  ((boolean t)
                   (public int M (int n)
                           ((int num)
                            (t = true)
                            (if (((! (! t)) && true))
                                (num = 0)
                             else
                                (num = 1))
                            return num)))))
              "bang and && expressions")

(check-equal? (output "class Test{
                         public static void main(String[] a){
                           System.out.println(new Helper().M(5));
                         }
                       }
                       class Helper{
                       boolean t;
                       public int M(int n) {
                       while (0 < n) {
                         System.out.println(n);
                         n = n - 1;
                       }
                       return 0;
                       }
                       }")
              '((class Test
                  (public static void main
                          (String () a)
                          ((System.out.println (((new Helper ()) M (5)))))))
                (class Helper
                  ((boolean t)
                   (public int M (int n)
                           ((while ((0 < n))
                                  ((System.out.println (n))
                                   (n = (n - 1))))
                            return 0)))))
              "while test")

(check-equal? (output "class Test{
                         public static void main(String[] a){
                           System.out.println(new Helper().M(5));
                         }
                       }
                       class Helper{
                         public int M(int n) {
                         int[] arr;
                         arr = new int[10];
                         arr[0] = 45;
                         return arr.length;
                       }
                                }")
              '((class Test
                  (public static void main
                          (String () a)
                          ((System.out.println (((new Helper ()) M (5)))))))
                (class Helper
                   ((public int M (int n)
                           ((int () arr)
                            (arr = (new int (10)))
                            (arr (0) = 45)
                            return (arr length))))))
              "array test")

(check-equal? (output "
class Factorial{
           public static void main(String[] a){
               System.out.println(new Fac2().ComputeFac(10));
           }
}
class Fac{
      public int ComputeFac(int num) {
                int num_aux;
                if(num < 1)
                  num_aux = 1;
                else
                  num_aux = num * this.ComputeFac(num - 1);
                return num_aux;
       }
}

class Fac2 extends Fac{
      public int ComputeFac(int num) {
             return super.ComputeFac(num);
       }
      public int Unrelated() {
             return super.ComputeFac(0);
                    }
}")
              '((class Factorial {
                                  public static void main(String[] a) {
                                                                       (System.out.println(((new Fac2()) ComputeFac(10))))
                                                                       }
                                         })
                
                (class Fac {
                            (public int ComputeFac(int num){
                                                            (int num_aux)
                                                            (if ((num < 1))
                                                                (num_aux = 1)
                                                                else
                                                                (num_aux = (num * (this ComputeFac((num - 1))))))
                                                            return num_aux
                                                            })
                            })
                
                (class Fac2 extends Fac {
                                         (public int ComputeFac(int num){
                                                                         return (super ComputeFac(num))
                                                                                })
                                         (public int Unrelated(){
                                                                 return (super ComputeFac(0))
                                                                        })
                                         }))
              "super extends test")