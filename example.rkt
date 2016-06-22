#lang s-exp "mini-java.rkt"

;; example from the MiniJava Reference

(class Factorial {
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
   ;; (public int Bad(){
   ;;      return (super Bad())
   ;;      })
})

(class Fac2 extends Fac {
   (public int ComputeFac(int num){
        return (super ComputeFac(num))
        })
   (public int Unrelated(){
        return (super ComputeFac(0))
        })
   ;; (public int Bad(){
   ;;      return (super Bad())
   ;;      })
})
