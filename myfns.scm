;cse655lab3    11/27/2011
;Yuxiang Kou
;kou.29@osu.edu

;The documents about this program is at the end of this file.
;The main function defined is the last one in the file,

;Problem: The assignment is to write a function, eliminateNsort, which will receive two lists of numbers as arguments. Suppose L1, L2 are the two lists. The result returned by the function should be obtained from L1, L2 as follows: start with L1 and remove from it all those elements that are also in L2; then sort the remaining elements into non-decreasing order to get the result list.

;The main function's name is "eliminateNsort", the first parameter is list1, second parameter is list2
;The output is a new list in which only contains list1's atoms and these atoms are not contained in list2.

(define (mysort lis)  ;sort the list, so that the sorted list is in a non-decreasing order
  (cond
    ((null? lis) '())
    (else (cons (mysmallest lis (car lis))  ;construct the list with the smallest atom and the rest of the list
                (mysort (myremove lis (mysmallest lis (car lis))))))))  ;the rest of the list is the list without the smallest atom

(define (mysmallest lis ato) ;find the smallest atom in a list
  (cond                      ;the first parameter is a list, the second parameter is current smallest atom
    ((null? lis) ato)
    ((> (car lis) ato) (mysmallest (cdr lis) ato))
    (else (mysmallest (cdr lis) (car lis)))))

(define (myremove lis ato) ;remove atom from a list
  (cond
    ((null? lis) '())
    ((= (car lis) ato) (cdr lis))
    (else (cons (car lis) (myremove (cdr lis) ato)))))

(define (myfindnot ato lis)  ;judge if atom is in the list
  (cond
    ((null? lis) #t)         ;atom is not in the list, return true
    ((= ato (car lis)) #f)   ;atom is in the list, return false
    (else (myfindnot ato (cdr lis)))))

(define (mynewlist lis1 lis2)  ;input list 1 and list 2, out put a new list
  (cond                        ;the output list contais the atoms that only exit in list1 but not exist in list2
    ((null? lis1) '())
    ((myfindnot (car lis1) lis2) (cons (car lis1)
                                       (mynewlist (cdr lis1) lis2)))
    (else (mynewlist (cdr lis1) lis2))))

(define (eliminateNsort lis1 lis2)
  (mysort (mynewlist lis1 lis2)))    ;first construct a new list, then sort it

;
;
;
;Description
;
;The main function is "eliminateNsort", which uses two steps to achieve the goal: first, use "mynewlist" function to create a new list that contains the atoms which only occurs in list1 but not occurs in list2; second, use "mysort" function to sort the new created list, make it non-decreasing.
;The "mynewlist" function compare each atom in list1 with each atom in list2, then only retains the atoms in list1 but not in list2. In this function, "myfindnot" function is used to judge if a atom in list1 is also in list2.
;The "myfindnot" function is to judge if a atom is in a list.
;The "mysort" function is to sort a list into non-decreasing. First, find the smallest atom in list by using "my smallest" function. Then construct a list with this atom and the rest of the list by using "myremove" function which is to remove the smallest atom from the list.
;The "mysmallest" function is to find the smallest atom in a list by using recursion.
;The "myremove" function is to remove a atom from a list by using recursion.
;
;
;User's Guide
;
;This program is used to sort a list into non-decreasing order and remove the elements both in this list and another given list. For example, given lists (0 6 4 2 5 3 1 8 9) and (1 3 5), the program will return (0 2 4 6 8 9).
;The input of this program are two lists: the first parameter is list1, which is to be sorted, the second parameter is list2, for each atom in list1, if it is contained in list2, it will not be contained in the output list. The output of this program is a list, which contains the atoms in list1 but not in list2, and the output list is in non-decreasing order.
;The input lists should be simple lists, for example, (0 1 2 5 3) is a valid input list, and () is also valid, but (0 (3 2) 1) is invalid.
;In order to use this program, just type " (eliminateNsort L1 L2)", for example: (eliminateNsort '(1 5 3 7 2) '(0 8 3))
;
;
;Test Plan
;
;These test cases have been used for testing:
;(eliminateNsort '(9 8 7 6 5 6 4 2 3 1 3 6 7 8 5 9) '(0 1 2 3))
;(eliminateNsort '() '())
;(eliminateNsort '() '(1 3 5))
;(eliminateNsort '(1 2 3 8 6 5 4 2) '())
;(eliminateNsort '(0 8 4 5 7 9) '(0))
;(eliminateNsort '(1) '(3))
;(eliminateNsort '(1) '(1))
;The program have passed all these test cases.
;
;If you have any question about this program, please contact kou.29@osu.edu
