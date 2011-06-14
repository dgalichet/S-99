package com.pragmaticfoundy.s99

import org.specs2.Specification
import com.pragmaticfoundry.s99.ListUtils._
import com.pragmaticfoundry.s99.ListUtils

/**
 * @author David Galichet.
 */

class ListUtilsTests extends Specification { def is =

    "List utils must (full requirement in http://aperiodic.net/phil/scala/s-99/"    ^
                                                                                    p^
        "be able to return last element of a list"                                  ! p01^
        "be able to Find the last but one element of a list"                        ! p02^
        "be able to Find the Kth element of a list"                                 ! p03^
        "be able to Find the number of elements of a list"                          ! p04^
        "be able to Reverse a list"                                                 ! p05^
        "be able to Find out whether a list is a palindrome"                        ! p06^
        "be able to Flatten a nested list structure"                                ! p07^
        "be able to Eliminate consecutive duplicates of list elements"              ! p08^
        "be able to Pack consecutive duplicates of list elements into sublists"     ! p09^
        "return a tuple ( List(equal head elements), tail)"                         ! h01^
        "be able to Run-length encoding of a list"                                  ! p10^
                                                                                    end
/*        "be able to Modified run-length encoding"                                   ! p11^
        "be able to Decode a run-length encoded list"                               ! p12^*/

    def p01 = { 8 === last(List(1, 1, 2, 3, 5, 8)) }

    def p02 = { 5 === penultimate(List(1, 1, 2, 3, 5, 8)) }

    def p03 = { 2 === nth(2, List(1, 1, 2, 3, 5, 8)) }

    def p04 = { 6 === ListUtils.length(List(1, 1, 2, 3, 5, 8)) }

    def p05 = { List(8, 5, 3, 2, 1, 1) === reverse(List(1, 1, 2, 3, 5, 8)) }

    def p06 = { isPalindrome(List(1)) and !isPalindrome(List(1, 2)) and isPalindrome(List(1, 2, 3, 2, 1)) }

    def p07 = { List(1, 2, 3) === flatten(List(1, 2, 3)) and
        List(1, 1, 2) === flatten(List(1, List(1, 2))) and
        List(1, 1, 2, 3, 5, 8) === flatten(List(List(1, 1), 2, List(3, List(5, 8)))) }

    def p08 = {
        List() === compress(List()) and List('a) === compress(List('a)) and
        List('a, 'b) === compress(List('a, 'b)) and
        List('a, 'b) === compress(List('a, 'a, 'b)) and
        List('a, 'b, 'c, 'a, 'd, 'e) === compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    }

    def p09 = { List(List('a)) === pack(List('a)) and
        List(List('a), List('b), List('c)) === pack(List('a, 'b, 'c)) and
        List(List('a, 'a), List('b), List('c)) === pack(List('a, 'a, 'b, 'c)) and
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) ===
            pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    }

    def p10 = { List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)) ===
        encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) }

    def p11 = { false /*encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))*/ }

    def p12 = { false /*decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))*/ }

    def h01 = {
        (List('a), List('b, 'c)) === packFirsts(List('a, 'b, 'c)) and
        (List('a, 'a), List('b, 'c)) === packFirsts(List('a, 'a, 'b, 'c))
    }
}