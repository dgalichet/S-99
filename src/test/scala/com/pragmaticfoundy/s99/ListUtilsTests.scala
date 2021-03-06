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
        "be able to Modified run-length encoding"                                   ! p11^
        "return a list of n specified elements"                                     ! h02^
        "be able to Decode a run-length encoded list"                               ! p12^
        "Run-length encoding of a list (direct solution)."                          ! p13^
        "Duplicate elements of a list"                                              ! p14^
        "Duplicate the elements of a list a given number of times"                  ! p15^
        "Drop every Nth element from a list"                                        ! p16^
        "Split a list into two parts"                                               ! p17^
        "Extract a slice from a list"                                               ! p18^
        "Rotate a list N places to the left"                                        ! p19^
                                                                                    end

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

    def p11 = { List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)) ===
        encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) }

    def p12 = {
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) ===
            decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) }

    def p13 = {
        List((1, 'a), (1, 'b), (1, 'c)) === encodeDirect(List('a, 'b, 'c)) and
        List((2, 'a), (1, 'b), (1, 'c)) === encodeDirect(List('a, 'a, 'b, 'c)) and
        List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)) ===
            encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    }

    def p14 = {
        List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd) === duplicate(List('a, 'b, 'c, 'c, 'd))
    }

    def p15 = {
        List('a, 'a, 'a) === duplicateN(3, List('a)) and
        List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd) === duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    }

    def p16 = {
        List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k) === drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

    def p17 = {
        (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
            split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

    def p18 = {
        List('d, 'e, 'f, 'g) === slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

    def p19 = {
        List('b, 'c, 'a) === rotate(1, List('a, 'b, 'c)) and
        List('d, 'a, 'b, 'c) === rotate(3, List('a, 'b, 'c, 'd)) and
        List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c) ===
            rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) and
        List('d, 'a, 'b, 'c) === rotate(-1, List('a, 'b, 'c, 'd)) and
        List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) ===
            rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    }

    def h01 = {
        (List('a), List('b, 'c)) === packFirsts(List('a, 'b, 'c)) and
        (List('a, 'a), List('b, 'c)) === packFirsts(List('a, 'a, 'b, 'c))
    }

    def h02 = {
        Nil === mult(0, 'a) and
        List('a) === mult (1, 'a) and
        List('a, 'a, 'a, 'a) === mult(4, 'a)
    }
}