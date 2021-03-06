package com.pragmaticfoundry.s99

import annotation.tailrec

/**
 * @author David Galichet.
 */

object ListUtils {

    // P01
    def last[T](l:List[T]):T = l match {
        case Nil => throw new NoSuchElementException("List is empty")
        case head::Nil => head
        case head::tail => last(tail)
    }

    // P02
    def penultimate[T](l:List[T]):T = l.reverse match {
        case first::second::tail => second
        case _ => l.head
    }

    // P03
    def nth[T](k:Int, l:List[T]):T = if (k == 0) l.head else nth(k - 1, l.tail)

    // P04
    def length[T](l:List[T]):Long = l.foldLeft(0) { case (i, _) => i+1 }

    // P05
    def reverse[T](l:List[T]):List[T] = {
        var reverted:List[T] = Nil
        for (e <- l) reverted ::= e
        reverted
    }

    // P06
    def isPalindrome[T](l:List[T]) = l.reverse == l

    // P07
    def flatten[Any](l:List[_]):List[Any] = l.foldRight(List.empty[Any]) {
        case ( t:List[_], r ) => flatten(t):::r
        case ( t:Any, r ) => t::r
    }

    // P08
    def compress[T](l:List[T]):List[T] = l match {
        case Nil => Nil
        case head::Nil => List(head)
        case head::tail => if (head != tail.head) head::compress(tail) else compress(tail)
    }

    // P09
    def pack[T](l:List[T]):List[List[T]] = packFirsts(l) match {
        case ( Nil, Nil ) => Nil
        case ( h, t ) => h::pack(t)
    }

    // utility for P09
    def packFirsts[T](l:List[T]):(List[T], List[T]) = ( (List.empty[T], List.empty[T]) /: l) {
        case ( ( Nil, _ ), e ) => ( e::Nil, Nil )
        case ( (h, Nil), e ) if h.head == e => ( e::h, Nil )
        case ( (h, t), e) => ( h, t :+ e )
    }

    // P10
    def encode[T](l:List[T]):List[(Int, T)] = pack(l) map { e => (e.size, e.head) }

    // P11
    def encodeModified[T](l:List[T]):List[Any] = encode(l) map {
        case (1, x) => x
        case x => x
    }

    // P12
    def decode[T](l:List[(Int, T)]):List[T] = l flatMap {
        case (m, e) => mult(m, e)
    }

    def mult[T](m:Int, e:T):List[T] = { for (i <- 1 to m) yield e }.toList

    // P13
    def encodeDirect[T](l:List[T]):List[(Int, T)] = l.foldRight(List.empty[List[T]]) {
        case (t, Nil) => List(t)::Nil
        case (t, head::tail) if head.head == t => (t::head)::tail
        case (t, head::tail) => List(t)::head::tail
    }.map(e => (e.size, e.head))

    // P14
    def duplicate[T](l:List[T]):List[T] = l.flatMap(e => List(e, e))

    // P15
    def duplicateN[T](m:Int, l:List[T]):List[T] = l.flatMap(e => mult(m, e))

    // P16
    def drop[T](n:Int, l:List[T]):List[T] = l.foldLeft((1, List.empty[T])) {
        case ((i, nl), _) if i == n => (1, nl)
        case ((i, nl), e) => (i+1, e::nl)
    }._2.reverse

    // p17
    def split[T](n:Int, l:List[T]):(List[T], List[T]) = (l.take(n), l.drop(n))

    // P18
    def slice[T](from:Int, to:Int, l:List[T]):List[T] = l.take(to).drop(from)

    // P19
    @tailrec def rotate[T](n:Int, l:List[T]):List[T] = n match {
        case 0 => l
        case 1 => l.tail:::(l.head::Nil)
        case -1 => l.last::l.take(l.size - 1)
        case i if i > 1 => rotate(i-1, l.tail:::(l.head::Nil))
        case i if i < -1 => rotate(i+1, l.last::l.take(l.size - 1))
    }
}