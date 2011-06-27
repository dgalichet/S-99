package com.pragmaticfoundry.s99

import javax.management.remote.rmi._RMIConnection_Stub
import org.specs2.internal.scalaz.ListT

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
    def length[T](l:List[T]):Long = l match {
        case Nil => throw new NoSuchElementException("List is empty")
        case head::Nil => 1
        case head::tail => 1 + length(tail)
    }

    // P05
    def reverse[T](l:List[T]):List[T] = {
        var reverted:List[T] = Nil
        for (e <- l) reverted ::= e
        reverted
    }

    // P06
    def isPalindrome[T](l:List[T]) = l.reverse == l

    // P07
    def flatten[Any](l:List[_]):List[Any] =  l.reverse.foldLeft(List.empty[Any]) {
        case ( r, t:List[_] ) => flatten(t):::r
        case ( r, t:Any ) => t::r
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

    def mult[T](m:Int, e:T):List[T] = m match {
        case 0 => Nil
        case x => e::mult(x-1, e)
    }

    // P13
    def encodeDirect[T](l:List[T]):List[(Int, T)] = l.foldLeft(List.empty[List[T]]) {
        case (Nil, t) => List(t)::Nil
        case (head::tail, t) if head.head == t => (t::head)::tail
        case (head::tail, t) => List(t)::head::tail
    }.reverse.map(e => (e.size, e.head))

    // P14
    def duplicate[T](l:List[T]):List[T] = l.flatMap(e => List(e, e))

    // P15
    def duplicateN[T](m:Int, l:List[T]):List[T] = l.flatMap(e => mult(m, e))
}