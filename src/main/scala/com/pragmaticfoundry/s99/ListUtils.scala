package com.pragmaticfoundry.s99

/**
 * @author David Galichet.
 */

object ListUtils {

    def last[T](l:List[T]):T = l match {
        case Nil => throw new NoSuchElementException("List is empty")
        case head::Nil => head
        case head::tail => last(tail)
    }

    def penultimate[T](l:List[T]):T = l.reverse match {
        case first::second::tail => second
        case _ => l.head
    }

    def nth[T](k:Int, l:List[T]):T = if (k == 0) l.head else nth(k - 1, l.tail)

    def length[T](l:List[T]):Long = l match {
        case Nil => throw new NoSuchElementException("List is empty")
        case head::Nil => 1
        case head::tail => 1 + length(tail)
    }

    def reverse[T](l:List[T]):List[T] = {
        var reverted:List[T] = Nil
        for (e <- l) reverted ::= e
        reverted
    }

    def isPalindrome[T](l:List[T]) = l.reverse == l

    def flatten[Any](l:List[_]):List[Any] =  l.reverse.foldLeft(List.empty[Any]) {
        case ( r, t:List[_] ) => flatten(t):::r
        case ( r, t:Any ) => t::r
    }

    def compress[T](l:List[T]):List[T] = l match {
        case Nil => Nil
        case head::Nil => List(head)
        case head::tail => if (head != tail.head) head::compress(tail) else compress(tail)
    }

    def pack[T](l:List[T]):List[List[T]] = l match {
        case Nil => Nil
        case head::Nil => List(List(head))
        case head::tail if head == tail.head => List(head)::pack(tail)
        case head::tail => List(head)::pack(tail)
    }

    def packFirsts[T](l:List[T]):(List[T], List[T]) = ( (List.empty[T], List.empty[T]) /: l) {
        case ( ( Nil, _ ), e ) => ( e::Nil, Nil )
        case ( (h, Nil), e ) if h.head == e => ( e::h, Nil )
        case ( (h, t), e) => ( h, t :+ e )
    }

}