package fintech.homework07

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {

  def mergeSort[T](array: collection.mutable.IndexedSeq[T])(implicit smaller: (T,T)=>Boolean): Unit = {
    val temporaryArray =  array.clone()

    MergeSort(array, 0, array.size - 1, temporaryArray, smaller )
  }

  def Merge[T](array:collection.mutable.IndexedSeq[T], start: Int, middle: Int, end: Int, temporaryArray: collection.mutable.Seq[T], smaller : (T,T)=> Boolean): Unit = {
    var leftPtr = start
    var rightPtr = middle + 1
    val length = end - start + 1
    var j = 0
    while(j < length)
     {
      if (rightPtr > end || (leftPtr <= middle && smaller(array(leftPtr),array(rightPtr)))) {
        temporaryArray(j) = array(leftPtr)
        leftPtr += 1
      }
      else {
        temporaryArray(j) = array(rightPtr)
        rightPtr += 1
      }

       j += 1
    }
    var i = 0
    while (i < length) {
      array(i + start) = temporaryArray(i)
      i += 1
    }
  }

  def MergeSort[T](array: collection.mutable.IndexedSeq[T], start: Int, end: Int, temporaryArray: collection.mutable.Seq[T], smaller : (T,T)=> Boolean): Unit = {
    if (start == end) return
    val middle = (start + end) / 2
    MergeSort(array, start, middle, temporaryArray, smaller)
    MergeSort(array, middle + 1, end, temporaryArray, smaller)
    Merge(array, start, middle, end, temporaryArray, smaller)
  }




def quickSor[T](array: collection.mutable.IndexedSeq[T]) (implicit smaller: (T,T)=>Boolean): Unit = {

  HoareSort(array, 0, array.size - 1)

}

def HoareSort[T](array:  collection.mutable.IndexedSeq[T], start: Int, end: Int)(implicit smaller: (T,T)=>Boolean): Unit = {
  if (end == start) return
  val pivot = array(end)
  var storeIndex = start
  var i = start
  while (i <= end - 1) {
    if (smaller(array(i),pivot)) {
      val t = array(i)
      array(i)= array(storeIndex)
      array(storeIndex) = t
      storeIndex += 1

    }
    i+=1
  }
  val n = array(storeIndex)
  array(storeIndex) = array(end)
  array(end) = n
  if (storeIndex > start) HoareSort(array, start, storeIndex - 1)
  if (storeIndex < end) HoareSort(array, storeIndex + 1, end)
}


}
