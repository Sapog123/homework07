package fintech.homework07
import org.scalatest.{FlatSpec, Matchers}

class SortingSpec extends FlatSpec with Matchers {

  it should "work well quick sort" in {
    var array = collection.mutable.IndexedSeq(2,7,4,1,3,5,6,8)
    Sorting.quickSor(array)((a:Int,b: Int)=> a<=b)
    array should be (collection.mutable.IndexedSeq(1,2,3,4,5,6,7,8))
  }

  it should "work well merge sort" in {
    val array = collection.mutable.IndexedSeq(2,7,4,1,3,5,6,8)
    Sorting.mergeSort(array)((a:Int,b: Int)=> a<b)
    array should be (collection.mutable.IndexedSeq(1,2,3,4,5,6,7,8))
  }
}


