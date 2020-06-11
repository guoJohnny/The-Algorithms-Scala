package ArrayAlgorithms

import scala.Array
import org.scalatest.FlatSpec
import scala.collection.mutable.ArrayBuffer

class ArrayAlgorithmsSpec extends FlatSpec{
    
    "ArrayBuffer remove negative number" should "return a removed arraybuffer passed to it" in {     
        val arraybuffer = Array(2, 3, 11, -1, -2, -3, 22, 21).to[ArrayBuffer]
        ArrayAlgorithms.removeNegativeNumber(arraybuffer)
        assert(arraybuffer.toArray === Array(2, 3, 11, -1, 22, 21))
    }
    
    "Median of Two Sorted Arrays" should "return a numbere passed to it" in {     
        val nums1 = Array(1,3)
        val nums2 = Array(2,4)
        assert(ArrayAlgorithms.findMedianSortedArrays(nums1, nums2) === 2.5)
    }
}
