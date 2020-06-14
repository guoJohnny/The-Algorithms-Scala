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
    
    "Median of Two Sorted Arrays" should "return a number passed to it" in {     
        val nums1 = Array(1,3)
        val nums2 = Array(2,4)
        assert(ArrayAlgorithms.findMedianSortedArrays(nums1, nums2) === 2.5)
    }

    "Container With Most Water" should "return a number passed to it" in {     
        val array = Array(1,8,6,2,5,4,8,3,7)
        assert(ArrayAlgorithms.maxArea(array) === 49)
    }

    "Three sum" should "passed to it" in {     
        val array = Array(-1,0,1,2,-1,-4)
        val array2 = Array(-1,2,1,-4)
        assert(ArrayAlgorithms.threeSum(array).mkString(",") === "List(-1, -1, 2),List(-1, 0, 1)")
        assert(ArrayAlgorithms.threeSumClosest(array2, 1) === 2)
    }

    "Four sum" should "passed to it" in {     
        val array = Array(1,0,-1,0,-2,2)
        assert(ArrayAlgorithms.fourSumHash(array, 0).mkString(",") === "List(-1, 0, 0, 1),List(-2, -1, 1, 2),List(-2, 0, 0, 2)")
    }

    "Remove Duplicates from Sorted Array" should "return a number and passed to it" in {     
        val array = Array(0,0,1,1,1,2,2,3,3,4)
        val array2 = Array(1,1,2)
        val number = ArrayAlgorithms.removeDuplicates(array)
        val number2 = ArrayAlgorithms.removeDuplicates(array2)
        assert(array.slice(0,number).mkString(",") === "0,1,2,3,4")
        assert(array2.slice(0,number2).mkString(",") === "1,2")
    }

    "Remove Element" should "return a number and passed to it" in {     
        val array = Array(3,2,2,3)
        val number = ArrayAlgorithms.removeElement(array, 3)
        assert(array.slice(0,number).mkString(",") === "2,2")
    }

    "Next Permutation" should " passed to it" in {     
        val array = Array(3,2,1)
        ArrayAlgorithms.nextPermutation(array)
        assert(array.mkString(",") === "1,2,3")
    }
}
