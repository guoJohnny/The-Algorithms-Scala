package ArrayAlgorithms

import scala.collection.mutable.ArrayBuffer
import scala.Array

object ArrayAlgorithms {
    
    /**
      * Remove all negative numbers in arraybuffer except the first negative number
      *
      * @param array    - Arraybuffer
      */
    def removeNegativeNumber(array: ArrayBuffer[Int]) : Unit = {
        var first = true
        val index = for(i <- 0 until array.length if first || array(i) > 0) yield {
            if (array(i) < 0) first = false ; i
        }
        for (i <- 0 until index.length) array(i) = array(index(i))  
        array.trimEnd(array.length - index.length) 
    }

    /**
      * Leetcode 3
      * There are two sorted arrays nums1 and nums2 of size m and n respectively.
      * Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).
      * You may assume nums1 and nums2 cannot be both empty.
      * @param nums1
      * @param nums2
      * @return
      */
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
        val array = (nums1 ++ nums2).sorted
        val mednum = array.length / 2
        if (array.length % 2 == 0) return (array(mednum) + array(mednum - 1)) / 2.0
        array(array.length/2)
    }

    /**
      * Leetcode 11 
      * Container With Most Water
      * @param height
      * @return
      */
     def maxArea(height: Array[Int]): Int = {
        var maxArea:Int = 0
        var (l, r) = (0, height.length -1)
        
        while(l < r) {
            val hl = height(l)
            val hr = height(r)
            maxArea = math.max(maxArea, math.min(hl, hr) * (r-l))
            if(hl < hr) {
                l += 1
            } else {
                r -= 1
            }
        }
        
        maxArea
    }
}
